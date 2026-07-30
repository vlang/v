module c

import os
import runtime
import strings
import time
import v3.flat
import v3.gen.c.naming
import v3.types
import v3.workers

const max_flat_cgen_jobs = 10
const min_flat_cgen_parallel_items = 128
const scoped_cgen_worker_batches = 1
const flat_cgen_chunks_per_job = 12

$if !windows {
	// FlatCgenChunkArgs represents flat cgen chunk args data used by c.
	struct FlatCgenChunkArgs {
		worker         voidptr
		work_items_ptr voidptr
		is_master      bool
	}

	struct FlatCgenCostArgs {
		a         &flat.FlatAst
		items_ptr voidptr
		start     int
		end       int
		g         voidptr // &FlatGen, non-nil in fused prep mode (read-only access)
	mut:
		refs  map[string]bool
		cands []FlatCgenPrepCandidate
	}

	struct FlatCgenDynamicArgs {
		worker          voidptr
		work_chunks_ptr voidptr
		chunk_queue     chan int
		reserve_cost    i64
	}

	fn flat_cgen_cost_thread(arg voidptr) voidptr {
		mut a := unsafe { &FlatCgenCostArgs(arg) }
		mut items := unsafe { &[]FlatFnGenItem(a.items_ptr) }
		mut stack := []flat.NodeId{cap: 256}
		if !isnil(a.g) {
			// Fused prep mode: also collect the fn-ptr preseed candidates the
			// master replays in order after the join (see refine_fn_item_costs).
			g := unsafe { &FlatGen(a.g) }
			mut text_cache := &PrepTypTextCache{}
			mut type_seen := &PreseedTypeSeen{}
			mut cur_file := ''
			mut cur_module := ''
			for idx in a.start .. a.end {
				unsafe {
					item_file := items[idx].file
					item_module := items[idx].module
					if item_file != cur_file || item_module != cur_module {
						cur_file = item_file
						cur_module = item_module
						text_cache.generation++
					}
					cost, needs_prelude_scan := exact_flat_fn_gen_item_cost_and_prep(g,
						items[idx].node_id, idx, mut a.refs, mut stack, mut a.cands, mut
						text_cache, mut type_seen)
					items[idx].cost = cost
					items[idx].skip_prelude_scan = !needs_prelude_scan
				}
			}
			return unsafe { nil }
		}
		for idx in a.start .. a.end {
			unsafe {
				cost, needs_prelude_scan := exact_flat_fn_gen_item_cost(a.a, items[idx].node_id, mut
					a.refs, mut stack)
				items[idx].cost = cost
				items[idx].skip_prelude_scan = !needs_prelude_scan
			}
		}
		return unsafe { nil }
	}

	fn parallel_type_decls_thread(arg voidptr) voidptr {
		mut w := unsafe { &FlatGen(arg) }
		// This force-sync task uses the master generator without entering a
		// prealloc worker arena. Keep context caches disabled here; body workers
		// own arena-local caches and account for nearly all lookup traffic.
		w.import_alias_cache = unsafe { nil }
		w.enum_selector_cache = unsafe { nil }
		w.enum_method_cache = unsafe { nil }
		w.qualified_enum_method_cache = unsafe { nil }
		// Self-host declaration output is several MiB. Reserve it once instead of
		// repeatedly copying a geometrically growing builder.
		w.sb.ensure_cap(4 * 1024 * 1024)
		w.parallel_const_code = w.precompute_consts()
		w.gen_type_declaration_block()
		w.parallel_type_decls = w.sb.str()
		unsafe { w.sb.free() }
		w.sb = strings.new_builder(4096)
		w.forward_decls()
		w.parallel_forward_decls = w.sb.str()
		unsafe { w.sb.free() }
		w.sb = strings.new_builder(4096)
		w.parallel_support_ready = true
		return unsafe { nil }
	}

	// fixed_storage_scan_thread runs the fixed-storage-const use scan (a full
	// post-transform AST pass) on a private fork while the master collects the
	// fn work items and pre-seeds the parallel tables.
	fn fixed_storage_scan_thread(arg voidptr) voidptr {
		mut w := unsafe { &FlatGen(arg) }
		fssw := time.new_stopwatch()
		scope := cgen_worker_scope_begin(w.scope_parallel_workers)
		w.collect_fixed_storage_consts()
		w.precompute_param_type_index()
		w.precompute_concrete_optional_abi_fns()
		w.worker_scope = scope
		cgen_worker_scope_leave(scope)
		w.timing_profile('  [ttime]     cg fs scan     ${f64(fssw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		return unsafe { nil }
	}

	fn pre_dispatch_master_thread(arg voidptr) voidptr {
		mut g := unsafe { &FlatGen(arg) }
		g.prepare_pre_dispatch_master()
		return unsafe { nil }
	}

	// flat_cgen_chunk_thread supports flat cgen chunk thread handling for c.
	fn flat_cgen_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &FlatCgenChunkArgs(arg) }
		mut w := unsafe { &FlatGen(a.worker) }
		items := unsafe { &[]FlatFnGenItem(a.work_items_ptr) }
		if w.scope_parallel_workers {
			if a.is_master {
				w.gen_fn_items_scoped_master_batches(*items)
			} else {
				w.gen_fn_items_scoped_batches(*items)
			}
		} else {
			w.gen_fn_items(*items)
		}
		return unsafe { nil }
	}

	fn flat_cgen_dynamic_thread(arg voidptr) voidptr {
		a := unsafe { &FlatCgenDynamicArgs(arg) }
		mut w := unsafe { &FlatGen(a.worker) }
		chunks := unsafe { &[][]FlatFnGenItem(a.work_chunks_ptr) }
		w.gen_fn_chunks_scoped_dynamic(*chunks, a.chunk_queue, a.reserve_cost)
		return unsafe { nil }
	}
}

// finish_pending_item_prep_serial is the fallback for the work item selection
// defers to the parallel exact-cost pass (C-extern refs, and in parallel-prep
// mode also costs and fn-ptr preseeds): when that pass cannot run, do the
// deferred work serially like the former fused prep walk did.
fn (mut g FlatGen) finish_pending_item_prep_serial() {
	if !g.prep_externs_pending && !g.prep_costs_pending {
		return
	}
	mut stack := []flat.NodeId{cap: 256}
	if g.prep_costs_pending {
		g.prep_costs_pending = false
		// The selection-scope-allocated caches are gone by now; walk with
		// fresh ones.
		g.prep_typ_text_cache = &PrepTypTextCache{}
		g.preseed_type_seen = &PreseedTypeSeen{}
		mut type_text_cache := map[string]bool{}
		for i in 0 .. g.fn_gen_items.len {
			item := g.fn_gen_items[i]
			if item.file != g.tc.cur_file || item.module != g.tc.cur_module {
				type_text_cache.clear()
				if !isnil(g.prep_typ_text_cache) {
					g.prep_typ_text_cache.generation++
				}
			}
			g.tc.cur_file = item.file
			g.tc.cur_module = item.module
			g.fn_gen_items[i].cost = g.fn_item_cost_and_prep(item.node_id, mut stack, mut
				type_text_cache)
		}
	}
	if g.prep_externs_pending {
		g.prep_externs_pending = false
		items := g.fn_gen_items
		for item in items {
			_ = g.fn_item_cost_and_c_extern_prep(item.node_id, mut stack)
		}
	}
}

fn (mut g FlatGen) refine_fn_item_costs(no_parallel bool, reserve_worker bool) {
	if no_parallel || g.fn_gen_items.len < min_flat_cgen_parallel_items {
		g.finish_pending_item_prep_serial()
		return
	}
	$if windows {
		g.finish_pending_item_prep_serial()
		return
	} $else {
		if isnil(g.a.worker_pool) || g.a.worker_pool.size() == 0 {
			g.finish_pending_item_prep_serial()
			return
		}
		available_jobs := g.a.worker_pool.size() + 1 - if reserve_worker { 1 } else { 0 }
		n_jobs := flat_cgen_job_count(available_jobs, g.fn_gen_items.len)
		fused := g.prep_costs_pending
		mut prep_g := unsafe { nil }
		if fused {
			prep_g = voidptr(g)
		}
		mut args := []FlatCgenCostArgs{cap: n_jobs}
		mut tasks := []workers.Task{cap: n_jobs}
		for job in 0 .. n_jobs {
			start := g.fn_gen_items.len * job / n_jobs
			end := g.fn_gen_items.len * (job + 1) / n_jobs
			args << FlatCgenCostArgs{
				a:         unsafe { g.a }
				items_ptr: unsafe { voidptr(&g.fn_gen_items) }
				start:     start
				end:       end
				g:         prep_g
			}
		}
		for job in 0 .. n_jobs {
			tasks << workers.Task{
				run:        flat_cgen_cost_thread
				arg:        unsafe { voidptr(&args[job]) }
				force_sync: job == 0
			}
		}
		rfsw := time.new_stopwatch()
		g.a.worker_pool.run(tasks)
		g.timing_profile('  [ttime]     cg refine pool ${f64(rfsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		for arg in args {
			for name, used in arg.refs {
				if used {
					g.c_extern_refs[name] = true
				}
			}
		}
		if fused {
			rpsw := time.new_stopwatch()
			mut n_cands := 0
			for arg in args {
				n_cands += arg.cands.len
			}
			g.replay_prep_candidates(args)
			g.timing_profile('  [ttime]     cg replay      ${f64(rpsw.elapsed().microseconds()) / 1000.0:7.2f} ms (cands: ${n_cands})')
			g.prep_costs_pending = false
		}
		g.prep_externs_pending = false
	}
}

// replay_prep_candidates applies the fn-ptr preseeds collected by the parallel
// prep workers, in source order, so registrations land exactly as the former
// serial walk produced them.
fn (mut g FlatGen) replay_prep_candidates(args []FlatCgenCostArgs) {
	mut type_text_cache := map[string]bool{}
	// Fresh local dedup cache: g.preseed_type_seen was allocated inside the
	// (already freed) selection scope and must not be touched here.
	mut replay_seen := &PreseedTypeSeen{}
	for arg in args {
		for cand in arg.cands {
			item := g.fn_gen_items[cand.item_idx]
			if item.file != g.tc.cur_file || item.module != g.tc.cur_module {
				type_text_cache.clear()
				g.tc.cur_file = item.file
				g.tc.cur_module = item.module
			}
			if cand.is_expr {
				w0, w1, slot := preseed_type_words(cand.typ)
				if !replay_seen.seen[slot] || replay_seen.w0[slot] != w0
					|| replay_seen.w1[slot] != w1 {
					replay_seen.w0[slot] = w0
					replay_seen.w1[slot] = w1
					replay_seen.seen[slot] = true
					g.preseed_parallel_fn_ptr_type(cand.typ)
				}
			} else {
				if g.should_preseed_parallel_type_text_cached(cand.text, mut type_text_cache) {
					g.preseed_parallel_fn_ptr_type(g.tc.parse_type(cand.text))
				}
			}
		}
	}
}

@[direct_array_access]
fn (mut g FlatGen) preintern_ast_string_literals() {
	if g.ast_string_literals_ready {
		for value in g.ast_string_literals {
			g.intern_string(value)
		}
		return
	}
	for i in 0 .. g.a.nodes.len {
		node := unsafe { &g.a.nodes[i] }
		if node.kind == .string_literal {
			g.intern_string(node.value)
		}
	}
}

fn (mut g FlatGen) prepare_pre_dispatch_master() {
	mut n_items := 0
	if g.scope_parallel_workers {
		mut pmsw := time.new_stopwatch()
		selection_scope := cgen_worker_scope_begin(true)
		master_tc := g.tc
		g.tc = g.clone_parallel_type_checker()
		g.timing_profile('  [ttime]       pm clone tc  ${f64(pmsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		pmsw.restart()
		// Fuse body-local function-pointer discovery into the item cost walk so
		// parallel type declarations see every typedef before their task starts.
		// The globally numbered string table must also be complete before output.
		g.preintern_ast_string_literals()
		g.timing_profile('  [ttime]       pm str walk  ${f64(pmsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		pmsw.restart()
		g.want_parallel_prep = true
		items := g.ensure_fn_gen_items()
		g.want_parallel_prep = false
		g.timing_profile('  [ttime]       pm items     ${f64(pmsw.elapsed().microseconds()) / 1000.0:7.2f} ms (n: ${items.len})')
		pmsw.restart()
		if _ := g.ierror_interface_name() {
			g.intern_string('')
		}
		g.register_interface_strings()
		g.tc = master_tc
		cgen_worker_scope_leave(selection_scope)
		items_scope := cgen_worker_scope_begin(true)
		mut owned_items := []FlatFnGenItem{cap: items.len}
		for item in items {
			owned_items << FlatFnGenItem{
				node_id:                   item.node_id
				file:                      item.file
				module:                    item.module
				c_name:                    item.c_name.clone()
				cost:                      item.cost
				is_program_specialization: item.is_program_specialization
				direct_array_access:       item.direct_array_access
			}
		}
		g.fn_gen_items = owned_items
		g.emitted_fns = clone_cgen_string_bool_map(g.emitted_fns)
		cgen_worker_scope_leave(items_scope)
		g.scoped_fn_items_scope = items_scope
		// These tables remain live after release_scoped_fn_items, so promote them
		// into the enclosing cgen arena rather than the retained item arena.
		g.str_lits = clone_cgen_string_list(g.str_lits)
		g.str_lit_ids = clone_cgen_string_int_map(g.str_lit_ids)
		g.fn_ptr_types = clone_cgen_string_map(g.fn_ptr_types)
		g.used_fn_ptr_types = clone_cgen_string_bool_map(g.used_fn_ptr_types)
		g.c_extern_refs = clone_cgen_string_bool_map(g.c_extern_refs)
		g.c_name_cache = clone_c_name_cache(g.c_name_cache)
		g.generic_app_cache = clone_generic_app_cache(g.generic_app_cache)
		cgen_worker_scope_free(selection_scope)
		n_items = g.fn_gen_items.len
		g.timing_profile('  [ttime]       pm clone out ${f64(pmsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	} else {
		g.want_parallel_prep = true
		n_items = g.ensure_fn_gen_items().len
		g.want_parallel_prep = false
	}
	if n_items >= min_flat_cgen_parallel_items {
		// The fused item walk already interned and pre-seeded; only the
		// epilogue remains.
		if !g.scope_parallel_workers {
			if _ := g.ierror_interface_name() {
				g.intern_string('')
			}
			g.register_interface_strings()
		}
		if g.test_files.len == 0 && !g.has_entry_main() {
			for stmt in g.top_level_stmts() {
				g.collect_c_extern_referenced_symbols_from_node(stmt.id, mut g.c_extern_refs)
			}
		}
		g.parallel_prepared = true
	}
	// Force the lazily-built const short-name index now: workers share it
	// read-only, so it must be complete before any fork starts.
	_ = g.unique_const_ref_name('__v3_prewarm__') or { '' }
}

fn clone_cgen_string_map(values map[string]string) map[string]string {
	mut cloned := map[string]string{}
	for key, value in values {
		cloned[key.clone()] = value.clone()
	}
	return cloned
}

fn clone_cgen_string_bool_map(values map[string]bool) map[string]bool {
	mut cloned := map[string]bool{}
	for key, value in values {
		cloned[key.clone()] = value
	}
	return cloned
}

fn clone_cgen_string_int_map(values map[string]int) map[string]int {
	mut cloned := map[string]int{}
	for key, value in values {
		cloned[key.clone()] = value
	}
	return cloned
}

fn clone_c_name_cache(source &CNameCache) &CNameCache {
	mut entries := map[string]string{}
	if !isnil(source) {
		for key, value in source.entries {
			entries[key.clone()] = value.clone()
		}
	}
	return &CNameCache{
		entries: entries
	}
}

fn clone_generic_app_cache(source &GenericAppCache) &GenericAppCache {
	mut entries := map[string]GenericAppInfo{}
	if !isnil(source) {
		for key, value in source.entries {
			entries[key.clone()] = GenericAppInfo{
				base: value.base.clone()
				args: value.args.clone()
				ok:   value.ok
			}
		}
	}
	return &GenericAppCache{
		entries: entries
	}
}

// write_scoped_cgen_batch_output writes a batch builder while its disposable
// scope is still active, avoiding a second output copy in the parent arena.
fn (mut g FlatGen) write_scoped_cgen_batch_output(batch &FlatGen) bool {
	mut file := os.open_append(g.scoped_fn_output_path) or {
		g.output_error = err.msg()
		return false
	}
	if batch.cache_split {
		mut b := unsafe { batch }
		source := b.sb.str()
		stable_source := b.rewrite_cache_string_symbols(source)
		file.write_string(stable_source) or {
			g.output_error = err.msg()
			file.close()
			unsafe {
				source.free()
				stable_source.free()
			}
			return false
		}
		unsafe {
			source.free()
			stable_source.free()
		}
	} else {
		unsafe {
			file.write_full_buffer(batch.sb.data, usize(batch.sb.len)) or {
				g.output_error = err.msg()
				file.close()
				return false
			}
		}
	}
	file.close()
	return true
}

// absorb_scoped_cgen_batch copies a finished batch's observable side tables
// and, when needed, output into the helper's result arena.
fn (mut g FlatGen) absorb_scoped_cgen_batch(batch &FlatGen, output_streamed bool) {
	mut b := unsafe { batch }
	if !output_streamed {
		output := b.sb.str()
		if output.len > 0 {
			g.fn_segs << output
		} else {
			unsafe { output.free() }
		}
	}
	unsafe { b.sb.free() }
	// Preserve worker-only literals at the IDs already written into batch output.
	for literal in batch.str_lits[g.str_lits.len..] {
		g.intern_string(literal.clone())
	}
	for opt_name, val_type in batch.needed_optional_types {
		if opt_name !in g.needed_optional_types {
			g.needed_optional_types[opt_name.clone()] = val_type.clone()
		}
	}
	for encoded, name in batch.fn_ptr_types {
		if encoded !in g.fn_ptr_types {
			g.fn_ptr_types[encoded.clone()] = name.clone()
		}
	}
	for encoded, used in batch.used_fn_ptr_types {
		if used {
			g.used_fn_ptr_types[encoded.clone()] = true
		}
	}
	for name, used in batch.c_extern_refs {
		if used {
			g.c_extern_refs[name.clone()] = true
		}
	}
	for name, enabled in batch.libc_compat_fns {
		if enabled {
			g.libc_compat_fns[name.clone()] = true
		}
	}
	for key, name in batch.spawn_wrapper_names {
		if key !in g.spawn_wrapper_names {
			g.spawn_wrapper_names[key.clone()] = name.clone()
		}
	}
	for def in batch.spawn_wrapper_defs {
		if batch.cache_split {
			stable_def := b.rewrite_cache_string_symbols(def)
			g.add_spawn_wrapper_def(stable_def)
		} else {
			g.add_spawn_wrapper_def(def.clone())
		}
	}
	for key, name in batch.callback_wrapper_names {
		if key !in g.callback_wrapper_names {
			g.callback_wrapper_names[key.clone()] = name.clone()
		}
	}
	for def in batch.callback_wrapper_defs {
		if batch.cache_split {
			stable_def := b.rewrite_cache_string_symbols(def)
			g.add_callback_wrapper_def(stable_def)
		} else {
			g.add_callback_wrapper_def(def.clone())
		}
	}
	for wrappers in batch.parallel_chunk_wrapper_defs {
		g.parallel_chunk_wrapper_defs << ParallelChunkWrapperDefs{
			chunk_idx: wrappers.chunk_idx
			spawn:     clone_cgen_string_list(wrappers.spawn)
			callback:  clone_cgen_string_list(wrappers.callback)
		}
	}
}

// gen_fn_items_scoped_batches bounds helper scratch without adding worker-pool
// barriers. Each batch gets fresh mutable generator/checker caches while its C
// output is accumulated in a much smaller result arena.
fn (mut g FlatGen) gen_fn_items_scoped_batches(items []FlatFnGenItem) {
	result_scope := cgen_worker_scope_begin(true)
	mut total_cost := i64(items.len)
	for item in items {
		total_cost += item.cost
	}
	n_batches := if items.len < scoped_cgen_worker_batches {
		items.len
	} else {
		scoped_cgen_worker_batches
	}
	mut start := 0
	mut consumed_cost := i64(0)
	for batch_idx in 0 .. n_batches {
		mut end := start
		target_cost := total_cost * i64(batch_idx + 1) / i64(n_batches)
		for end < items.len
			&& (batch_idx == n_batches - 1 || consumed_cost < target_cost || end == start) {
			consumed_cost += i64(items[end].cost) + 1
			end++
		}
		scratch_scope := cgen_worker_scope_begin(true)
		mut batch := g.new_parallel_worker(batch_idx)
		// Weighted AST cost tracks generated body bytes closely enough to avoid
		// the 64 KiB builder growing and copying five or six times per worker.
		batch.sb = strings.new_builder(int(total_cost * 5) + 65_536)
		batch.gen_fn_items(items[start..end])
		cgen_worker_scope_leave(scratch_scope)
		g.absorb_scoped_cgen_batch(batch, false)
		cgen_worker_scope_free(scratch_scope)
		start = end
	}
	g.worker_scope = result_scope
	cgen_worker_scope_leave(result_scope)
}

fn (mut g FlatGen) gen_fn_chunks_scoped_dynamic(chunks [][]FlatFnGenItem, chunk_queue chan int, reserve_cost i64) {
	result_scope := cgen_worker_scope_begin(true)
	scratch_scope := cgen_worker_scope_begin(true)
	mut batch := g.new_parallel_worker(0)
	batch.sb = strings.new_builder(int(reserve_cost * 5) + 65_536)
	mut chunk_indexes := []int{cap: 16}
	mut output_starts := []int{cap: 16}
	mut output_lengths := []int{cap: 16}
	for {
		chunk_idx := <-chunk_queue or { break }
		batch.parallel_chunk_wrapper_defs << ParallelChunkWrapperDefs{
			chunk_idx: chunk_idx
		}
		batch.parallel_chunk_wrapper_capture = batch.parallel_chunk_wrapper_defs.len - 1
		output_start := batch.sb.len
		batch.gen_fn_items(chunks[chunk_idx])
		batch.parallel_chunk_wrapper_capture = -1
		chunk_indexes << chunk_idx
		output_starts << output_start
		output_lengths << batch.sb.len - output_start
	}
	cgen_worker_scope_leave(scratch_scope)
	for i, chunk_idx in chunk_indexes {
		output := batch.sb.spart(output_starts[i], output_lengths[i])
		if output.len > 0 {
			g.fn_segs << output
			g.fn_seg_chunk_indexes << chunk_idx
		} else {
			unsafe { output.free() }
		}
	}
	g.absorb_scoped_cgen_batch(batch, true)
	cgen_worker_scope_free(scratch_scope)
	g.worker_scope = result_scope
	cgen_worker_scope_leave(result_scope)
}

// gen_fn_items_scoped_master_batches publishes each caller-thread batch
// directly into the already-scoped master generator, so its temporary caches
// do not remain resident for the rest of cgen.
fn (mut g FlatGen) gen_fn_items_scoped_master_batches(items []FlatFnGenItem) {
	mut total_cost := i64(items.len)
	for item in items {
		total_cost += item.cost
	}
	n_batches := if items.len < scoped_cgen_worker_batches {
		items.len
	} else {
		scoped_cgen_worker_batches
	}
	mut start := 0
	mut consumed_cost := i64(0)
	for batch_idx in 0 .. n_batches {
		mut end := start
		target_cost := total_cost * i64(batch_idx + 1) / i64(n_batches)
		for end < items.len
			&& (batch_idx == n_batches - 1 || consumed_cost < target_cost || end == start) {
			consumed_cost += i64(items[end].cost) + 1
			end++
		}
		scratch_scope := cgen_worker_scope_begin(true)
		mut batch := g.new_parallel_worker(batch_idx)
		batch.gen_fn_items(items[start..end])
		cgen_worker_scope_leave(scratch_scope)
		g.absorb_scoped_cgen_batch(batch, false)
		cgen_worker_scope_free(scratch_scope)
		start = end
	}
}

fn clone_param_types_by_short(values map[string][]types.Type) map[string][]types.Type {
	mut cloned := map[string][]types.Type{}
	for name, params in values {
		cloned[name.clone()] = types.clone_owned_types(params)
	}
	return cloned
}

fn clone_embedded_fields_by_type(values map[string][]types.StructField) map[string][]types.StructField {
	mut cloned := map[string][]types.StructField{}
	for name, fields in values {
		mut owned_fields := []types.StructField{cap: fields.len}
		for field in fields {
			owned_fields << types.StructField{
				name:        field.name.clone()
				typ:         types.clone_owned_type(field.typ)
				has_default: field.has_default
				is_embed:    field.is_embed
				is_mut:      field.is_mut
			}
		}
		cloned[name.clone()] = owned_fields
	}
	return cloned
}

fn (mut g FlatGen) publish_fixed_storage_scan(mut fs_worker FlatGen) {
	if fs_worker.worker_scope == unsafe { nil } {
		g.fixed_storage_consts = fs_worker.fixed_storage_consts.clone()
		g.param_types_by_short = fs_worker.param_types_by_short.move()
		g.concrete_optional_abi_fns = fs_worker.concrete_optional_abi_fns.move()
		return
	}
	g.fixed_storage_consts = fs_worker.fixed_storage_consts.clone()
	g.param_types_by_short = clone_param_types_by_short(fs_worker.param_types_by_short)
	g.concrete_optional_abi_fns = fs_worker.concrete_optional_abi_fns.clone()
	cgen_worker_scope_free(fs_worker.worker_scope)
	fs_worker.worker_scope = unsafe { nil }
}

// gen_fns_dispatch emits fns dispatch output for c.
fn (mut g FlatGen) gen_fns_dispatch(no_parallel bool) {
	if no_parallel {
		if g.scope_parallel_workers {
			items := g.ensure_fn_gen_items()
			g.reset_context_lookup_caches()
			if items.len < min_flat_cgen_parallel_items {
				g.gen_fn_items(items)
			} else {
				g.gen_fn_items_scoped_master_batches(items)
			}
		} else {
			g.gen_fns()
		}
		g.gen_synthetic_main_after_fns()
		return
	}
	items := g.ensure_fn_gen_items()
	g.reset_context_lookup_caches()
	n_items := items.len
	$if windows {
		g.gen_fn_items(items)
		g.gen_synthetic_main_after_fns()
		return
	} $else {
		if isnil(g.a.worker_pool) {
			g.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		available_jobs := g.a.worker_pool.size() + 1
		// Type declarations use one pool task. Once it finishes, that same worker
		// can drain a queued body task instead of staying reserved for the whole
		// function-generation phase.
		parallel_type_decls := available_jobs > 2 && g.scope_parallel_workers
			&& !g.program_body_only && g.incremental_fn_names.len == 0
		n_jobs := flat_cgen_job_count(available_jobs, n_items)
		if n_items < min_flat_cgen_parallel_items || n_jobs <= 1 {
			if g.scope_parallel_workers {
				if n_items < min_flat_cgen_parallel_items {
					g.gen_fn_items(items)
				} else {
					g.gen_fn_items_scoped_master_batches(items)
				}
			} else {
				g.gen_fn_items(items)
			}
			g.gen_synthetic_main_after_fns()
			return
		}
		// Freeze the checker's warm type cache (fully populated by the check and
		// transform phases) as the shared read-only base for every worker's
		// fresh cache; the master's own memoization writes go to a private
		// overlay for the duration of the region.
		g.tc.freeze_type_cache_for_forks()
		g.freeze_parallel_lookup_caches()
		if !g.parallel_prepared {
			g.prepare_parallel_items(items)
		}
		chunk_jobs := if parallel_type_decls {
			n_jobs * flat_cgen_chunks_per_job
		} else {
			n_jobs
		}
		mut chunk_items := split_flat_cgen_items(items, chunk_jobs)
		chunk_count := chunk_items.len
		if parallel_type_decls {
			fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
			static_dispatch := fail.len > 0
			worker_count := if static_dispatch { chunk_count } else { n_jobs }
			mut cgen_workers := []voidptr{cap: worker_count}
			mut ordered_chunk_outputs := []string{}
			mut ordered_wrapper_defs := []ParallelChunkWrapperDefs{}
			worker_setup_scope := cgen_worker_scope_begin(true)
			for ci := 0; ci < worker_count; ci++ {
				mut w := g.new_parallel_dispatch_worker(ci)
				cgen_workers << voidptr(w)
			}
			cgen_worker_scope_leave(worker_setup_scope)
			if static_dispatch {
				mut args := []FlatCgenChunkArgs{cap: chunk_count}
				mut tasks := []workers.Task{cap: chunk_count + 1}
				for ci in 0 .. chunk_count {
					args << FlatCgenChunkArgs{
						worker:         cgen_workers[ci]
						work_items_ptr: unsafe { voidptr(&chunk_items[ci]) }
					}
					tasks << workers.Task{
						run:        flat_cgen_chunk_thread
						arg:        unsafe { voidptr(&args[ci]) }
						force_sync: fail == 'cgen:all' || fail == 'cgen:body:all'
							|| fail == 'cgen:body:${ci}'
					}
				}
				tasks << workers.Task{
					run:        parallel_type_decls_thread
					arg:        voidptr(g)
					force_sync: true
				}
				g.parallel_used = g.a.worker_pool.run(tasks)
			} else {
				// Long-lived workers pull small source-contiguous chunks from a shared
				// queue. This balances expression-cost and scheduler variation without
				// rebuilding the generator caches for every chunk.
				mut dsw := time.new_stopwatch()
				ordered_chunk_outputs = []string{len: chunk_count}
				ordered_wrapper_defs = []ParallelChunkWrapperDefs{len: chunk_count}
				chunk_queue := chan int{cap: chunk_count}
				for ci in 0 .. chunk_count {
					chunk_queue <- ci
				}
				chunk_queue.close()
				mut total_cost := i64(items.len)
				for item in items {
					total_cost += item.cost
				}
				reserve_cost := total_cost / i64(worker_count) + 1
				mut args := []FlatCgenDynamicArgs{cap: worker_count}
				mut tasks := []workers.Task{cap: worker_count + 1}
				tasks << workers.Task{
					run: parallel_type_decls_thread
					arg: voidptr(g)
				}
				for ci in 0 .. worker_count {
					args << FlatCgenDynamicArgs{
						worker:          cgen_workers[ci]
						work_chunks_ptr: unsafe { voidptr(&chunk_items) }
						chunk_queue:     chunk_queue
						reserve_cost:    reserve_cost
					}
					tasks << workers.Task{
						run:        flat_cgen_dynamic_thread
						arg:        unsafe { voidptr(&args[ci]) }
						force_sync: ci == 0
					}
				}
				g.parallel_used = g.a.worker_pool.run(tasks)
				g.timing_profile('  [ttime]   cg pool.run      ${f64(dsw.elapsed().microseconds()) / 1000.0:7.2f} ms (chunks: ${chunk_count}, workers: ${worker_count})')
			}
			// The declaration thread disables the master's caches while body
			// workers use their private copies. Restore them for synthetic output.
			mut msw := time.new_stopwatch()
			g.reset_context_lookup_caches()
			for worker_ptr in cgen_workers {
				mut w := unsafe { &FlatGen(worker_ptr) }
				if ordered_chunk_outputs.len > 0 {
					g.merge_parallel_worker_ordered(w, mut ordered_chunk_outputs, mut
						ordered_wrapper_defs)
				} else {
					g.merge_parallel_worker(w)
				}
				if w.worker_scope != unsafe { nil } {
					cgen_worker_scope_free(w.worker_scope)
					w.worker_scope = unsafe { nil }
				}
			}
			g.replay_ordered_parallel_wrapper_defs(ordered_wrapper_defs)
			g.timing_profile('  [ttime]   cg merge         ${f64(msw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			for output in ordered_chunk_outputs {
				if output.len > 0 {
					g.fn_segs << output
				}
			}
			cgen_worker_scope_free(worker_setup_scope)
			g.tc.unfreeze_type_cache_after_forks()
			g.gen_synthetic_main_after_fns()
			synthetic_output := g.sb.str()
			unsafe { g.sb.free() }
			g.sb = strings.new_builder(0)
			if synthetic_output.len > 0 {
				g.fn_segs << synthetic_output
			} else {
				unsafe { synthetic_output.free() }
			}
			return
		}
		// chunk[0] is emitted by the master directly into its own builder; the
		// other chunks get helper threads. Function-local temporary names are reset
		// for each item, so their spelling does not depend on chunk assignment.
		thread_count := chunk_count - 1
		mut args := []FlatCgenChunkArgs{cap: chunk_count}
		args << FlatCgenChunkArgs{
			worker:         voidptr(g)
			work_items_ptr: unsafe { voidptr(&chunk_items[0]) }
			is_master:      true
		}
		// Keep helper output in ordered result segments until the join so generated
		// string IDs can be reconciled with literals emitted by the master chunk.
		mut cgen_workers := []voidptr{cap: thread_count}
		worker_setup_scope := cgen_worker_scope_begin(g.scope_parallel_workers)
		for ci := 0; ci < thread_count; ci++ {
			mut w := g.new_parallel_dispatch_worker(ci + 1)
			cgen_workers << voidptr(w)
		}
		for ci := 0; ci < thread_count; ci++ {
			args << FlatCgenChunkArgs{
				worker:         cgen_workers[ci]
				work_items_ptr: unsafe { voidptr(&chunk_items[ci + 1]) }
			}
		}
		cgen_worker_scope_leave(worker_setup_scope)
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: chunk_count}
		for ci in 0 .. chunk_count {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        flat_cgen_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'cgen:all' || fail == 'cgen:body:all'
					|| fail == 'cgen:body:${helper_idx}'
			}
		}
		g.parallel_used = g.a.worker_pool.run(tasks)
		master_output := g.sb.str()
		unsafe { g.sb.free() }
		g.sb = strings.new_builder(4096)
		if master_output.len > 0 {
			g.fn_segs << master_output
		} else {
			unsafe { master_output.free() }
		}
		for ci := 0; ci < thread_count; ci++ {
			mut w := unsafe { &FlatGen(cgen_workers[ci]) }
			g.merge_parallel_worker(w)
			if w.worker_scope != unsafe { nil } {
				cgen_worker_scope_free(w.worker_scope)
				w.worker_scope = unsafe { nil }
			}
		}
		cgen_worker_scope_free(worker_setup_scope)
		g.tc.unfreeze_type_cache_after_forks()
		// Synthetic main temps continue after the master's chunk[0] range.
		g.gen_synthetic_main_after_fns()
		synthetic_output := g.sb.str()
		unsafe { g.sb.free() }
		g.sb = strings.new_builder(0)
		if synthetic_output.len > 0 {
			g.fn_segs << synthetic_output
		} else {
			unsafe { synthetic_output.free() }
		}
	}
}

// prepare_serial_fn_tables gives runtime `-no-parallel` generation the same
// deterministic preseed order as the parallel dispatcher, before constants
// can allocate string-literal IDs.
fn (mut g FlatGen) prepare_serial_fn_tables() {
	if g.parallel_prepared {
		return
	}
	g.want_parallel_prep = true
	items := g.ensure_fn_gen_items()
	g.want_parallel_prep = false
	if items.len >= min_flat_cgen_parallel_items {
		if _ := g.ierror_interface_name() {
			g.intern_string('')
		}
		g.register_interface_strings()
		g.parallel_prepared = true
	}
}

// freeze_parallel_lookup_caches keeps the warm pre-dispatch caches as immutable
// bases while the master and every body worker memoize into private overlays.
fn (mut g FlatGen) freeze_parallel_lookup_caches() {
	shared_c_name_cache := g.c_name_cache
	g.c_name_cache = &CNameCache{
		base: shared_c_name_cache
	}
	shared_generic_app_cache := g.generic_app_cache
	g.generic_app_cache = &GenericAppCache{
		base: shared_generic_app_cache
	}
}

// flat_cgen_job_count supports flat cgen job count handling for c.
fn flat_cgen_job_count(n_runtime_jobs int, n_items int) int {
	if n_runtime_jobs <= 0 || n_items <= 0 {
		return 0
	}
	mut n_jobs := n_runtime_jobs
	if n_jobs > max_flat_cgen_jobs {
		n_jobs = max_flat_cgen_jobs
	}
	if n_jobs > n_items {
		n_jobs = n_items
	}
	return n_jobs
}

// split_flat_cgen_items supports split flat cgen items handling for c.
fn split_flat_cgen_items(items []FlatFnGenItem, n_jobs int) [][]FlatFnGenItem {
	if n_jobs <= 0 || items.len == 0 {
		return [][]FlatFnGenItem{}
	}
	mut chunks := [][]FlatFnGenItem{}
	mut total_cost := 0
	for item in items {
		total_cost += item.cost
	}
	mut current := []FlatFnGenItem{}
	mut consumed_cost := 0
	mut chunk_idx := 0
	mut chunks_left := n_jobs
	for idx, item in items {
		remaining_items := items.len - idx
		next_target := total_cost * (chunk_idx + 1) / n_jobs
		if current.len > 0 && consumed_cost >= next_target && chunks_left > 1
			&& remaining_items >= chunks_left {
			chunks << current
			current = []FlatFnGenItem{}
			chunk_idx++
			chunks_left--
		}
		current << item
		consumed_cost += item.cost
	}
	if current.len > 0 {
		chunks << current
	}
	return chunks
}

// stripe_flat_cgen_items mixes several narrow, cost-balanced source ranges
// into each worker. Cgen work varies by expression kind as well as AST size;
// striping prevents one worker from inheriting an entire expensive name range.
fn stripe_flat_cgen_items(chunks [][]FlatFnGenItem, n_jobs int) [][]FlatFnGenItem {
	if n_jobs <= 0 || chunks.len == 0 {
		return [][]FlatFnGenItem{}
	}
	mut striped := [][]FlatFnGenItem{len: n_jobs}
	for idx, chunk in chunks {
		striped[idx % n_jobs] << chunk
	}
	return striped
}

// balance_flat_cgen_chunks assigns narrow, contiguous name ranges by cost,
// then restores source order within each worker. It keeps type/name caches hot
// while avoiding the modulo alignment sensitivity of simple striping.
fn balance_flat_cgen_chunks(chunks [][]FlatFnGenItem, n_jobs int) [][]FlatFnGenItem {
	if n_jobs <= 0 || chunks.len == 0 {
		return [][]FlatFnGenItem{}
	}
	mut chunk_costs := []i64{len: chunks.len}
	mut assigned := []bool{len: chunks.len}
	for idx, chunk in chunks {
		for item in chunk {
			chunk_costs[idx] += i64(item.cost) + 1
		}
	}
	mut worker_costs := []i64{len: n_jobs}
	mut worker_chunks := [][]int{len: n_jobs}
	for _ in chunks {
		mut largest := -1
		for idx, cost in chunk_costs {
			if !assigned[idx] && (largest < 0 || cost > chunk_costs[largest]) {
				largest = idx
			}
		}
		mut least_worker := 0
		for job in 1 .. n_jobs {
			if worker_costs[job] < worker_costs[least_worker] {
				least_worker = job
			}
		}
		assigned[largest] = true
		worker_chunks[least_worker] << largest
		worker_costs[least_worker] += chunk_costs[largest]
	}
	mut balanced := [][]FlatFnGenItem{len: n_jobs}
	for job, mut chunk_ids in worker_chunks {
		chunk_ids.sort(a < b)
		for chunk_id in chunk_ids {
			balanced[job] << chunks[chunk_id]
		}
	}
	return balanced
}

// fn_item_cost_and_prep computes the split cost, collects C-extern refs, and
// pre-seeds function-pointer types in one subtree traversal.
fn (mut g FlatGen) fn_item_cost_and_prep(node_id flat.NodeId, mut stack []flat.NodeId, mut type_text_cache map[string]bool) int {
	// Direct users of this helper (including small standalone generators) may
	// not have run collect_gen_info's fused literal collection.
	if !g.ast_string_literals_ready {
		g.preintern_ast_string_literals()
	}
	mut cost := 0
	stack.clear()
	stack << node_id
	for stack.len > 0 {
		current_id := stack.pop()
		idx := int(current_id)
		if idx < 0 || idx >= g.a.nodes.len {
			continue
		}
		node := unsafe { &g.a.nodes[idx] }
		cost++
		// String literals were already interned by the whole-AST literal walk in
		// prepare_pre_dispatch_master, and C-extern refs are re-collected by the
		// parallel exact-cost pass that always follows this prep (with a serial
		// fallback, see refine_fn_item_costs). Keep this walk preseed-only.
		if node.typ.len > 0
			&& g.should_preseed_parallel_type_text_ptr_cached(node.typ, mut type_text_cache) {
			g.preseed_parallel_fn_ptr_type(g.tc.parse_type(node.typ))
		}
		if expr_type := g.parallel_cached_expr_type(current_id, node) {
			// Checker-cached expression types repeat the same ~1K canonical
			// values across hundreds of thousands of nodes; traverse each
			// distinct value once instead of per node.
			if g.preseed_type_first_seen(expr_type) {
				g.preseed_parallel_fn_ptr_type(expr_type)
			}
		}
		for i := node.children_count - 1; i >= 0; i-- {
			child_id := g.a.children[node.children_start + i]
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
	return cost
}

fn (mut g FlatGen) fn_item_cost_and_c_extern_prep(node_id flat.NodeId, mut stack []flat.NodeId) int {
	mut cost := 0
	stack.clear()
	stack << node_id
	for stack.len > 0 {
		current_id := stack.pop()
		idx := int(current_id)
		if idx < 0 || idx >= g.a.nodes.len {
			continue
		}
		node := unsafe { &g.a.nodes[idx] }
		cost++
		if node.kind == .selector {
			g.collect_c_extern_ref_from_node(node)
		}
		for i := node.children_count - 1; i >= 0; i-- {
			child_id := g.a.children[node.children_start + i]
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
	return cost
}

// prepare_parallel_items supports prepare parallel items handling for FlatGen.
fn (mut g FlatGen) prepare_parallel_items(items []FlatFnGenItem) {
	mut stack := []flat.NodeId{cap: 256}
	// Function bodies can materialize literals from declaration metadata (for
	// example struct-field defaults) that lives outside every function subtree.
	// Intern all source literals before workers fork so their numeric IDs remain
	// valid regardless of which chunk first references that metadata.
	for node in g.a.nodes {
		if node.kind == .string_literal {
			g.intern_string(node.value)
		}
	}
	// The cache is keyed by the bare type text and reset whenever the
	// file/module context changes (items are grouped by file, so resets are
	// rare); the old composite '${file}\n${module}\n${typ}' key allocated a
	// string per visited node.
	mut type_text_cache := map[string]bool{}
	for item in items {
		if item.file != g.tc.cur_file || item.module != g.tc.cur_module {
			type_text_cache.clear()
		}
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		g.prepare_parallel_node(item.node_id, mut stack, mut type_text_cache)
	}
	if _ := g.ierror_interface_name() {
		g.intern_string('')
	}
	g.register_interface_strings()
}

// prepare_parallel_node supports prepare parallel node handling for FlatGen.
fn (mut g FlatGen) prepare_parallel_node(id flat.NodeId, mut stack []flat.NodeId, mut type_text_cache map[string]bool) {
	stack.clear()
	stack << id
	for stack.len > 0 {
		current_id := stack.pop()
		idx := int(current_id)
		if idx < 0 || idx >= g.a.nodes.len {
			continue
		}
		node := unsafe { &g.a.nodes[idx] }
		if node.kind == .string_literal {
			g.intern_string(node.value)
		}
		g.collect_c_extern_ref_from_node(node)
		if node.typ.len > 0
			&& g.should_preseed_parallel_type_text_cached(node.typ, mut type_text_cache) {
			g.preseed_parallel_fn_ptr_type(g.tc.parse_type(node.typ))
		}
		if expr_type := g.parallel_cached_expr_type(current_id, node) {
			g.preseed_parallel_fn_ptr_type(expr_type)
		}
		for i := node.children_count - 1; i >= 0; i-- {
			child_id := g.a.children[node.children_start + i]
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
}

fn (g &FlatGen) parallel_cached_expr_type(id flat.NodeId, node &flat.Node) ?types.Type {
	idx := int(id)
	if idx < 0 {
		return none
	}
	if g.tc.parallel_check_sparse && (idx < g.tc.check_range_lo || idx > g.tc.check_range_hi) {
		if t := g.tc.sparse_expr_type_values[idx] {
			return t
		}
		if node.kind == .call {
			if name := g.tc.sparse_resolved_call_names[idx] {
				if t := g.tc.fn_ret_types[name] {
					return t
				}
			}
		}
		return none
	}
	if idx < g.tc.expr_type_set.len && idx < g.tc.expr_type_values.len && g.tc.expr_type_set[idx] {
		return g.tc.expr_type_values[idx]
	}
	if node.kind == .call && idx < g.tc.resolved_call_set.len && idx < g.tc.resolved_call_names.len
		&& g.tc.resolved_call_set[idx] {
		name := g.tc.resolved_call_names[idx]
		if t := g.tc.fn_ret_types[name] {
			return t
		}
	}
	return none
}

// FlatCgenPrepCandidate is one deferred fn-ptr preseed action discovered by a
// parallel prep worker, replayed by the master in source order so the typedef
// registration order matches the former serial walk exactly.
struct FlatCgenPrepCandidate {
	is_expr  bool
	text     string     // node.typ text (is_expr == false)
	typ      types.Type // checker-cached expr type (is_expr == true)
	item_idx int
}

// exact_flat_fn_gen_item_cost_and_prep is exact_flat_fn_gen_item_cost plus the
// candidate collection of the former serial fused prep walk: distinct type
// texts and distinct cached expression types, in encounter order. All FlatGen
// and checker access is read-only (nothing writes the dense expr caches during
// cgen; every remember_expr_type caller is a mut check-phase path).
@[direct_array_access]
fn exact_flat_fn_gen_item_cost_and_prep(g &FlatGen, node_id flat.NodeId, item_idx int, mut c_extern_refs map[string]bool, mut stack []flat.NodeId, mut cands []FlatCgenPrepCandidate, mut text_cache PrepTypTextCache, mut type_seen PreseedTypeSeen) (int, bool) {
	a := g.a
	mut cost := 0
	mut needs_prelude_scan := false
	stack.clear()
	stack << node_id
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= a.nodes.len {
			continue
		}
		node := unsafe { &a.nodes[idx] }
		cost += flat_cgen_node_cost(node.kind)
		if node.kind == .lock_expr || node.kind == .label_stmt
			|| (node.kind == .defer_stmt && node.value == 'function') {
			needs_prelude_scan = true
		}
		if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
			base_id := a.children[node.children_start]
			if int(base_id) >= 0 {
				base := unsafe { &a.nodes[int(base_id)] }
				if base.kind == .ident && base.value == 'C' {
					raw_name := 'C.${node.value}'
					raw_cfn := naming.c_name(raw_name)
					c_extern_refs[raw_name] = true
					c_extern_refs[raw_cfn] = true
					c_extern_refs[c_winapi_wide_export_name(raw_cfn)] = true
				}
			}
		}
		if node.typ.len > 0 {
			slot := int((u64(voidptr(node.typ.str)) >> 4) & 4095)
			if text_cache.gens[slot] != text_cache.generation
				|| text_cache.ptrs[slot] != voidptr(node.typ.str)
				|| text_cache.lens[slot] != node.typ.len {
				text_cache.ptrs[slot] = voidptr(node.typ.str)
				text_cache.gens[slot] = text_cache.generation
				text_cache.lens[slot] = node.typ.len
				cands << FlatCgenPrepCandidate{
					text:     node.typ
					item_idx: item_idx
				}
			}
		}
		if expr_type := g.parallel_cached_expr_type(id, node) {
			w0, w1, slot := preseed_type_words(expr_type)
			if !type_seen.seen[slot] || type_seen.w0[slot] != w0 || type_seen.w1[slot] != w1 {
				type_seen.w0[slot] = w0
				type_seen.w1[slot] = w1
				type_seen.seen[slot] = true
				cands << FlatCgenPrepCandidate{
					is_expr:  true
					typ:      expr_type
					item_idx: item_idx
				}
			}
		}
		for i := node.children_count - 1; i >= 0; i-- {
			child_id := a.children[node.children_start + i]
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
	return cost, needs_prelude_scan
}

@[inline]
fn preseed_type_words(typ &types.Type) (u64, u64, int) {
	words := unsafe { &u64(voidptr(typ)) }
	w0 := unsafe { words[0] }
	w1 := unsafe { words[1] }
	return w0, w1, int(((w0 >> 4) ^ w1) & 4095)
}

// par_cgen_prep_enabled gates the parallel fused item prep, so a single binary
// can A/B or disable it (`V3_NO_PAR_CGEN_PREP=1`).
fn par_cgen_prep_enabled() bool {
	return os.getenv('V3_NO_PAR_CGEN_PREP') == ''
}

// PreseedTypeSeen dedups fn-ptr preseed traversals of checker-cached
// expression types during the fused prep walk. Keys are the opaque 16-byte
// Type value (variant box pointer + tag): identical bytes always denote the
// same type, so a hit safely skips the traversal; collisions just evict. The
// cache lives only for one item-selection walk, within the selection scope
// that owns any non-canonical type values it may reference.
struct PreseedTypeSeen {
mut:
	w0   [4096]u64
	w1   [4096]u64
	seen [4096]bool
}

// preseed_type_first_seen reports whether this exact type value has not been
// preseeded yet during the current prep walk, recording it as seen.
fn (mut g FlatGen) preseed_type_first_seen(typ &types.Type) bool {
	mut cache := g.preseed_type_seen
	if isnil(cache) {
		return true
	}
	words := unsafe { &u64(voidptr(typ)) }
	w0 := unsafe { words[0] }
	w1 := unsafe { words[1] }
	slot := int(((w0 >> 4) ^ w1) & 4095)
	if cache.seen[slot] && cache.w0[slot] == w0 && cache.w1[slot] == w1 {
		return false
	}
	cache.w0[slot] = w0
	cache.w1[slot] = w1
	cache.seen[slot] = true
	return true
}

// PrepTypTextCache short-circuits repeated preseed verdicts for the SAME type
// text instance during the fused item prep walk: node.typ strings are shared
// instances, so a pointer probe replaces most content-hash map lookups. The
// generation is bumped whenever the file/module context changes (verdicts
// depend on qualify_name), matching the map cache's clear.
struct PrepTypTextCache {
mut:
	generation u32 = 1
	ptrs       [4096]voidptr
	gens       [4096]u32
	lens       [4096]int
	verdicts   [4096]bool
}

fn (mut g FlatGen) should_preseed_parallel_type_text_ptr_cached(typ string, mut cache map[string]bool) bool {
	mut tcache := g.prep_typ_text_cache
	if isnil(tcache) {
		return g.should_preseed_parallel_type_text_cached(typ, mut cache)
	}
	slot := int((u64(voidptr(typ.str)) >> 4) & 4095)
	// Same pointer + same length + live generation means same text in the same
	// context (the length guards unsafe zero-copy slices sharing a base).
	if tcache.gens[slot] == tcache.generation && tcache.ptrs[slot] == voidptr(typ.str)
		&& tcache.lens[slot] == typ.len {
		return tcache.verdicts[slot]
	}
	verdict := g.should_preseed_parallel_type_text_cached(typ, mut cache)
	tcache.ptrs[slot] = voidptr(typ.str)
	tcache.gens[slot] = tcache.generation
	tcache.lens[slot] = typ.len
	tcache.verdicts[slot] = verdict
	return verdict
}

fn (g &FlatGen) should_preseed_parallel_type_text_cached(typ string, mut cache map[string]bool) bool {
	if typ.len == 0 {
		return false
	}
	if cached := cache[typ] {
		return cached
	}
	should_preseed := g.should_preseed_parallel_type_text(typ)
	cache[typ] = should_preseed
	return should_preseed
}

// should_preseed_parallel_type_text reports whether should preseed parallel type text applies in c.
fn (g &FlatGen) should_preseed_parallel_type_text(typ string) bool {
	if typ.len == 0 {
		return false
	}
	clean := g.parallel_base_type_text(typ)
	if clean.contains('fn(') || clean.contains('fn (') {
		return true
	}
	if clean in g.tc.type_aliases {
		return true
	}
	qtyp := g.tc.qualify_name(clean)
	return qtyp in g.tc.type_aliases
}

// parallel_base_type_text supports parallel base type text handling for FlatGen.
fn (g &FlatGen) parallel_base_type_text(typ string) string {
	mut clean := trimmed_space(typ)
	for clean.len > 0 {
		if clean.starts_with('shared ') {
			clean = trimmed_space(clean[7..])
		} else if clean[0] == `&` || clean[0] == `?` || clean[0] == `!` {
			clean = trimmed_space(clean[1..])
		} else if clean.starts_with('...') {
			clean = trimmed_space(clean[3..])
		} else if clean.starts_with('[]') {
			clean = trimmed_space(clean[2..])
		} else {
			break
		}
	}
	return clean
}

// preseed_parallel_fn_ptr_type supports preseed parallel fn ptr type handling for FlatGen.
fn (mut g FlatGen) preseed_parallel_fn_ptr_type(typ types.Type) {
	if typ is types.FnType {
		g.register_fn_ptr_type(g.fn_ptr_type_key(typ))
		for param in typ.params {
			g.preseed_parallel_fn_ptr_type(param)
		}
		g.preseed_parallel_fn_ptr_type(typ.return_type)
	} else if typ is types.Pointer {
		g.preseed_parallel_fn_ptr_type(typ.base_type)
	} else if typ is types.Array {
		g.preseed_parallel_fn_ptr_type(typ.elem_type)
	} else if typ is types.ArrayFixed {
		g.preseed_parallel_fn_ptr_type(typ.elem_type)
	} else if typ is types.Map {
		g.preseed_parallel_fn_ptr_type(typ.key_type)
		g.preseed_parallel_fn_ptr_type(typ.value_type)
	} else if typ is types.OptionType {
		g.preseed_parallel_fn_ptr_type(typ.base_type)
	} else if typ is types.ResultType {
		g.preseed_parallel_fn_ptr_type(typ.base_type)
	} else if typ is types.Alias {
		g.preseed_parallel_fn_ptr_type(typ.base_type)
	} else if typ is types.MultiReturn {
		for item in typ.types {
			g.preseed_parallel_fn_ptr_type(item)
		}
	}
}

// new_parallel_worker builds a per-worker FlatGen for parallel codegen.
//
// The lookup tables populated before gen_fns_dispatch (in collect_gen_info,
// collect_interface_impls and the precompute_* passes) are READ-ONLY during codegen, so
// they are SHARED by reference instead of cloned — V maps/arrays are reference types and
// concurrent readers are safe. Only the state a worker actually mutates while emitting is
// kept private: the output builder; the string-literal table (interned during gen); the
// fn_ptr_types / needed_optional_types / emitted_* sets and the param_types_cache /
// array_method_cache memoization caches (all written during gen); the per-function
// cur_param_* scratch; and runtime_inits (kept private out of caution). This drops the
// bulk of each worker's clone cost — previously the whole table set was duplicated per
// worker and, under -gc none, never freed.
fn (g &FlatGen) new_parallel_worker(worker_id int) &FlatGen {
	return g.new_parallel_worker_config(worker_id, false)
}

// new_parallel_dispatch_worker selects the lightweight accumulator only when
// scoped batching keeps all actual emission in fresh full workers.
fn (g &FlatGen) new_parallel_dispatch_worker(worker_id int) &FlatGen {
	if g.scope_parallel_workers {
		return g.new_parallel_result_worker(worker_id)
	}
	return g.new_parallel_worker(worker_id)
}

// new_parallel_result_worker creates a non-emitting helper accumulator. Caches
// that it only passes to fresh batch generators stay shared with the frozen
// master snapshot; result tables remain private. Its string tables are copied
// eagerly because the master can extend its own table after tasks start, before
// a helper's first copy-on-write intern.
fn (g &FlatGen) new_parallel_result_worker(worker_id int) &FlatGen {
	return g.new_parallel_worker_config(worker_id, true)
}

fn (g &FlatGen) new_parallel_worker_config(worker_id int, result_only bool) &FlatGen {
	return &FlatGen{
		sb:                             strings.new_builder(if result_only { 0 } else { 64_000 })
		a:                              unsafe { g.a }
		used_fns:                       g.used_fns
		used_fn_names:                  g.used_fn_names
		fn_gen_items:                   g.fn_gen_items
		top_level_node_ids:             g.top_level_node_ids
		test_files:                     if result_only { g.test_files } else { g.test_files.clone() }
		cache_program_files:            g.cache_program_files
		incremental_fn_names:           g.incremental_fn_names
		cached_support_identifiers:     g.cached_support_identifiers
		str_lits:                       if result_only {
			clone_cgen_string_list(g.str_lits)
		} else if g.scope_parallel_workers {
			g.str_lits
		} else {
			g.str_lits.clone()
		}
		str_lit_ids:                    if result_only {
			clone_cgen_string_int_map(g.str_lit_ids)
		} else if g.scope_parallel_workers {
			g.str_lit_ids
		} else {
			g.str_lit_ids.clone()
		}
		str_lits_shared:                g.scope_parallel_workers && !result_only
		global_types:                   g.global_types
		global_raw_type_texts:          g.global_raw_type_texts
		enum_vals:                      g.enum_vals
		enum_value_exprs:               g.enum_value_exprs
		interfaces:                     g.interfaces
		const_vals:                     g.const_vals
		const_modules:                  g.const_modules
		const_init_order:               g.const_init_order
		fixed_storage_consts:           g.fixed_storage_consts
		global_modules:                 g.global_modules
		global_inits:                   g.global_inits
		global_init_order:              g.global_init_order
		enum_backing_infos:             g.enum_backing_infos
		iface_impls:                    g.iface_impls
		interface_dispatch_required:    g.interface_dispatch_required
		iface_type_ids:                 g.iface_type_ids
		interface_boxed_types:          g.interface_boxed_types
		interface_boxed_types_done:     g.interface_boxed_types_done
		ierror_method_emit_names:       g.ierror_method_emit_names
		ierror_stack_pointer_aliases:   []map[string]bool{}
		ierror_owned_pointer_by_owner:  map[string]bool{}
		recursive_drop_helpers:         g.recursive_drop_helpers
		local_pointer_storage_by_owner: map[string]bool{}
		local_c_type_by_owner:          map[string]string{}
		local_raw_type_by_owner:        map[string]string{}
		local_shared_storage_by_owner:  map[string]bool{}
		local_fn_value_c_name_by_owner: map[string]string{}
		sum_name_lookup:                g.sum_name_lookup
		module_init_fns:                g.module_init_fns
		module_init_fn_modules:         g.module_init_fn_modules
		module_imports:                 g.module_imports
		preserved_header_files_seen:    g.preserved_header_files_seen
		libc_compat_fns:                g.libc_compat_fns.clone()
		tc:                             if result_only {
			unsafe { g.tc }
		} else {
			g.clone_parallel_type_checker()
		}
		has_builtins:                   g.has_builtins
		cache_split:                    g.cache_split
		skip_generics:                  g.skip_generics
		tmp_count:                      (worker_id + 1) * 100_000
		line_start:                     true
		modules:                        g.modules
		fn_ptr_types:                   g.fn_ptr_types.clone()
		used_fn_ptr_types:              if g.scope_parallel_workers {
			map[string]bool{}
		} else {
			g.used_fn_ptr_types.clone()
		}
		fixed_array_ret_wrappers:       g.fixed_array_ret_wrappers
		concrete_optional_abi_fns:      g.concrete_optional_abi_fns
		fn_decl_param_types:            g.fn_decl_param_types
		fn_decl_variadic:               g.fn_decl_variadic
		fn_decl_variadic_short_counts:  g.fn_decl_variadic_short_counts
		fn_decl_shared_params:          g.fn_decl_shared_params
		fn_shared_params_resolved:      g.fn_shared_params_resolved
		has_shared_params:              g.has_shared_params
		fn_decl_mut_receivers:          g.fn_decl_mut_receivers
		fn_decl_ret_types:              g.fn_decl_ret_types
		non_generic_fn_names_by_module: g.non_generic_fn_names_by_module
		generic_fn_keys_by_short:       g.generic_fn_keys_by_short
		generic_fn_keys_by_cname:       g.generic_fn_keys_by_cname
		generic_fn_key_ordinal:         g.generic_fn_key_ordinal
		struct_decl_infos:              g.struct_decl_infos
		struct_decl_short_infos:        g.struct_decl_short_infos
		shared_type_names:              g.shared_type_names
		shared_alias_pointer_shorts:    g.shared_alias_pointer_shorts
		default_value_stack:            map[string]bool{}
		const_runtime_inits:            if result_only {
			g.const_runtime_inits
		} else {
			g.const_runtime_inits.clone()
		}
		runtime_inits:                  if result_only {
			g.runtime_inits
		} else {
			g.runtime_inits.clone()
		}
		compiler_vroot:                 g.compiler_vroot
		compiler_vexe:                  g.compiler_vexe
		compiler_vexe_env_setup:        g.compiler_vexe_env_setup
		cur_param_names:                if result_only {
			g.cur_param_names
		} else {
			g.cur_param_names.clone()
		}
		cur_param_type_values:          if result_only {
			g.cur_param_type_values
		} else {
			g.cur_param_type_values.clone()
		}
		cur_param_types:                if result_only {
			g.cur_param_types
		} else {
			g.cur_param_types.clone()
		}
		cur_concrete_optional_params:   if result_only {
			g.cur_concrete_optional_params
		} else {
			g.cur_concrete_optional_params.clone()
		}
		cur_mut_params:                 if result_only {
			g.cur_mut_params
		} else {
			g.cur_mut_params.clone()
		}
		cur_mut_param_owners:           if result_only {
			g.cur_mut_param_owners
		} else {
			g.cur_mut_param_owners.clone()
		}
		cur_fn_ret:                     g.cur_fn_ret
		cur_fn_ret_is_optional:         g.cur_fn_ret_is_optional
		cur_fn_ret_base:                g.cur_fn_ret_base
		loop_label_depths:              map[string]int{}
		expected_expr_type:             g.expected_expr_type
		expected_enum:                  g.expected_enum
		needed_optional_types:          g.needed_optional_types.clone()
		emitted_optional_types:         if result_only {
			g.emitted_optional_types
		} else {
			g.emitted_optional_types.clone()
		}
		// Function selection is complete before workers are created; body
		// generation only reads this set.
		emitted_fns:                     g.emitted_fns
		array_method_cache:              if result_only {
			g.array_method_cache
		} else {
			g.array_method_cache.clone()
		}
		param_types_cache:               if result_only {
			g.param_types_cache
		} else {
			g.param_types_cache.clone()
		}
		interface_receiver_cache:        &StringLookupCache{}
		normalize_call_cache:            &StringLookupCache{}
		flattened_generic_name_cache:    &StringLookupCache{}
		generic_struct_context_ct_cache: &StringLookupCache{}
		struct_cname_cache:              &StringLookupCache{}
		unique_struct_ct_cache:          &StringLookupCache{}
		alias_method_cache:              &StringLookupCache{}
		import_alias_cache:              &ContextStringLookupCache{}
		enum_selector_cache:             &ContextStringLookupCache{}
		enum_method_cache:               &ContextStringLookupCache{}
		qualified_enum_method_cache:     &ContextStringLookupCache{}
		struct_decl_pref_cache:          &StructDeclPrefCache{}
		embedded_fields_by_type:         g.embedded_fields_by_type
		param_types_by_short:            g.param_types_by_short
		generic_method_candidates:       g.generic_method_candidates
		spawn_wrapper_names:             g.spawn_wrapper_names.clone()
		spawn_wrapper_defs:              g.spawn_wrapper_defs.clone()
		spawn_wrapper_defs_seen:         g.spawn_wrapper_defs_seen.clone()
		callback_wrapper_names:          g.callback_wrapper_names.clone()
		callback_wrapper_defs:           g.callback_wrapper_defs.clone()
		callback_wrapper_defs_seen:      g.callback_wrapper_defs_seen.clone()
		c_extern_refs:                   g.c_extern_refs.clone()
		c_extern_refs_ready:             g.c_extern_refs_ready
		scope_parallel_workers:          g.scope_parallel_workers
		c_name_cache:                    &CNameCache{
			base: if !isnil(g.c_name_cache.base) { g.c_name_cache.base } else { g.c_name_cache }
		}
		// The const short-name index is read-only after its first build (the
		// master queries it during the const precompute, before the forks);
		// sharing it avoids a rebuild per worker.
		const_short_index: g.const_short_index
		mut_recv_facts:    &FnNameFactCache{}
		generic_app_cache: &GenericAppCache{
			base: if !isnil(g.generic_app_cache.base) {
				g.generic_app_cache.base
			} else {
				g.generic_app_cache
			}
		}
	}
}

fn (g &FlatGen) clone_parallel_type_checker_legacy() &types.TypeChecker {
	// Cgen only reads file-level bindings. Give each worker an empty child scope
	// over the immutable checked scope instead of cloning the full symbol table.
	fs := types.new_scope(g.tc.file_scope)
	mut wtc := &types.TypeChecker{
		a:                                     unsafe { g.tc.a }
		fn_ret_types:                          g.tc.fn_ret_types
		fn_param_types:                        g.tc.fn_param_types
		c_fn_module_ret_types:                 g.tc.c_fn_module_ret_types
		c_fn_module_param_types:               g.tc.c_fn_module_param_types
		c_fn_module_variadic:                  g.tc.c_fn_module_variadic
		fn_ret_type_texts:                     g.tc.fn_ret_type_texts
		fn_param_type_texts:                   g.tc.fn_param_type_texts
		fn_type_files:                         g.tc.fn_type_files
		fn_type_modules:                       g.tc.fn_type_modules
		fn_generic_params:                     g.tc.fn_generic_params
		specialized_generic_fns:               g.tc.specialized_generic_fns
		fn_variadic:                           g.tc.fn_variadic
		fn_implicit_veb_ctx:                   g.tc.fn_implicit_veb_ctx
		c_variadic_fns:                        g.tc.c_variadic_fns
		structs:                               g.tc.structs
		struct_modules:                        g.tc.struct_modules
		struct_files:                          g.tc.struct_files
		soa_structs:                           g.tc.soa_structs
		struct_error_embeds_shadow_builtin:    g.tc.struct_error_embeds_shadow_builtin
		struct_generic_params:                 g.tc.struct_generic_params
		struct_field_c_abi_fns:                g.tc.struct_field_c_abi_fns
		unions:                                g.tc.unions
		type_aliases:                          g.tc.type_aliases
		type_alias_generic_params:             g.tc.type_alias_generic_params
		type_alias_c_abi_fns:                  g.tc.type_alias_c_abi_fns
		sum_types:                             g.tc.sum_types
		sum_generic_params:                    g.tc.sum_generic_params
		enum_names:                            g.tc.enum_names
		enum_fields:                           g.tc.enum_fields
		flag_enums:                            g.tc.flag_enums
		interface_names:                       g.tc.interface_names
		interface_generic_params:              g.tc.interface_generic_params
		interface_fields:                      g.tc.interface_fields
		interface_embeds:                      g.tc.interface_embeds
		interface_abstract_methods:            g.tc.interface_abstract_methods
		interface_impl_name_snapshots:         g.tc.interface_impl_name_snapshots
		interface_impl_candidates_at_snapshot: g.tc.interface_impl_candidates_at_snapshot
		c_globals:                             g.tc.c_globals
		const_types:                           g.tc.const_types
		const_exprs:                           g.tc.const_exprs
		const_modules:                         g.tc.const_modules
		const_files:                           g.tc.const_files
		const_suffixes:                        g.tc.const_suffixes
		imports:                               g.tc.imports
		file_imports:                          g.tc.file_imports
		file_selective_imports:                g.tc.file_selective_imports
		file_modules:                          g.tc.file_modules
		file_scope:                            g.tc.file_scope
		cur_scope:                             fs
		scope_pool:                            []&types.Scope{}
		has_builtins:                          g.tc.has_builtins
		resolution_type_mode:                  g.tc.resolution_type_mode
		cur_module:                            g.tc.cur_module
		cur_file:                              g.tc.cur_file
		errors:                                g.tc.errors.clone()
		resolved_call_names:                   g.tc.resolved_call_names
		resolved_call_set:                     g.tc.resolved_call_set
		resolved_fn_value_names:               g.tc.resolved_fn_value_names
		resolved_fn_value_set:                 g.tc.resolved_fn_value_set
		statement_nodes:                       g.tc.statement_nodes
		expr_type_values:                      g.tc.expr_type_values
		expr_type_set:                         g.tc.expr_type_set
		checking_nodes:                        g.tc.checking_nodes
		parallel_check_sparse:                 g.tc.parallel_check_sparse
		check_range_lo:                        g.tc.check_range_lo
		check_range_hi:                        g.tc.check_range_hi
		sparse_resolved_call_names:            g.tc.sparse_resolved_call_names
		sparse_resolved_fn_values:             g.tc.sparse_resolved_fn_values
		sparse_statement_nodes:                g.tc.sparse_statement_nodes
		sparse_expr_type_values:               g.tc.sparse_expr_type_values
		sparse_checking_nodes:                 g.tc.sparse_checking_nodes
		diagnose_unknown_calls:                g.tc.diagnose_unknown_calls
		reject_unlowered_map_mutation:         g.tc.reject_unlowered_map_mutation
		diagnostic_files:                      g.tc.diagnostic_files
		selected_file_called_fns:              g.tc.selected_file_called_fns
		smartcasts:                            g.tc.smartcasts
		// Read-only map cgen uses to recover substituted signatures for generic-receiver
		// method values (`Box[int].method` as a callback); without it a parallel worker
		// sees an empty map and gen_method_value_closure falls through.
		generic_method_value_info: g.tc.generic_method_value_info
		params_structs:            g.tc.params_structs
		c_typedef_structs:         g.tc.c_typedef_structs
	}
	wtc.inherit_ownership_codegen_metadata_from(g.tc)
	// A private empty TypeCache lets the worker use the lazily-built lookup
	// indexes (short type names, local fn decls) and the field/IError
	// memoizations instead of their uncached full-scan fallbacks. It shares no
	// state with other threads.
	wtc.set_fresh_type_cache_based_on(g.tc, g.tc.type_cache_parse_enabled())
	wtc.reset_resolution_type_view_cache()
	return wtc
}

fn (mut g FlatGen) publish_worker_string_literals(w &FlatGen) map[int]int {
	mut remap := map[int]int{}
	mut common_len := 0
	for common_len < g.str_lits.len && common_len < w.str_lits.len
		&& g.str_lits[common_len] == w.str_lits[common_len] {
		common_len++
	}
	for local_id in common_len .. w.str_lits.len {
		literal := w.str_lits[local_id]
		global_id := if existing_id := g.str_lit_ids[literal] {
			existing_id
		} else {
			g.intern_string(literal.clone())
		}
		if global_id != local_id {
			remap[local_id] = global_id
		}
	}
	return remap
}

fn remap_scoped_worker_string_symbols(source string, remap map[int]int, user_c_symbols map[string]bool) string {
	if remap.len == 0 {
		return source.clone()
	}
	mut out := strings.new_builder(source.len)
	mut i := 0
	for i < source.len {
		if source[i] in [`"`, `'`] {
			quote := source[i]
			start := i
			i++
			for i < source.len {
				if source[i] == `\\` && i + 1 < source.len {
					i += 2
					continue
				}
				i++
				if source[i - 1] == quote {
					break
				}
			}
			out.write_string(source[start..i])
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `/` {
			start := i
			i += 2
			for i < source.len && source[i] != `\n` {
				i++
			}
			out.write_string(source[start..i])
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `*` {
			start := i
			i += 2
			for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
				i++
			}
			if i + 1 < source.len {
				i += 2
			} else {
				i = source.len
			}
			out.write_string(source[start..i])
			continue
		}
		if c_identifier_start(source[i]) {
			start := i
			i++
			for i < source.len && c_identifier_continue(source[i]) {
				i++
			}
			identifier := source[start..i]
			if cache_numbered_string_symbol(identifier) && !user_c_symbols[identifier] {
				mut local_id := 0
				for digit in identifier[5..].bytes() {
					local_id = local_id * 10 + int(digit - `0`)
				}
				if global_id := remap[local_id] {
					out.write_string('_str_${global_id}')
					continue
				}
			}
			out.write_string(identifier)
			continue
		}
		out.write_u8(source[i])
		i++
	}
	return out.str()
}

// merge_parallel_worker supports merge parallel worker handling for FlatGen.
fn (mut g FlatGen) merge_parallel_worker(w &FlatGen) {
	mut unordered := []string{}
	mut unordered_wrapper_defs := []ParallelChunkWrapperDefs{}
	g.merge_parallel_worker_into(w, mut unordered, mut unordered_wrapper_defs)
}

fn (mut g FlatGen) merge_parallel_worker_ordered(w &FlatGen, mut ordered []string, mut ordered_wrapper_defs []ParallelChunkWrapperDefs) {
	g.merge_parallel_worker_into(w, mut ordered, mut ordered_wrapper_defs)
}

fn (mut g FlatGen) merge_parallel_worker_into(w &FlatGen, mut ordered []string, mut ordered_wrapper_defs []ParallelChunkWrapperDefs) {
	mut ww := unsafe { w }
	if g.output_error.len == 0 && w.output_error.len > 0 {
		g.output_error = w.output_error.clone()
	}
	string_id_remap := g.publish_worker_string_literals(w)
	user_c_symbols := if string_id_remap.len > 0 {
		g.cache_user_c_string_symbols()
	} else {
		map[string]bool{}
	}
	worker_output := ww.sb.str()
	if worker_output.len > 0 {
		if g.cache_split {
			stable_output := ww.rewrite_cache_string_symbols(worker_output)
			g.fn_segs << stable_output
			unsafe { worker_output.free() }
		} else if string_id_remap.len > 0 {
			g.fn_segs << remap_scoped_worker_string_symbols(worker_output, string_id_remap,
				user_c_symbols)
			unsafe { worker_output.free() }
		} else {
			g.fn_segs << worker_output
		}
	} else {
		unsafe { worker_output.free() }
	}
	// The ordered segment owns the copied output; release the worker builder.
	unsafe { ww.sb.free() }
	for segment_idx, segment in w.fn_segs {
		normalized := if g.cache_split {
			ww.rewrite_cache_string_symbols(segment)
		} else if string_id_remap.len > 0 {
			remap_scoped_worker_string_symbols(segment, string_id_remap, user_c_symbols)
		} else {
			segment.clone()
		}
		if ordered.len > 0 && segment_idx < w.fn_seg_chunk_indexes.len {
			chunk_idx := w.fn_seg_chunk_indexes[segment_idx]
			if chunk_idx >= 0 && chunk_idx < ordered.len {
				ordered[chunk_idx] = normalized
				continue
			}
		}
		g.fn_segs << normalized
	}
	if g.cache_split {
		for literal in w.str_lits {
			g.intern_string(literal.clone())
		}
	}
	for opt_name, val_type in w.needed_optional_types {
		g.needed_optional_types[opt_name.clone()] = val_type.clone()
	}
	for encoded, name in w.fn_ptr_types {
		if encoded !in g.fn_ptr_types {
			g.fn_ptr_types[encoded.clone()] = name.clone()
		}
	}
	for encoded, used in w.used_fn_ptr_types {
		if used {
			g.used_fn_ptr_types[encoded.clone()] = true
		}
	}
	for name, used in w.c_extern_refs {
		if used {
			g.c_extern_refs[name.clone()] = true
		}
	}
	for name, enabled in w.libc_compat_fns {
		if enabled {
			g.libc_compat_fns[name.clone()] = true
		}
	}
	// Spawn wrappers (thread arg structs + trampoline fns) are generated on demand
	// inside fn bodies, so a worker that emits a `spawn` produces wrapper defs the
	// master must also emit. Deduplicate by their deterministic key/def.
	for key, name in w.spawn_wrapper_names {
		if key !in g.spawn_wrapper_names {
			g.spawn_wrapper_names[key.clone()] = name.clone()
		}
	}
	if ordered.len > 0 {
		for wrappers in w.parallel_chunk_wrapper_defs {
			if wrappers.chunk_idx < 0 || wrappers.chunk_idx >= ordered_wrapper_defs.len {
				continue
			}
			for def in wrappers.spawn {
				normalized := if g.cache_split {
					ww.rewrite_cache_string_symbols(def)
				} else if string_id_remap.len > 0 {
					remap_scoped_worker_string_symbols(def, string_id_remap, user_c_symbols)
				} else {
					def.clone()
				}
				ordered_wrapper_defs[wrappers.chunk_idx].spawn << normalized
			}
		}
	} else {
		for def in w.spawn_wrapper_defs {
			if g.cache_split {
				g.add_spawn_wrapper_def(ww.rewrite_cache_string_symbols(def))
			} else if string_id_remap.len > 0 {
				g.add_spawn_wrapper_def(remap_scoped_worker_string_symbols(def, string_id_remap,
					user_c_symbols))
			} else {
				g.add_spawn_wrapper_def(def.clone())
			}
		}
	}
	for key, name in w.callback_wrapper_names {
		if key !in g.callback_wrapper_names {
			g.callback_wrapper_names[key.clone()] = name.clone()
		}
	}
	if ordered.len > 0 {
		for wrappers in w.parallel_chunk_wrapper_defs {
			if wrappers.chunk_idx < 0 || wrappers.chunk_idx >= ordered_wrapper_defs.len {
				continue
			}
			for def in wrappers.callback {
				normalized := if g.cache_split {
					ww.rewrite_cache_string_symbols(def)
				} else if string_id_remap.len > 0 {
					remap_scoped_worker_string_symbols(def, string_id_remap, user_c_symbols)
				} else {
					def.clone()
				}
				ordered_wrapper_defs[wrappers.chunk_idx].callback << normalized
			}
		}
	} else {
		for def in w.callback_wrapper_defs {
			if g.cache_split {
				g.add_callback_wrapper_def(ww.rewrite_cache_string_symbols(def))
			} else if string_id_remap.len > 0 {
				g.add_callback_wrapper_def(remap_scoped_worker_string_symbols(def, string_id_remap,
					user_c_symbols))
			} else {
				g.add_callback_wrapper_def(def.clone())
			}
		}
	}
}

fn (mut g FlatGen) replay_ordered_parallel_wrapper_defs(wrapper_defs []ParallelChunkWrapperDefs) {
	for wrappers in wrapper_defs {
		for def in wrappers.spawn {
			g.add_spawn_wrapper_def(def)
		}
		for def in wrappers.callback {
			g.add_callback_wrapper_def(def)
		}
	}
}

// run_pre_dispatch_parallel overlaps the serial pre-dispatch work: the
// fixed-storage-const scan runs on a helper thread while the master collects
// the fn work items and pre-seeds the string/fn-ptr tables the workers need.
// Returns false when the parallel path is not applicable (the caller then
// runs the serial order).
fn (mut g FlatGen) run_pre_dispatch_parallel(no_parallel bool) bool {
	$if windows {
		return false
	} $else {
		if no_parallel {
			return false
		}
		if isnil(g.a.worker_pool) {
			g.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		mut fs_worker := g.new_parallel_worker(0)
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		if fail.len > 0 {
			g.a.worker_pool.run([
				workers.Task{
					run:        fixed_storage_scan_thread
					arg:        voidptr(fs_worker)
					force_sync: fail == 'cgen:all' || fail == 'cgen:pre:all' || fail == 'cgen:pre:0'
				},
				workers.Task{
					run:        pre_dispatch_master_thread
					arg:        voidptr(g)
					force_sync: true
				},
			])
			g.refine_fn_item_costs(no_parallel, false)
		} else {
			// Item selection only reads the AST and immutable checker tables, so let
			// its exact-cost pass use the otherwise-idle pool while the independent
			// fixed-storage scan finishes on a helper thread.
			mut psw := time.new_stopwatch()
			fixed_storage_thread := spawn fixed_storage_scan_thread(voidptr(fs_worker))
			g.prepare_pre_dispatch_master()
			g.timing_profile('  [ttime]     cg prep master ${f64(psw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			psw.restart()
			g.refine_fn_item_costs(no_parallel, true)
			g.timing_profile('  [ttime]     cg cost refine ${f64(psw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			psw.restart()
			_ = fixed_storage_thread.wait()
			g.timing_profile('  [ttime]     cg fs wait     ${f64(psw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		}
		g.publish_fixed_storage_scan(mut fs_worker)
		if g.parallel_prepared && !g.prep_externs_pending {
			// Item-body and top-level C-extern refs are fully collected (fused
			// prep + exact-cost pass or its serial fallback); the pre-dispatch
			// preseed can reuse them instead of re-walking every selected body.
			g.c_extern_refs_ready = true
		}
		return true
	}
}

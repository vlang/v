module c

import os
import runtime
import strings
import v3.flat
import v3.types
import v3.workers

const max_flat_cgen_jobs = 2
const min_flat_cgen_parallel_items = 1024
const scoped_cgen_worker_batches = 256

$if !windows {
	// FlatCgenChunkArgs represents flat cgen chunk args data used by c.
	struct FlatCgenChunkArgs {
		worker         voidptr
		work_items_ptr voidptr
		is_master      bool
	}

	// fixed_storage_scan_thread runs the fixed-storage-const use scan (a full
	// post-transform AST pass) on a private fork while the master collects the
	// fn work items and pre-seeds the parallel tables.
	fn fixed_storage_scan_thread(arg voidptr) voidptr {
		mut w := unsafe { &FlatGen(arg) }
		scope := cgen_worker_scope_begin(w.scope_parallel_workers)
		w.collect_fixed_storage_consts()
		w.precompute_embedded_fields()
		w.precompute_param_type_index()
		w.precompute_concrete_optional_abi_fns()
		w.worker_scope = scope
		cgen_worker_scope_leave(scope)
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
}

fn (mut g FlatGen) prepare_pre_dispatch_master() {
	mut n_items := 0
	if g.scope_parallel_workers {
		selection_scope := cgen_worker_scope_begin(true)
		master_tc := g.tc
		g.tc = g.clone_parallel_type_checker()
		g.want_parallel_prep = false
		items := g.ensure_fn_gen_items()
		g.tc = master_tc
		cgen_worker_scope_leave(selection_scope)
		items_scope := cgen_worker_scope_begin(true)
		mut owned_items := []FlatFnGenItem{cap: items.len}
		for item in items {
			owned_items << FlatFnGenItem{
				node_id:                   item.node_id
				file:                      item.file.clone()
				module:                    item.module.clone()
				c_name:                    item.c_name.clone()
				cost:                      item.cost
				is_program_specialization: item.is_program_specialization
			}
		}
		g.fn_gen_items = owned_items
		g.emitted_fns = clone_cgen_string_bool_map(g.emitted_fns)
		cgen_worker_scope_leave(items_scope)
		g.scoped_fn_items_scope = items_scope
		g.c_name_cache = &CNameCache{}
		cgen_worker_scope_free(selection_scope)
		prep_scope := cgen_worker_scope_begin(true)
		prep_master_tc := g.tc
		g.tc = g.clone_parallel_type_checker()
		g.prepare_parallel_items(g.fn_gen_items)
		g.tc = prep_master_tc
		cgen_worker_scope_leave(prep_scope)
		g.str_lits = clone_cgen_string_list(g.str_lits)
		g.str_lit_ids = clone_cgen_string_int_map(g.str_lit_ids)
		g.fn_ptr_types = clone_cgen_string_map(g.fn_ptr_types)
		g.used_fn_ptr_types = clone_cgen_string_bool_map(g.used_fn_ptr_types)
		g.c_extern_refs = clone_cgen_string_bool_map(g.c_extern_refs)
		g.c_name_cache = &CNameCache{}
		cgen_worker_scope_free(prep_scope)
		n_items = g.fn_gen_items.len
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
		g.c_extern_refs_ready = true
		g.parallel_prepared = true
	}
	// Force the lazily-built const short-name index now: workers share it
	// read-only, so it must be complete before any fork starts.
	_ = g.unique_const_ref_name('__v3_prewarm__') or { '' }
}

fn clone_cgen_string_list(values []string) []string {
	mut cloned := []string{cap: values.len}
	for value in values {
		cloned << value.clone()
	}
	return cloned
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

// write_scoped_cgen_batch_output writes a batch builder while its disposable
// scope is still active, avoiding a second output copy in the parent arena.
fn (mut g FlatGen) write_scoped_cgen_batch_output(batch &FlatGen) bool {
	mut file := os.open_append(g.scoped_fn_output_path) or {
		g.output_error = err.msg()
		return false
	}
	unsafe {
		file.write_full_buffer(batch.sb.data, usize(batch.sb.len)) or {
			g.output_error = err.msg()
			file.close()
			return false
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
		g.needed_optional_types[opt_name.clone()] = val_type.clone()
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
		g.add_spawn_wrapper_def(def.clone())
	}
	for key, name in batch.callback_wrapper_names {
		if key !in g.callback_wrapper_names {
			g.callback_wrapper_names[key.clone()] = name.clone()
		}
	}
	for def in batch.callback_wrapper_defs {
		g.add_callback_wrapper_def(def.clone())
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
		batch.gen_fn_items(items[start..end])
		output_streamed := g.scoped_fn_output_path.len > 0
		output_ok := !output_streamed || g.write_scoped_cgen_batch_output(batch)
		cgen_worker_scope_leave(scratch_scope)
		if !output_ok {
			cgen_worker_scope_free(scratch_scope)
			return
		}
		g.absorb_scoped_cgen_batch(batch, output_streamed)
		cgen_worker_scope_free(scratch_scope)
		start = end
	}
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
		output_streamed := g.scoped_fn_output_path.len > 0
		output_ok := !output_streamed || g.write_scoped_cgen_batch_output(batch)
		cgen_worker_scope_leave(scratch_scope)
		if !output_ok {
			cgen_worker_scope_free(scratch_scope)
			return
		}
		g.absorb_scoped_cgen_batch(batch, output_streamed)
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
				name: field.name.clone()
				typ:  types.clone_owned_type(field.typ)
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
	g.embedded_fields_by_type = clone_embedded_fields_by_type(fs_worker.embedded_fields_by_type)
	g.concrete_optional_abi_fns = fs_worker.concrete_optional_abi_fns.clone()
	cgen_worker_scope_free(fs_worker.worker_scope)
	fs_worker.worker_scope = unsafe { nil }
}

// gen_fns_dispatch emits fns dispatch output for c.
fn (mut g FlatGen) gen_fns_dispatch(no_parallel bool) {
	if no_parallel {
		g.gen_fns()
		g.gen_synthetic_main_after_fns()
		return
	}
	items := g.ensure_fn_gen_items()
	n_items := items.len
	$if windows {
		g.gen_fn_items(items)
		g.gen_synthetic_main_after_fns()
		return
	} $else {
		if isnil(g.a.worker_pool) {
			g.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		n_jobs := flat_cgen_job_count(g.a.worker_pool.size() + 1, n_items)
		if g.output_path.len > 0 && !g.cache_split && g.scope_parallel_workers {
			g.scoped_fn_output_path = '${g.output_path}.v3-fns.tmp.0'
			g.scoped_fn_output_paths = [g.scoped_fn_output_path]
			os.rm(g.scoped_fn_output_path) or {}
		}
		if g.scope_parallel_workers || n_items < min_flat_cgen_parallel_items || n_jobs <= 1 {
			if g.scope_parallel_workers {
				g.gen_fn_items_scoped_master_batches(items)
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
		if !g.parallel_prepared {
			g.prepare_parallel_items(items)
		}
		mut chunk_items := split_flat_cgen_items(items, n_jobs)
		chunk_count := chunk_items.len
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
		mut cgen_workers := []voidptr{cap: thread_count}
		if g.scoped_fn_output_path.len > 0 {
			for ci := 0; ci < thread_count; ci++ {
				worker_path := '${g.scoped_fn_output_path}.${ci + 1}'
				g.scoped_fn_output_paths << worker_path
				os.rm(worker_path) or {}
			}
		}
		worker_setup_scope := cgen_worker_scope_begin(g.scope_parallel_workers)
		for ci := 0; ci < thread_count; ci++ {
			mut w := g.new_parallel_dispatch_worker(ci + 1)
			if g.scoped_fn_output_path.len > 0 {
				w.scoped_fn_output_path = g.scoped_fn_output_paths[ci + 1]
			}
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
	mut target_cost := total_cost / n_jobs
	if total_cost % n_jobs != 0 {
		target_cost++
	}
	if target_cost < 1 {
		target_cost = 1
	}
	mut current := []FlatFnGenItem{}
	mut current_cost := 0
	mut chunks_left := n_jobs
	// The master emits chunk[0] while also paying for the serial prep before
	// and the merges after the region; give it a lighter share.
	mut chunk_target := target_cost * 11 / 16
	for idx, item in items {
		remaining_items := items.len - idx
		if current.len > 0 && current_cost >= chunk_target && chunks_left > 1
			&& remaining_items >= chunks_left {
			chunks << current
			current = []FlatFnGenItem{}
			current_cost = 0
			chunks_left--
			chunk_target = target_cost
		}
		current << item
		current_cost += item.cost
	}
	if current.len > 0 {
		chunks << current
	}
	return chunks
}

// fn_item_cost_and_prep computes the split cost, collects C-extern refs, and
// pre-seeds function-pointer types in one subtree traversal.
fn (mut g FlatGen) fn_item_cost_and_prep(node_id flat.NodeId, mut stack []flat.NodeId, mut type_text_cache map[string]bool) int {
	mut cost := 0
	stack.clear()
	stack << node_id
	for stack.len > 0 {
		current_id := stack.pop()
		idx := int(current_id)
		if idx < 0 || idx >= g.a.nodes.len {
			continue
		}
		node := g.a.nodes[idx]
		cost++
		if node.kind == .selector {
			g.collect_c_extern_ref_from_node(node)
		}
		if g.should_preseed_parallel_type_text_cached(node.typ, mut type_text_cache) {
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
	return cost
}

// prepare_parallel_items supports prepare parallel items handling for FlatGen.
fn (mut g FlatGen) prepare_parallel_items(items []FlatFnGenItem) {
	mut stack := []flat.NodeId{cap: 256}
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
		node := g.a.nodes[idx]
		if node.kind == .string_literal {
			g.intern_string(node.value)
		}
		g.collect_c_extern_ref_from_node(node)
		if g.should_preseed_parallel_type_text_cached(node.typ, mut type_text_cache) {
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

fn (g &FlatGen) parallel_cached_expr_type(id flat.NodeId, node flat.Node) ?types.Type {
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
// master snapshot; result tables remain private.
fn (g &FlatGen) new_parallel_result_worker(worker_id int) &FlatGen {
	return g.new_parallel_worker_config(worker_id, true)
}

fn (g &FlatGen) new_parallel_worker_config(worker_id int, result_only bool) &FlatGen {
	return &FlatGen{
		sb:                             strings.new_builder(64_000)
		a:                              unsafe { g.a }
		used_fns:                       g.used_fns
		used_fn_names:                  g.used_fn_names
		test_files:                     if result_only { g.test_files } else { g.test_files.clone() }
		str_lits:                       if result_only || g.scope_parallel_workers {
			g.str_lits
		} else {
			g.str_lits.clone()
		}
		str_lit_ids:                    if result_only || g.scope_parallel_workers {
			g.str_lit_ids
		} else {
			g.str_lit_ids.clone()
		}
		str_lits_shared:                g.scope_parallel_workers
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
		iface_type_ids:                 g.iface_type_ids
		interface_boxed_types:          g.interface_boxed_types
		interface_boxed_types_done:     g.interface_boxed_types_done
		ierror_method_emit_names:       g.ierror_method_emit_names
		ierror_stack_pointer_aliases:   []map[string]bool{}
		local_pointer_storage_by_owner: map[string]bool{}
		local_c_type_by_owner:          map[string]string{}
		local_raw_type_by_owner:        map[string]string{}
		local_shared_storage_by_owner:  map[string]bool{}
		local_fn_value_c_name_by_owner: map[string]string{}
		sum_name_lookup:                g.sum_name_lookup
		module_init_fns:                g.module_init_fns
		module_init_fn_modules:         g.module_init_fn_modules
		module_imports:                 g.module_imports
		libc_compat_fns:                g.libc_compat_fns.clone()
		tc:                             if result_only {
			unsafe { g.tc }
		} else {
			g.clone_parallel_type_checker()
		}
		has_builtins:                   g.has_builtins
		cache_split:                    g.cache_split
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
		emitted_fns:                g.emitted_fns
		array_method_cache:         if result_only {
			g.array_method_cache
		} else {
			g.array_method_cache.clone()
		}
		param_types_cache:          if result_only {
			g.param_types_cache
		} else {
			g.param_types_cache.clone()
		}
		embedded_fields_by_type:    g.embedded_fields_by_type
		param_types_by_short:       g.param_types_by_short
		generic_method_candidates:  g.generic_method_candidates
		spawn_wrapper_names:        g.spawn_wrapper_names.clone()
		spawn_wrapper_defs:         g.spawn_wrapper_defs.clone()
		spawn_wrapper_defs_seen:    g.spawn_wrapper_defs_seen.clone()
		callback_wrapper_names:     g.callback_wrapper_names.clone()
		callback_wrapper_defs:      g.callback_wrapper_defs.clone()
		callback_wrapper_defs_seen: g.callback_wrapper_defs_seen.clone()
		scope_parallel_workers:     g.scope_parallel_workers
		c_name_cache:               &CNameCache{}
		// The const short-name index is read-only after its first build (the
		// master queries it during the const precompute, before the forks);
		// sharing it avoids a rebuild per worker.
		const_short_index: g.const_short_index
		mut_recv_facts:    &FnNameFactCache{}
	}
}

// clone_parallel_type_checker builds a per-worker TypeChecker for parallel codegen.
//
// During codegen the checker's lookup tables are READ-ONLY: cgen only ever assigns the
// scalar `cur_file`/`cur_module` fields, and the read paths it uses (expr_type, c_type,
// parse_type, resolve_type, cached_resolved_call) never write into the big maps — the only
// memoizing write is into `type_cache`, which is left nil here so workers take the uncached
// path. V maps and arrays are reference types, so the read-only tables are SHARED by
// reference (no `.clone()`), exactly like the already-shared `a` FlatAst. This avoids
// deep-copying the program-wide `expr_type_*`/`structs`/signature tables once per worker,
// which was the bulk of parallel cgen's extra RAM and serial setup time.
//
// Only genuinely per-worker mutable state is given its own copy: the scope chain (gen pushes
// child scopes) and `errors` (avoid a concurrent append race, though gen does not emit any).
fn (g &FlatGen) clone_parallel_type_checker() &types.TypeChecker {
	// Cgen only reads file-level bindings. Give each worker an empty child scope
	// over the immutable checked scope instead of cloning the full symbol table.
	fs := types.new_scope(g.tc.file_scope)
	mut wtc := &types.TypeChecker{
		a:                                  unsafe { g.tc.a }
		fn_ret_types:                       g.tc.fn_ret_types
		fn_param_types:                     g.tc.fn_param_types
		fn_ret_type_texts:                  g.tc.fn_ret_type_texts
		fn_param_type_texts:                g.tc.fn_param_type_texts
		fn_type_files:                      g.tc.fn_type_files
		fn_type_modules:                    g.tc.fn_type_modules
		fn_generic_params:                  g.tc.fn_generic_params
		specialized_generic_fns:            g.tc.specialized_generic_fns
		fn_variadic:                        g.tc.fn_variadic
		fn_implicit_veb_ctx:                g.tc.fn_implicit_veb_ctx
		c_variadic_fns:                     g.tc.c_variadic_fns
		structs:                            g.tc.structs
		struct_modules:                     g.tc.struct_modules
		struct_files:                       g.tc.struct_files
		soa_structs:                        g.tc.soa_structs
		struct_error_embeds_shadow_builtin: g.tc.struct_error_embeds_shadow_builtin
		struct_generic_params:              g.tc.struct_generic_params
		struct_field_c_abi_fns:             g.tc.struct_field_c_abi_fns
		unions:                             g.tc.unions
		type_aliases:                       g.tc.type_aliases
		type_alias_c_abi_fns:               g.tc.type_alias_c_abi_fns
		sum_types:                          g.tc.sum_types
		sum_generic_params:                 g.tc.sum_generic_params
		enum_names:                         g.tc.enum_names
		enum_fields:                        g.tc.enum_fields
		flag_enums:                         g.tc.flag_enums
		interface_names:                    g.tc.interface_names
		interface_fields:                   g.tc.interface_fields
		interface_embeds:                   g.tc.interface_embeds
		interface_abstract_methods:         g.tc.interface_abstract_methods
		interface_impl_name_snapshots:      g.tc.interface_impl_name_snapshots
		c_globals:                          g.tc.c_globals
		const_types:                        g.tc.const_types
		const_exprs:                        g.tc.const_exprs
		const_modules:                      g.tc.const_modules
		const_files:                        g.tc.const_files
		const_suffixes:                     g.tc.const_suffixes
		imports:                            g.tc.imports
		file_imports:                       g.tc.file_imports
		file_selective_imports:             g.tc.file_selective_imports
		file_modules:                       g.tc.file_modules
		file_scope:                         fs
		cur_scope:                          fs
		scope_pool:                         []&types.Scope{}
		has_builtins:                       g.tc.has_builtins
		resolution_type_mode:               g.tc.resolution_type_mode
		cur_module:                         g.tc.cur_module
		cur_file:                           g.tc.cur_file
		errors:                             g.tc.errors.clone()
		resolved_call_names:                g.tc.resolved_call_names
		resolved_call_set:                  g.tc.resolved_call_set
		resolved_fn_value_names:            g.tc.resolved_fn_value_names
		resolved_fn_value_set:              g.tc.resolved_fn_value_set
		statement_nodes:                    g.tc.statement_nodes
		expr_type_values:                   g.tc.expr_type_values
		expr_type_set:                      g.tc.expr_type_set
		checking_nodes:                     g.tc.checking_nodes
		parallel_check_sparse:              g.tc.parallel_check_sparse
		check_range_lo:                     g.tc.check_range_lo
		check_range_hi:                     g.tc.check_range_hi
		sparse_resolved_call_names:         g.tc.sparse_resolved_call_names
		sparse_resolved_fn_values:          g.tc.sparse_resolved_fn_values
		sparse_statement_nodes:             g.tc.sparse_statement_nodes
		sparse_expr_type_values:            g.tc.sparse_expr_type_values
		sparse_checking_nodes:              g.tc.sparse_checking_nodes
		diagnose_unknown_calls:             g.tc.diagnose_unknown_calls
		reject_unlowered_map_mutation:      g.tc.reject_unlowered_map_mutation
		diagnostic_files:                   g.tc.diagnostic_files
		selected_file_called_fns:           g.tc.selected_file_called_fns
		smartcasts:                         g.tc.smartcasts
		// Read-only map cgen uses to recover substituted signatures for generic-receiver
		// method values (`Box[int].method` as a callback); without it a parallel worker
		// sees an empty map and gen_method_value_closure falls through.
		generic_method_value_info: g.tc.generic_method_value_info
		params_structs:            g.tc.params_structs
	}
	wtc.inherit_ownership_codegen_metadata_from(g.tc)
	// A private empty TypeCache lets the worker use the lazily-built lookup
	// indexes (short type names, local fn decls) and the field/IError
	// memoizations instead of their uncached full-scan fallbacks. It shares no
	// state with other threads.
	wtc.set_fresh_type_cache_based_on(g.tc, g.tc.type_cache_parse_enabled())
	return wtc
}

// merge_parallel_worker supports merge parallel worker handling for FlatGen.
fn (mut g FlatGen) merge_parallel_worker(w &FlatGen) {
	mut ww := unsafe { w }
	if g.output_error.len == 0 && w.output_error.len > 0 {
		g.output_error = w.output_error.clone()
	}
	worker_output := ww.sb.str()
	if worker_output.len > 0 {
		g.fn_segs << worker_output
	} else {
		unsafe { worker_output.free() }
	}
	// The ordered segment owns the copied output; release the worker builder.
	unsafe { ww.sb.free() }
	for segment in w.fn_segs {
		g.fn_segs << segment.clone()
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
	for def in w.spawn_wrapper_defs {
		g.add_spawn_wrapper_def(def.clone())
	}
	for key, name in w.callback_wrapper_names {
		if key !in g.callback_wrapper_names {
			g.callback_wrapper_names[key.clone()] = name.clone()
		}
	}
	for def in w.callback_wrapper_defs {
		g.add_callback_wrapper_def(def.clone())
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
		if g.scope_parallel_workers {
			scan_scope := cgen_worker_scope_begin(true)
			mut scan := g.new_parallel_worker(0)
			scan.collect_fixed_storage_consts()
			scan.precompute_param_type_index()
			scan.precompute_concrete_optional_abi_fns()
			cgen_worker_scope_leave(scan_scope)
			g.fixed_storage_consts = scan.fixed_storage_consts.clone()
			g.param_types_by_short = clone_param_types_by_short(scan.param_types_by_short)
			g.concrete_optional_abi_fns = scan.concrete_optional_abi_fns.clone()
			cgen_worker_scope_free(scan_scope)
			g.prepare_pre_dispatch_master()
			return true
		}
		mut fs_worker := g.new_parallel_worker(0)
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
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
		g.publish_fixed_storage_scan(mut fs_worker)
		return true
	}
}

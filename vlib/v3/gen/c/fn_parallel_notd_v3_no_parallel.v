module c

import runtime
import strings
import v3.flat
import v3.types

const max_flat_cgen_jobs = 10
const min_flat_cgen_parallel_items = 1024

$if !windows {
	// FlatCgenChunkArgs represents flat cgen chunk args data used by c.
	struct FlatCgenChunkArgs {
		worker         voidptr
		work_items_ptr voidptr
	}

	// C.pthread_t declares C pthread t data used by c.
	@[typedef]
	struct C.pthread_t {}

	// C.pthread_create declares the C pthread_create symbol used by c.
	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int

	// C.pthread_join declares the C pthread_join symbol used by c.
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int

	// C.pthread_attr_init declares the C pthread_attr_init symbol used by c.
	fn C.pthread_attr_init(attr voidptr) int

	// C.pthread_attr_setstacksize declares the C pthread_attr_setstacksize symbol used by c.
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int

	// C.pthread_attr_destroy declares the C pthread_attr_destroy symbol used by c.
	fn C.pthread_attr_destroy(attr voidptr) int

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

	// postamble_segments_thread_a / _b emit the body-independent postamble
	// groups on private FlatGen forks while the body workers run.
	fn postamble_segments_thread_a(arg voidptr) voidptr {
		mut w := unsafe { &FlatGen(arg) }
		scope := cgen_worker_scope_begin(w.scope_parallel_workers)
		w.emit_postamble_segments_a()
		w.worker_scope = scope
		cgen_worker_scope_leave(scope)
		return unsafe { nil }
	}

	fn postamble_segments_thread_b(arg voidptr) voidptr {
		mut w := unsafe { &FlatGen(arg) }
		scope := cgen_worker_scope_begin(w.scope_parallel_workers)
		w.emit_postamble_segments_b()
		w.worker_scope = scope
		cgen_worker_scope_leave(scope)
		return unsafe { nil }
	}

	// flat_cgen_chunk_thread supports flat cgen chunk thread handling for c.
	fn flat_cgen_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &FlatCgenChunkArgs(arg) }
		mut w := unsafe { &FlatGen(a.worker) }
		items := unsafe { &[]FlatFnGenItem(a.work_items_ptr) }
		scope := cgen_worker_scope_begin(w.scope_parallel_workers)
		w.gen_fn_items(*items)
		w.worker_scope = scope
		cgen_worker_scope_leave(scope)
		return unsafe { nil }
	}
}

fn cgen_worker_scope_begin(enabled bool) voidptr {
	$if prealloc {
		if enabled {
			return unsafe { prealloc_scope_begin() }
		}
	}
	return unsafe { nil }
}

fn cgen_worker_scope_leave(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_leave(scope) }
		}
	}
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
	n_jobs := flat_cgen_job_count(runtime.nr_jobs(), n_items)
	$if windows {
		g.gen_fn_items(items)
		g.gen_synthetic_main_after_fns()
		return
	} $else {
		if n_items < min_flat_cgen_parallel_items || n_jobs <= 1 {
			g.gen_fn_items(items)
			g.gen_synthetic_main_after_fns()
			return
		}
		g.parallel_used = true
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
		// other chunks get helper threads, plus ONE extra helper that emits the
		// body-independent postamble groups (previously a serial tail after the
		// region) on a private fork. Worker chunk ci keeps the temp-name base
		// (ci+1)*100_000, so each chunk stays in its own disjoint _tN range and
		// the numbering is deterministic.
		thread_count := chunk_count - 1
		mut thread_ids := []C.pthread_t{len: thread_count + 2}
		mut args := []FlatCgenChunkArgs{cap: thread_count}
		mut workers := []voidptr{cap: thread_count}
		for ci := 0; ci < thread_count; ci++ {
			w := g.new_parallel_worker(ci + 1)
			workers << voidptr(w)
		}
		mut post_worker_a := g.postamble_fork(thread_count + 1)
		mut post_worker_b := g.postamble_fork(thread_count + 2)
		for ci := 0; ci < thread_count; ci++ {
			args << FlatCgenChunkArgs{
				worker:         workers[ci]
				work_items_ptr: unsafe { voidptr(&chunk_items[ci + 1]) }
			}
		}

		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		for ci := 0; ci < thread_count; ci++ {
			C.pthread_create(unsafe { &thread_ids[ci] }, attr, flat_cgen_chunk_thread,
				unsafe { voidptr(&args[ci]) })
		}
		C.pthread_create(unsafe { &thread_ids[thread_count] }, attr, postamble_segments_thread_a,
			voidptr(post_worker_a))
		C.pthread_create(unsafe { &thread_ids[thread_count + 1] }, attr,
			postamble_segments_thread_b, voidptr(post_worker_b))
		C.pthread_attr_destroy(attr)
		// Master emits chunk[0] into g.sb while the helper threads run, using
		// worker 0's temp base so its temps stay in their own range.
		g.tmp_count = 100_000
		g.gen_fn_items(chunk_items[0])
		for ci := 0; ci < thread_count + 2; ci++ {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}
		for ci := 0; ci < thread_count; ci++ {
			w := unsafe { &FlatGen(workers[ci]) }
			if w.worker_scope != unsafe { nil } {
				g.parallel_worker_scopes << w.worker_scope
			}
		}
		if post_worker_a.worker_scope != unsafe { nil } {
			g.parallel_worker_scopes << post_worker_a.worker_scope
		}
		if post_worker_b.worker_scope != unsafe { nil } {
			g.parallel_worker_scopes << post_worker_b.worker_scope
		}
		// Adopt the postamble fork's segments and emission-dedup state FIRST
		// (it ran logically at the start of the postamble), then merge helper
		// output in fixed chunk order — body-registered additions (fn-ptr
		// types, optional/fixed-array needs) then land on top, exactly like
		// the old serial order.
		g.adopt_postamble_worker(post_worker_a, post_worker_b)
		master_output := g.sb.str()
		unsafe { g.sb.free() }
		g.sb = strings.new_builder(4096)
		if master_output.len > 0 {
			g.fn_segs << master_output
		} else {
			unsafe { master_output.free() }
		}
		for ci := 0; ci < thread_count; ci++ {
			w := unsafe { &FlatGen(workers[ci]) }
			g.merge_parallel_worker(w)
		}
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
	if g.tc.parallel_check_sparse {
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
		g.resolve_fn_ptr_type(g.fn_ptr_type_key(typ))
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

// fn_ptr_type_key supports fn ptr type key handling for FlatGen.
fn (mut g FlatGen) fn_ptr_type_key(typ types.FnType) string {
	ret := if typ.return_type is types.Void { 'void' } else { g.tc.c_type(typ.return_type) }
	if typ.params.len == 0 {
		return 'fn_ptr:${ret}|void'
	}
	mut params := []string{}
	for param in typ.params {
		params << g.tc.c_type(param)
	}
	return 'fn_ptr:${ret}|${params.join(', ')}'
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
	return &FlatGen{
		sb:                             strings.new_builder(64_000)
		a:                              unsafe { g.a }
		used_fns:                       g.used_fns
		used_fn_names:                  g.used_fn_names
		test_files:                     g.test_files.clone()
		str_lits:                       g.str_lits.clone()
		str_lit_ids:                    g.str_lit_ids.clone()
		global_types:                   g.global_types
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
		ierror_method_emit_names:       g.ierror_method_emit_names
		ierror_stack_pointer_aliases:   []map[string]bool{}
		local_pointer_storage_by_owner: map[string]bool{}
		local_c_type_by_owner:          map[string]string{}
		local_shared_storage_by_owner:  map[string]bool{}
		sum_name_lookup:                g.sum_name_lookup
		module_init_fns:                g.module_init_fns
		module_init_fn_modules:         g.module_init_fn_modules
		module_imports:                 g.module_imports
		libc_compat_fns:                g.libc_compat_fns.clone()
		tc:                             g.clone_parallel_type_checker()
		has_builtins:                   g.has_builtins
		cache_split:                    g.cache_split
		tmp_count:                      (worker_id + 1) * 100_000
		line_start:                     true
		modules:                        g.modules
		fn_ptr_types:                   g.fn_ptr_types.clone()
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
		const_runtime_inits:            g.const_runtime_inits.clone()
		runtime_inits:                  g.runtime_inits.clone()
		compiler_vroot:                 g.compiler_vroot
		compiler_vexe:                  g.compiler_vexe
		cur_param_names:                g.cur_param_names.clone()
		cur_param_type_values:          g.cur_param_type_values.clone()
		cur_param_types:                g.cur_param_types.clone()
		cur_concrete_optional_params:   g.cur_concrete_optional_params.clone()
		cur_mut_params:                 g.cur_mut_params.clone()
		cur_mut_param_owners:           g.cur_mut_param_owners.clone()
		cur_fn_ret:                     g.cur_fn_ret
		cur_fn_ret_is_optional:         g.cur_fn_ret_is_optional
		cur_fn_ret_base:                g.cur_fn_ret_base
		loop_label_depths:              map[string]int{}
		expected_expr_type:             g.expected_expr_type
		expected_enum:                  g.expected_enum
		needed_optional_types:          g.needed_optional_types.clone()
		emitted_optional_types:         g.emitted_optional_types.clone()
		emitted_fns:                    g.emitted_fns.clone()
		array_method_cache:             g.array_method_cache.clone()
		param_types_cache:              g.param_types_cache.clone()
		embedded_fields_by_type:        g.embedded_fields_by_type
		param_types_by_short:           g.param_types_by_short
		generic_method_candidates:      g.generic_method_candidates
		spawn_wrapper_names:            g.spawn_wrapper_names.clone()
		spawn_wrapper_defs:             g.spawn_wrapper_defs.clone()
		callback_wrapper_names:         g.callback_wrapper_names.clone()
		callback_wrapper_defs:          g.callback_wrapper_defs.clone()
		scope_parallel_workers:         g.scope_parallel_workers
		c_name_cache:                   &CNameCache{}
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
	mut fs := types.new_scope(unsafe { nil })
	fs.names = g.tc.file_scope.names.clone()
	fs.types = g.tc.file_scope.types.clone()
	fs.name_indexes = g.tc.file_scope.name_indexes.clone()
	fs.generations = g.tc.file_scope.generations.clone()
	fs.next_generation = g.tc.file_scope.next_generation
	fs.lifetime = g.tc.file_scope.lifetime
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
		sparse_resolved_call_names:         g.tc.sparse_resolved_call_names
		sparse_resolved_fn_values:          g.tc.sparse_resolved_fn_values
		sparse_statement_nodes:             g.tc.sparse_statement_nodes
		sparse_expr_type_values:            g.tc.sparse_expr_type_values
		sparse_checking_nodes:              g.tc.sparse_checking_nodes
		diagnose_unknown_calls:             g.tc.diagnose_unknown_calls
		reject_unlowered_map_mutation:      g.tc.reject_unlowered_map_mutation
		diagnostic_files:                   g.tc.diagnostic_files
		selected_file_called_fns:           g.tc.selected_file_called_fns
		cur_fn_ret_type:                    g.tc.cur_fn_ret_type
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
	worker_output := ww.sb.str()
	if worker_output.len > 0 {
		g.fn_segs << worker_output
	} else {
		unsafe { worker_output.free() }
	}
	// The ordered segment owns the copied output; release the worker builder.
	unsafe { ww.sb.free() }
	for opt_name, val_type in w.needed_optional_types {
		g.needed_optional_types[opt_name] = val_type
	}
	for encoded, name in w.fn_ptr_types {
		if encoded !in g.fn_ptr_types {
			g.fn_ptr_types[encoded] = name
		}
	}
	for name, enabled in w.libc_compat_fns {
		if enabled {
			g.libc_compat_fns[name] = true
		}
	}
	// Spawn wrappers (thread arg structs + trampoline fns) are generated on demand
	// inside fn bodies, so a worker that emits a `spawn` produces wrapper defs the
	// master must also emit. Deduplicate by their deterministic key/def.
	for key, name in w.spawn_wrapper_names {
		if key !in g.spawn_wrapper_names {
			g.spawn_wrapper_names[key] = name
		}
	}
	for def in w.spawn_wrapper_defs {
		if def !in g.spawn_wrapper_defs {
			g.spawn_wrapper_defs << def
		}
	}
	for key, name in w.callback_wrapper_names {
		if key !in g.callback_wrapper_names {
			g.callback_wrapper_names[key] = name
		}
	}
	for def in w.callback_wrapper_defs {
		if def !in g.callback_wrapper_defs {
			g.callback_wrapper_defs << def
		}
	}
}

// adopt_postamble_worker takes over the postamble fork's emitted segments and
// its emission-dedup state, so the body-dependent postamble pieces that run
// after the region skip everything the fork already emitted and add only what
// the body workers registered on top (same net content as the old serial
// order).
// postamble_fork builds a FULL copy of the master FlatGen for a postamble
// helper thread: unlike new_parallel_worker (which lists only the fields body
// generation touches), the struct copy carries every collected table the
// postamble pieces read (C directives, flags, module tables, ...). Only the
// state the postamble WRITES is re-privatized: the builder, the checker (its
// cur_file/cur_module scratch and caches), cgen memoization caches, the c_name
// cache, and the emission-dedup maps that are adopted back after the join.
fn (g &FlatGen) postamble_fork(worker_id int) &FlatGen {
	mut w := *g
	w.sb = strings.new_builder(65536)
	w.line_start = true
	w.tc = g.clone_parallel_type_checker()
	w.c_name_cache = &CNameCache{}
	w.mut_recv_facts = &FnNameFactCache{}
	w.tmp_count = (worker_id + 1) * 100_000
	w.post_segs = []string{}
	w.emitted_optional_types = g.emitted_optional_types.clone()
	w.needed_optional_types = g.needed_optional_types.clone()
	w.emitted_fixed_array_typedefs = g.emitted_fixed_array_typedefs.clone()
	w.fixed_array_typedefs_needed = g.fixed_array_typedefs_needed.clone()
	w.emitted_fn_ptr_typedefs = g.emitted_fn_ptr_typedefs.clone()
	w.fn_ptr_types = g.fn_ptr_types.clone()
	w.multi_return_types = g.multi_return_types.clone()
	w.multi_return_type_names = g.multi_return_type_names.clone()
	w.libc_compat_fns = g.libc_compat_fns.clone()
	w.array_method_cache = g.array_method_cache.clone()
	w.param_types_cache = g.param_types_cache.clone()
	w.embedded_fields_by_type = g.embedded_fields_by_type.clone()
	w.spawn_wrapper_names = g.spawn_wrapper_names.clone()
	w.spawn_wrapper_defs = g.spawn_wrapper_defs.clone()
	w.callback_wrapper_names = g.callback_wrapper_names.clone()
	w.callback_wrapper_defs = g.callback_wrapper_defs.clone()
	w.c_extern_refs = g.c_extern_refs.clone()
	w.c_extern_refs_ready = g.c_extern_refs_ready
	w.worker_scope = unsafe { nil }
	w.parallel_worker_scopes = []voidptr{}
	// Snapshot of the interned-string table for the string_literals segment;
	// the master's later interning appends beyond it (see
	// string_literals_from).
	w.str_lits = g.str_lits.clone()
	w.str_lit_ids = g.str_lit_ids.clone()
	return &w
}

fn (mut g FlatGen) adopt_postamble_worker(wa &FlatGen, wb &FlatGen) {
	mut segs := []string{cap: 4}
	segs << wa.post_segs[0]
	segs << wb.post_segs[0]
	segs << wb.post_segs[1]
	segs << wb.post_segs[2]
	g.post_segs = segs
	// Fork A ran the typedef/struct pieces: adopt its emission-dedup state so
	// the post-region pieces only add what the body workers registered on top.
	g.emitted_optional_types = wa.emitted_optional_types.clone()
	g.needed_optional_types = wa.needed_optional_types.clone()
	g.emitted_fixed_array_typedefs = wa.emitted_fixed_array_typedefs.clone()
	g.fixed_array_typedefs_needed = wa.fixed_array_typedefs_needed.clone()
	g.fixed_array_typedefs_ready = wa.fixed_array_typedefs_ready
	g.emitted_fn_ptr_typedefs = wa.emitted_fn_ptr_typedefs.clone()
	g.fn_ptr_types = wa.fn_ptr_types.clone()
	mut libc := wa.libc_compat_fns.clone()
	for name, enabled in wb.libc_compat_fns {
		if enabled {
			libc[name] = true
		}
	}
	g.libc_compat_fns = libc.move()
	// Fork B retained the C-extern references collected by the fused item walk
	// and emitted the string table up to its snapshot.
	g.c_extern_refs = wb.c_extern_refs.clone()
	g.c_extern_refs_ready = wb.c_extern_refs_ready
	g.post_str_lits_snapshot = wb.str_lits.len
	// global_decls runs on fork B and queues program/global assignments for
	// `_vinit`; retain those queues when the emitted postamble is adopted.
	g.const_runtime_inits = wb.const_runtime_inits.clone()
	g.const_runtime_init_modules = wb.const_runtime_init_modules.clone()
	g.runtime_inits = wb.runtime_inits.clone()
	g.runtime_init_modules = wb.runtime_init_modules.clone()
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
		mut fs_worker := g.new_parallel_worker(0)
		mut tid := []C.pthread_t{len: 1}
		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		C.pthread_create(unsafe { &tid[0] }, attr, fixed_storage_scan_thread, voidptr(fs_worker))
		C.pthread_attr_destroy(attr)
		g.want_parallel_prep = true
		items := g.ensure_fn_gen_items()
		g.want_parallel_prep = false
		if items.len >= min_flat_cgen_parallel_items {
			// The fused item walk already interned and pre-seeded; only the
			// epilogue remains.
			if _ := g.ierror_interface_name() {
				g.intern_string('')
			}
			g.register_interface_strings()
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
		C.pthread_join(tid[0], unsafe { nil })
		if fs_worker.worker_scope != unsafe { nil } {
			g.parallel_worker_scopes << fs_worker.worker_scope
		}
		g.fixed_storage_consts = fs_worker.fixed_storage_consts.clone()
		// The helper also built these precomputes; hand them to the master so
		// later worker forks (and master-side emission) do not run with empty
		// maps. The fork is discarded, so a move is safe.
		g.param_types_by_short = fs_worker.param_types_by_short.move()
		g.concrete_optional_abi_fns = fs_worker.concrete_optional_abi_fns.move()
		return true
	}
}

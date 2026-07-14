module types

import runtime
import v3.flat

const min_parallel_check_items = 256
const max_parallel_check_jobs = 26
// Extra share of the total work (in percent of an even bucket) pre-assigned to
// the master's bucket; see split_check_items.
const check_master_bias_pct = i64(60)

struct CheckWorkItem {
	fn_idx   int
	range_lo int // first node id owned by this fn (fn subtree = [range_lo, fn_idx])
	file     string
	module   string
	cost     int
	rank     i64
}

$if !windows {
	struct CheckChunkArgs {
		worker    voidptr
		items_ptr voidptr
	}

	@[typedef]
	struct C.pthread_t {}

	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int
	fn C.pthread_attr_init(attr voidptr) int
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int
	fn C.pthread_attr_destroy(attr voidptr) int

	fn check_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &CheckChunkArgs(arg) }
		mut w := unsafe { &TypeChecker(a.worker) }
		items := unsafe { &[]CheckWorkItem(a.items_ptr) }
		w.check_fn_items_serial(*items)
		return unsafe { nil }
	}
}

// check_semantics_opt runs semantic checks, using worker threads for independent
// function bodies when requested and there is enough work.
pub fn (mut tc TypeChecker) check_semantics_opt(want_parallel bool) bool {
	if !want_parallel {
		tc.check_semantics()
		return false
	}
	$if windows {
		tc.check_semantics()
		return false
	} $else {
		return tc.check_semantics_parallel()
	}
}

fn (mut tc TypeChecker) check_semantics_parallel() bool {
	$if windows {
		tc.check_semantics()
		return false
	} $else {
		tc.resolution_type_mode = false
		// Freeze the warm post-collect type cache as the shared read-only base
		// for every worker thread and the master itself via a private overlay.
		tc.install_type_cache_overlay()
		// Invalid-IError-return diagnostics are gated to functions reachable
		// from the selected files. Most successful compiles never produce a
		// candidate, so defer the call-graph walk until after checking and only
		// run it when there is something to filter.
		tc.defer_ierror_gating = tc.diagnostic_files.len > 0
		tc.selected_file_called_fns = map[string]bool{}
		tc.selected_file_worklist = []string{}
		tc.check_export_attrs()
		items := tc.collect_parallel_check_items()
		final_file := tc.cur_file
		final_module := tc.cur_module
		was_parallel := tc.run_parallel_check(items)
		tc.cur_file = final_file
		tc.cur_module = final_module
		if tc.defer_ierror_gating {
			if tc.pending_ierror_errors.len > 0 {
				tc.collect_selected_file_called_fns()
			}
			if tc.filter_pending_ierror_errors() {
				tc.sort_parallel_check_errors()
			}
			tc.defer_ierror_gating = false
		}
		tc.restore_type_cache_base()
		// Match the serial checker: only generated post-check type text may use the
		// cross-module generic-argument fallback.
		tc.resolution_type_mode = true
		return was_parallel
	}
}

fn (mut tc TypeChecker) filter_pending_ierror_errors() bool {
	mut added := false
	for p in tc.pending_ierror_errors {
		if p.fn_qname in tc.selected_file_called_fns {
			tc.errors << p.err
			added = true
		}
	}
	tc.pending_ierror_errors = []PendingIerrorError{}
	return added
}

fn (mut tc TypeChecker) collect_parallel_check_items() []CheckWorkItem {
	tc.cur_module = ''
	tc.cur_file = ''
	mut items := []CheckWorkItem{}
	// Fn subtrees are contiguous: the fn_decl at index i owns exactly the node
	// range (previous top-level node, i], so the span doubles as the cost
	// estimate (replacing a full subtree walk per fn).
	mut prev_tl := -1
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.struct_decl {
				tc.check_decl_type_strings(flat.NodeId(i), node)
				tc.check_struct_field_defaults(node)
			}
			.type_decl, .interface_decl {
				tc.check_decl_type_strings(flat.NodeId(i), node)
			}
			.enum_decl {
				tc.check_enum_field_values(node)
			}
			.const_decl {
				tc.check_const_field_values(node)
			}
			.fn_decl {
				tc.check_decl_type_strings(flat.NodeId(i), node)
				cost := i - prev_tl
				items << CheckWorkItem{
					fn_idx:   i
					range_lo: prev_tl + 1
					file:     tc.cur_file
					module:   tc.cur_module
					cost:     cost
					rank:     i64(cost) * 1_000_000_000 - i64(i)
				}
			}
			.c_fn_decl {
				if tc.reject_unsupported_generics {
					tc.check_decl_type_strings(flat.NodeId(i), node)
				}
			}
			else {}
		}

		prev_tl = i
	}
	return items
}

fn (mut tc TypeChecker) run_parallel_check(items []CheckWorkItem) bool {
	$if windows {
		tc.check_fn_items_serial(items)
		return false
	} $else {
		n_jobs := check_job_count(runtime.nr_jobs(), items.len)
		if items.len < min_parallel_check_items || n_jobs <= 1 {
			tc.check_fn_items_serial(items)
			return false
		}
		mut chunks := split_check_items(items, n_jobs)
		chunk_count := chunks.len
		thread_count := chunk_count - 1
		mut workers := []voidptr{cap: thread_count}
		for _ in 0 .. thread_count {
			w := tc.fork_for_parallel_check()
			workers << voidptr(w)
		}
		mut args := []CheckChunkArgs{cap: thread_count}
		for ci in 0 .. thread_count {
			args << CheckChunkArgs{
				worker:    workers[ci]
				items_ptr: unsafe { voidptr(&chunks[ci + 1]) }
			}
		}

		mut thread_ids := []C.pthread_t{len: thread_count}
		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		for ci in 0 .. thread_count {
			C.pthread_create(unsafe { &thread_ids[ci] }, attr, check_chunk_thread,
				unsafe { voidptr(&args[ci]) })
		}
		C.pthread_attr_destroy(attr)
		// The master checks its own chunk under the same range discipline as the
		// workers: in-range cache writes go straight into the shared arrays (the
		// master owns those slots), out-of-range ones into its sparse maps, which
		// are replayed first after join so that worker merges overwrite them in
		// the same order the old serial flow did.
		tc.parallel_check_sparse = true
		tc.check_fn_items_serial(chunks[0])
		for ci in 0 .. thread_count {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}
		tc.merge_own_sparse_caches()
		tc.parallel_check_sparse = false
		for ci in 0 .. thread_count {
			mut w := unsafe { &TypeChecker(workers[ci]) }
			tc.merge_parallel_check_worker(w)
			w.free_parallel_check_worker_cache()
		}
		tc.sort_parallel_check_errors()
		return true
	}
}

fn (mut tc TypeChecker) sort_parallel_check_errors() {
	tc.errors.sort_with_compare(compare_type_errors)
	if tc.errors.len < 2 {
		return
	}
	mut deduped := []TypeError{cap: tc.errors.len}
	for err in tc.errors {
		if deduped.len > 0 && type_errors_equal(deduped[deduped.len - 1], err) {
			continue
		}
		deduped << err
	}
	tc.errors = deduped
}

fn compare_type_errors(a &TypeError, b &TypeError) int {
	if a.node != b.node {
		return int(a.node) - int(b.node)
	}
	if a.kind != b.kind {
		return int(a.kind) - int(b.kind)
	}
	if a.msg < b.msg {
		return -1
	}
	if a.msg > b.msg {
		return 1
	}
	return 0
}

fn type_errors_equal(a TypeError, b TypeError) bool {
	return a.node == b.node && a.kind == b.kind && a.msg == b.msg
}

fn check_job_count(n_runtime_jobs int, n_items int) int {
	if n_runtime_jobs <= 0 || n_items <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_parallel_check_jobs {
		n = max_parallel_check_jobs
	}
	if n > n_items {
		n = n_items
	}
	return n
}

fn split_check_items(items []CheckWorkItem, n int) [][]CheckWorkItem {
	mut buckets := [][]CheckWorkItem{len: n, init: []CheckWorkItem{}}
	mut loads := []i64{len: n}
	if n > 1 {
		// The master (bucket 0) runs on a busy performance core while several
		// workers inevitably land on efficiency cores (plus the called-fns
		// collector occupying another); measured per-unit it finishes its even
		// share early and idles in pthread_join. Give it a proportionally
		// heavier share by starting it with negative load.
		mut total := i64(0)
		for it in items {
			total += i64(it.cost) + 1
		}
		loads[0] = -total * check_master_bias_pct / i64(100 * n)
	}
	mut sorted := items.clone()
	sorted.sort(a.rank > b.rank)
	for it in sorted {
		mut best := 0
		for b in 1 .. n {
			if loads[b] < loads[best] {
				best = b
			}
		}
		buckets[best] << it
		loads[best] += i64(it.cost) + 1
	}
	for mut bucket in buckets {
		bucket.sort(a.fn_idx < b.fn_idx)
	}
	return buckets
}

// merge_own_sparse_caches replays the master's out-of-range cache writes
// (parked in its sparse maps while it checked its own chunk under the range
// discipline) into the shared node-indexed arrays, restoring the state the old
// serial flow produced with direct writes.
fn (mut tc TypeChecker) merge_own_sparse_caches() {
	for idx, name in tc.sparse_resolved_call_names {
		tc.resolved_call_names[idx] = name
		tc.resolved_call_set[idx] = true
	}
	for idx, name in tc.sparse_resolved_fn_values {
		tc.resolved_fn_value_names[idx] = name
		tc.resolved_fn_value_set[idx] = true
	}
	for idx, _ in tc.sparse_statement_nodes {
		tc.statement_nodes[idx] = true
	}
	for idx, typ in tc.sparse_expr_type_values {
		tc.expr_type_values[idx] = typ
		tc.expr_type_set[idx] = true
	}
	tc.sparse_resolved_call_names.clear()
	tc.sparse_resolved_fn_values.clear()
	tc.sparse_statement_nodes.clear()
	tc.sparse_expr_type_values.clear()
	tc.sparse_checking_nodes.clear()
}

fn (mut tc TypeChecker) check_fn_items_serial(items []CheckWorkItem) {
	for it in items {
		node := tc.a.nodes[it.fn_idx]
		tc.check_range_lo = it.range_lo
		tc.check_range_hi = it.fn_idx
		tc.check_fn_decl_semantics(it.fn_idx, node, it.file, it.module)
	}
	tc.check_range_lo = -1
	tc.check_range_hi = -1
}

fn (mut tc TypeChecker) check_fn_decl_semantics(fn_idx int, node flat.Node, file string, module_name string) {
	mut saved_mut_params := tc.cur_fn_mut_param_base_types.move()
	mut saved_mut_param_owners := tc.cur_fn_mut_param_binding_owners.move()
	mut saved_mut_local_owners := tc.cur_fn_mut_local_binding_owners.move()
	tc.cur_fn_mut_param_base_types = map[string]Type{}
	tc.cur_fn_mut_param_binding_owners = map[string]ScopeBindingOwner{}
	tc.cur_fn_mut_local_binding_owners = map[string]ScopeBindingOwner{}
	tc.cur_file = file
	tc.cur_module = module_name
	tc.cur_scope = tc.file_scope
	tc.cur_fn_ret_type = tc.parse_type(node.typ)
	tc.cur_fn_node_id = fn_idx
	tc.method_value_locals = map[string]bool{}
	tc.method_value_local_depth = map[string]int{}
	$if ownership ? {
		tc.ownership_begin_fn(node)
	}
	tc.push_scope()
	for pi in 0 .. node.children_count {
		p := tc.a.child_node(&node, pi)
		tc.insert_fn_param_binding(p)
	}
	tc.insert_implicit_veb_ctx(node)
	// Open generic declarations are checked when they are instantiated.  Walking every
	// template in a selected module diagnoses names that only exist after comptime
	// expansion (and even dead generic helpers), unlike the reference compiler.
	generic_params := tc.infer_decl_generic_params(node)
	qname := checker_qualified_fn_name(module_name, node.value)
	if generic_params.len == 0 || qname in tc.selected_file_called_fns {
		tc.check_fn_body(node)
	}
	tc.cur_fn_node_id = -1
	is_disabled_stub := node.value in tc.a.disabled_fns
	if tc.cur_fn_ret_type !is Unknown && !type_allows_implicit_return(tc.cur_fn_ret_type)
		&& !tc.fn_body_definitely_returns(node) && !is_disabled_stub
		&& tc.should_diagnose(flat.NodeId(fn_idx)) {
		tc.record_error(.return_mismatch,
			'missing return at end of function `${node.value}`; expected `${tc.cur_fn_ret_type.name()}`',
			flat.NodeId(fn_idx))
	}
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_fn()
	}
	tc.cur_fn_ret_type = Type(void_)
	tc.cur_fn_mut_param_base_types = saved_mut_params.move()
	tc.cur_fn_mut_param_binding_owners = saved_mut_param_owners.move()
	tc.cur_fn_mut_local_binding_owners = saved_mut_local_owners.move()
}

// prewarm_shared_type_cache forces the lazily-built type_cache indexes that
// fn-body checking commonly needs, so freezing the cache as the shared base
// hands every fork a complete index instead of each rebuilding its own.
fn (mut tc TypeChecker) prewarm_shared_type_cache() {
	if isnil(tc.type_cache) {
		return
	}
	_ = tc.local_fn_decl_exists('__v3_prewarm__')
	_ = tc.unique_qualified_type_name('__V3Prewarm__') or { '' }
	_ = tc.source_struct_has_non_builtin_error_embed('__V3Prewarm__', '', '')
}

// install_type_cache_overlay freezes the master's warm type cache as the
// shared read-only base for the parallel-check region and gives the master a
// private overlay, so its own writes cannot race worker reads of the base.
fn (mut tc TypeChecker) install_type_cache_overlay() {
	if isnil(tc.type_cache) {
		return
	}
	tc.prewarm_shared_type_cache()
	tc.type_cache = &TypeCache{
		base:          tc.type_cache
		parse_enabled: tc.type_cache.parse_enabled
	}
}

// restore_type_cache_base folds the master's private overlay back into the
// frozen base once every thread has joined, and reattaches the base as the
// live cache (post-check phases mutate and invalidate it in place).
fn (mut tc TypeChecker) restore_type_cache_base() {
	if isnil(tc.type_cache) {
		return
	}
	mut overlay := tc.type_cache
	if isnil(overlay.base) {
		return
	}
	mut base := overlay.base
	for k, v in overlay.parse_entries {
		base.parse_entries[k] = v
	}
	for k, v in overlay.c_entries {
		base.c_entries[k] = v
	}
	for k, v in overlay.struct_field_entries {
		base.struct_field_entries[k] = v
	}
	for k, v in overlay.struct_field_misses {
		base.struct_field_misses[k] = v
	}
	for k, v in overlay.ierror_compat_entries {
		base.ierror_compat_entries[k] = v
	}
	if overlay.source_error_embed_indexed && !base.source_error_embed_indexed {
		base.source_error_embed_entries = overlay.source_error_embed_entries.move()
		base.source_error_embed_indexed = true
	}
	if overlay.ierror_impl_names_set && !base.ierror_impl_names_set {
		base.ierror_impl_names = overlay.ierror_impl_names
		base.ierror_impl_names_set = true
	}
	if overlay.short_type_name_index_built && !base.short_type_name_index_built {
		base.short_type_name_index = overlay.short_type_name_index.move()
		base.short_type_name_index_built = true
	}
	if overlay.local_fn_decl_indexed_len > base.local_fn_decl_indexed_len {
		base.local_fn_decl_index = overlay.local_fn_decl_index.move()
		base.local_fn_decl_indexed_len = overlay.local_fn_decl_indexed_len
		base.local_fn_decl_last_module = overlay.local_fn_decl_last_module
	}
	tc.type_cache = base
}

fn (tc &TypeChecker) fork_for_parallel_check() &TypeChecker {
	mut w := *tc
	w.file_scope = new_scope(tc.file_scope)
	w.cur_scope = w.file_scope
	w.scope_pool = []&Scope{}
	w.scope_pool_index = 0
	w.errors = []TypeError{}
	w.pending_ierror_errors = []PendingIerrorError{}
	// The node-indexed cache arrays are intentionally SHARED with the master
	// (the fork copies the slice headers): each work item owns the disjoint
	// node id range [range_lo, fn_idx], and while parallel_check_sparse is set
	// a checker touches the shared arrays only for ids inside its current
	// item's range — no other thread reads or writes those slots. Everything
	// out of range goes through the private sparse maps below and is merged
	// after join.
	w.parallel_check_sparse = true
	w.check_range_lo = -1
	w.check_range_hi = -1
	w.sparse_resolved_call_names = map[int]string{}
	w.sparse_resolved_fn_values = map[int]string{}
	w.sparse_statement_nodes = map[int]bool{}
	w.sparse_expr_type_values = map[int]Type{}
	w.sparse_checking_nodes = map[int]bool{}
	w.method_values_by_fn = map[int][]string{}
	w.method_value_locals = map[string]bool{}
	w.method_value_local_depth = map[string]int{}
	w.cur_fn_mut_param_base_types = map[string]Type{}
	w.cur_fn_mut_param_binding_owners = map[string]ScopeBindingOwner{}
	w.cur_fn_mut_local_binding_owners = map[string]ScopeBindingOwner{}
	w.generic_method_value_info = map[string]CallInfo{}
	w.smartcasts = map[string]Type{}
	w.cur_fn_ret_type = Type(void_)
	w.cur_fn_node_id = -1
	$if ownership ? {
		w.ownership_fork_for_parallel_check(tc)
	}
	w.type_cache = &TypeCache{
		// The master's frozen pre-region cache (the overlay's base) is shared
		// read-only across all forks; each fork writes to its own maps.
		base:                       if tc.type_cache != unsafe { nil } {
			tc.type_cache.base
		} else {
			&TypeCache(unsafe { nil })
		}
		parse_enabled:              if tc.type_cache != unsafe { nil } {
			tc.type_cache.parse_enabled
		} else {
			false
		}
		parse_entries:              map[string]Type{}
		c_entries:                  map[string]string{}
		struct_field_entries:       map[string]Type{}
		struct_field_misses:        map[string]bool{}
		ierror_compat_entries:      map[string]int{}
		source_error_embed_entries: map[string]int{}
	}
	return &w
}

fn (mut tc TypeChecker) merge_parallel_check_worker(w &TypeChecker) {
	tc.errors << w.errors
	tc.pending_ierror_errors << w.pending_ierror_errors
	$if ownership ? {
		tc.ownership_merge_parallel_check_worker(w)
	}
	for idx, name in w.sparse_resolved_call_names {
		tc.resolved_call_names[idx] = name
		tc.resolved_call_set[idx] = true
	}
	for idx, name in w.sparse_resolved_fn_values {
		tc.resolved_fn_value_names[idx] = name
		tc.resolved_fn_value_set[idx] = true
	}
	for idx, _ in w.sparse_statement_nodes {
		tc.statement_nodes[idx] = true
	}
	for idx, typ in w.sparse_expr_type_values {
		tc.expr_type_values[idx] = typ
		tc.expr_type_set[idx] = true
	}
	for fn_idx, values in w.method_values_by_fn {
		if values.len == 0 {
			continue
		}
		if fn_idx in tc.method_values_by_fn {
			tc.method_values_by_fn[fn_idx] << values
		} else {
			tc.method_values_by_fn[fn_idx] = values.clone()
		}
	}
	for key, info in w.generic_method_value_info {
		tc.generic_method_value_info[key] = info
	}
}

fn (mut tc TypeChecker) free_parallel_check_worker_cache() {
	unsafe {
		if !isnil(tc.type_cache) {
			mut cache := tc.type_cache
			cache.parse_entries.free()
			cache.c_entries.free()
			cache.struct_field_entries.free()
			cache.struct_field_misses.free()
			cache.ierror_compat_entries.free()
			cache.source_error_embed_entries.free()
		}
	}
}

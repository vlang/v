module types

import runtime
import v3.flat

const min_parallel_check_items = 256
const max_parallel_check_jobs = 8

struct CheckWorkItem {
	fn_idx int
	file   string
	module string
	cost   int
	rank   i64
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
	$if ownership ? {
		tc.check_semantics()
		return false
	}
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
	tc.check_export_attrs()
	items := tc.collect_parallel_check_items()
	return tc.run_parallel_check(items)
}

fn (mut tc TypeChecker) collect_parallel_check_items() []CheckWorkItem {
	tc.cur_module = ''
	tc.cur_file = ''
	mut items := []CheckWorkItem{}
	for i, node in tc.a.nodes {
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
				cost := tc.check_fn_subtree_cost(i)
				items << CheckWorkItem{
					fn_idx: i
					file:   tc.cur_file
					module: tc.cur_module
					cost:   cost
					rank:   i64(cost) * 1_000_000_000 - i64(i)
				}
			}
			.c_fn_decl {
				if tc.reject_unsupported_generics {
					tc.check_decl_type_strings(flat.NodeId(i), node)
				}
			}
			else {}
		}
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
		tc.check_fn_items_serial(chunks[0])
		for ci in 0 .. thread_count {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}
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
	tc.errors.sort(a.node < b.node)
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

fn (tc &TypeChecker) check_fn_subtree_cost(fn_idx int) int {
	mut count := 0
	mut stack := [flat.NodeId(fn_idx)]
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= tc.a.nodes.len {
			continue
		}
		node := tc.a.nodes[idx]
		count++
		for ci in 0 .. node.children_count {
			child_id := tc.a.children[node.children_start + ci]
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
	return count
}

fn (mut tc TypeChecker) check_fn_items_serial(items []CheckWorkItem) {
	for it in items {
		node := tc.a.nodes[it.fn_idx]
		tc.check_fn_decl_semantics(it.fn_idx, node, it.file, it.module)
	}
}

fn (mut tc TypeChecker) check_fn_decl_semantics(fn_idx int, node flat.Node, file string, module_name string) {
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
		if p.kind == .param && p.value.len > 0 {
			tc.cur_scope.insert(p.value, tc.parse_type(p.typ))
		}
	}
	tc.insert_implicit_veb_ctx(node)
	tc.check_fn_body(node)
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
}

fn (tc &TypeChecker) fork_for_parallel_check() &TypeChecker {
	mut w := *tc
	w.file_scope = new_scope(tc.file_scope)
	w.cur_scope = w.file_scope
	w.scope_pool = []&Scope{}
	w.scope_pool_index = 0
	w.errors = []TypeError{}
	w.resolved_call_names = []string{}
	w.resolved_call_set = []bool{}
	w.resolved_fn_value_names = []string{}
	w.resolved_fn_value_set = []bool{}
	w.statement_nodes = []bool{}
	w.expr_type_values = []Type{}
	w.expr_type_set = []bool{}
	w.checking_nodes = []bool{}
	w.parallel_check_sparse = true
	w.sparse_resolved_call_names = map[int]string{}
	w.sparse_resolved_fn_values = map[int]string{}
	w.sparse_statement_nodes = map[int]bool{}
	w.sparse_expr_type_values = map[int]Type{}
	w.sparse_checking_nodes = map[int]bool{}
	w.method_values_by_fn = map[int][]string{}
	w.method_value_locals = map[string]bool{}
	w.method_value_local_depth = map[string]int{}
	w.generic_method_value_info = map[string]CallInfo{}
	w.smartcasts = map[string]Type{}
	w.cur_fn_ret_type = Type(void_)
	w.cur_fn_node_id = -1
	w.type_cache = &TypeCache{
		parse_enabled:        if tc.type_cache != unsafe { nil } {
			tc.type_cache.parse_enabled
		} else {
			false
		}
		parse_entries:        map[string]Type{}
		c_entries:            map[string]string{}
		struct_field_entries: map[string]Type{}
		struct_field_misses:  map[string]bool{}
	}
	return &w
}

fn (mut tc TypeChecker) merge_parallel_check_worker(w &TypeChecker) {
	tc.errors << w.errors
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
		}
	}
}

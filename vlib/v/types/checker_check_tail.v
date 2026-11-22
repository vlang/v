module types

// CheckTailPrepared holds what a parallel check works out, while its pool checks
// the bodies, for the scans of every node that follow the check on one thread:
// warn_unhandled_result_calls and check_unused_import_diagnostics. Each scan
// takes it only while the AST is the one it was made for, and scans as before
// otherwise. What it lists lives in arenas of its own, released with it.
@[heap]
struct CheckTailPrepared {
mut:
	scope     voidptr
	nodes_len int
	// result_calls has a bit for every node that can be a call that returns a
	// Result (see call_may_return_result): the calls that the workers found in
	// the bodies, the calls outside the bodies, and every node whose cached type
	// or resolved name was written after its worker looked at it.
	result_calls       []u64
	result_calls_ready bool
	fn_ret_types_len   int
	// The nodes of the files with an import to check (see
	// unused_import_nodes_by_file), found for `import_file_ids`.
	import_nodes_ready bool
	import_scope       voidptr
	import_file_ids    []int
	import_nodes       map[int][]int
}

// new_check_tail_prepared starts the preparation for the scans after a check of
// the current AST.
fn (tc &TypeChecker) new_check_tail_prepared() &CheckTailPrepared {
	// The header stays valid for a copy of the checker that still points to it
	// after the release; only what it lists is in the arena.
	mut tail := &CheckTailPrepared{
		nodes_len:        tc.a.nodes.len
		fn_ret_types_len: tc.fn_ret_types.len
	}
	scope := check_worker_scope_begin(tc.scope_parallel_check_workers)
	tail.result_calls = []u64{len: (tc.a.nodes.len + 63) / 64}
	check_worker_scope_leave(scope)
	tail.scope = scope
	return tail
}

// new_check_tail_prepared_outside starts the preparation like
// new_check_tail_prepared, with its header outside `scope`, the arena that is
// current: the checker points to the header until the next check, long after
// the caller has released that arena. It returns nil when the arena cannot be
// left, and the scans after the check then work as they do without a pool.
fn (tc &TypeChecker) new_check_tail_prepared_outside(scope voidptr) &CheckTailPrepared {
	$if prealloc {
		if scope != unsafe { nil } {
			state := unsafe { prealloc_scope_suspend(scope) }
			if state == unsafe { nil } {
				return unsafe { nil }
			}
			tail := tc.new_check_tail_prepared()
			unsafe { prealloc_scope_resume(scope, state) }
			return tail
		}
	}
	return tc.new_check_tail_prepared()
}

// release_check_tail drops what a check prepared for the scans after it.
fn (mut tc TypeChecker) release_check_tail() {
	mut tail := tc.check_tail
	if isnil(tail) {
		return
	}
	tc.check_tail = unsafe { nil }
	// Nothing is read or marked through a pointer that is still held.
	tail.nodes_len = 0
	tail.result_calls_ready = false
	tail.import_nodes_ready = false
	tail.result_calls = []u64{}
	tail.import_file_ids = []int{}
	tail.import_nodes = map[int][]int{}
	check_worker_scope_free(tail.import_scope)
	check_worker_scope_free(tail.scope)
	tail.import_scope = unsafe { nil }
	tail.scope = unsafe { nil }
}

@[direct_array_access; inline]
fn (mut tail CheckTailPrepared) mark_result_call(idx int) {
	if idx >= 0 && idx < tail.nodes_len {
		tail.result_calls[idx >> 6] |= u64(1) << u64(idx & 63)
	}
}

// note_check_tail_write records that the cached type or the resolved name of
// the node `idx` was written on the checker that prepares the scans after its
// check, so that the scan for unhandled Results looks at the node again.
@[inline]
fn (tc &TypeChecker) note_check_tail_write(idx int) {
	mut tail := tc.check_tail
	if !isnil(tail) {
		tail.mark_result_call(idx)
	}
}

// call_may_return_result reports whether the call `idx` can have a Result as its
// type (see call_expr_type), from the node caches as they are now and without
// parsing a type: `false` only when the caches give it another type, or none.
// A worker of a parallel check asks for the calls of a body it has just checked,
// whose cache slots only it has written.
@[direct_array_access]
fn (tc &TypeChecker) call_may_return_result(idx int) bool {
	node := &tc.a.nodes[idx]
	if node.children_count == 3 && tc.c_va_macro_call_name(*node) == 'C.va_arg' {
		return true
	}
	if idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
		typ := tc.expr_type_values[idx]
		if typ !is Void {
			return unalias_type(typ) is ResultType
		}
	}
	if idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
		if ret := tc.fn_ret_types[tc.resolved_call_names[idx].value] {
			return unalias_type(ret) is ResultType
		}
	}
	// A type the call is annotated with is parsed; else it has `void` or no type.
	return node.typ != 0 && !call_type_annotation_is_placeholder(node.typ)
}

// CheckTailScanArgs is the task of a parallel check that prepares, on the
// calling thread and beside the pool, what does not depend on the bodies.
struct CheckTailScanArgs {
	tc    voidptr // &TypeChecker, which the task only reads
	items voidptr // &[]CheckWorkItem, the bodies the pool checks
mut:
	tail &CheckTailPrepared = unsafe { nil }
}

fn check_tail_scan_thread(arg voidptr) voidptr {
	mut a := unsafe { &CheckTailScanArgs(arg) }
	tc := unsafe { &TypeChecker(a.tc) }
	items := unsafe { &[]CheckWorkItem(a.items) }
	mut tail := a.tail
	tail.result_calls_ready = tc.mark_calls_outside_check_items(*items, mut tail)
	scope := check_worker_scope_begin(tc.scope_parallel_check_workers)
	tail.import_file_ids = tc.unused_import_file_ids()
	tail.import_nodes = tc.nodes_of_files(tail.import_file_ids)
	check_worker_scope_leave(scope)
	tail.import_scope = scope
	tail.import_nodes_ready = true
	return unsafe { nil }
}

// mark_calls_outside_check_items marks the calls that are in none of the bodies
// `items`, which no worker looks at. It returns false when the bodies are not in
// node order, as the gaps between them are then not known.
@[direct_array_access]
fn (tc &TypeChecker) mark_calls_outside_check_items(items []CheckWorkItem, mut tail CheckTailPrepared) bool {
	mut next := tc.a.user_code_start
	mut last := -1
	for item in items {
		if item.range_lo <= last || item.fn_idx < item.range_lo {
			return false
		}
		last = item.fn_idx
		for idx in next .. int_min(item.range_lo, tail.nodes_len) {
			if tc.a.nodes[idx].kind == .call {
				tail.mark_result_call(idx)
			}
		}
		if item.fn_idx + 1 > next {
			next = item.fn_idx + 1
		}
	}
	for idx in next .. tail.nodes_len {
		if tc.a.nodes[idx].kind == .call {
			tail.mark_result_call(idx)
		}
	}
	return true
}

// prepared_result_calls returns what the check prepared for the scan for
// unhandled Results, when it did for the AST as it is now, or nil.
fn (tc &TypeChecker) prepared_result_calls() &CheckTailPrepared {
	tail := tc.check_tail
	if isnil(tail) || !tail.result_calls_ready || tail.nodes_len != tc.a.nodes.len
		|| tail.fn_ret_types_len != tc.fn_ret_types.len || !isnil(tc.incremental) {
		return unsafe { nil }
	}
	return tail
}

// prepared_unused_import_nodes returns the nodes of the files `file_ids`, and
// true, when the check listed them for exactly those files of the AST as it is
// now.
fn (tc &TypeChecker) prepared_unused_import_nodes(file_ids []int) (map[int][]int, bool) {
	tail := tc.check_tail
	if isnil(tail) || !tail.import_nodes_ready || tail.nodes_len != tc.a.nodes.len
		|| tail.import_file_ids != file_ids {
		return map[int][]int{}, false
	}
	return unsafe { tail.import_nodes }, true
}

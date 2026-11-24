module types

import v.flat

// A direct, immutable handle binding immediately followed by a lexical defer joins
// before that binding's scope can exit. The receiver identifier is the handle's
// only permitted use. A function-wide defer would allow the borrowed storage to
// leave scope first.
fn (tc &TypeChecker) ownership_scoped_thread_join_site(spawn_id flat.NodeId) ?flat.NodeId {
	spawn_node := tc.a.node(spawn_id)
	if spawn_node.value != 'spawn' {
		return none
	}
	mut binding_id := tc.direct_parent_id(spawn_id)
	for tc.valid_node_id(binding_id) && tc.a.node(binding_id).kind == .paren {
		binding_id = tc.direct_parent_id(binding_id)
	}
	binding := tc.a.node(binding_id)
	if binding.kind != .decl_assign || binding.children_count != 2 || binding.value !in ['', '1'] || binding.is_mut {
		return none
	}
	handle := tc.a.child_node(binding, 0)
	if handle.kind != .ident || handle.is_mut || handle.value == '_' {
		return none
	}
	scope := tc.a.node(tc.direct_parent_id(binding_id))
	if scope.kind !in [.block, .fn_decl, .fn_literal, .for_stmt, .for_in_stmt, .match_branch,
		.select_branch]
		|| scope.children_count < 2 {
		return none
	}
	for i in 0 .. scope.children_count - 1 {
		if tc.a.child(scope, i) == binding_id {
			return tc.ownership_scoped_thread_defer_receiver(tc.a.child(scope, i + 1), handle.value)
		}
	}
	return none
}

fn (tc &TypeChecker) ownership_scoped_thread_defer_receiver(id flat.NodeId, name string) ?flat.NodeId {
	stmt := tc.a.node(id)
	if stmt.kind != .defer_stmt || stmt.value == 'function' || stmt.children_count != 1 {
		return none
	}
	block := tc.a.child_node(stmt, 0)
	if block.kind != .block || block.children_count != 1 {
		return none
	}
	call := tc.a.node(tc.ownership_unwrap_expr(tc.a.child(block, 0)))
	if call.kind != .call || call.children_count != 1 {
		return none
	}
	callee := tc.a.child_node(call, 0)
	if callee.kind != .selector || callee.value != 'wait' || callee.children_count != 1 {
		return none
	}
	receiver_id := tc.a.child(callee, 0)
	receiver := tc.a.node(receiver_id)
	if receiver.kind == .ident && receiver.value == name {
		return receiver_id
	}
	return none
}

// Only a direct address of a local immutable scalar establishes a loan here.
// Compound values can contain aliases whose lifetime and synchronization must
// also be proved before their references can be passed to workers.
fn (tc &TypeChecker) ownership_scoped_thread_source(id flat.NodeId) ?string {
	id0 := tc.ownership_unwrap_expr(id)
	expr := tc.a.node(id0)
	if expr.kind != .prefix || expr.op != .amp || expr.children_count != 1 {
		return none
	}
	source_id := tc.ownership_unwrap_expr(tc.a.child(expr, 0))
	source := tc.a.node(source_id)
	if source.kind != .ident || tc.ident_is_mutable_lvalue(source.value) {
		return none
	}
	owner := tc.cur_scope.lookup_owner(source.value) or { return none }
	if tc.binding_owner_is_global(owner) {
		return none
	}
	typ := unalias_type(tc.resolve_type(source_id))
	if typ is Primitive || typ is Enum || typ is Rune || typ is Char || typ is ISize || typ is USize {
		return source.value
	}
	return none
}

fn (mut tc TypeChecker) ownership_record_scoped_thread_borrows(handle string, rhs_id flat.NodeId) {
	spawn_id := tc.ownership_unwrap_expr(rhs_id)
	spawn_node := tc.a.node(spawn_id)
	if spawn_node.kind != .spawn_expr || spawn_node.children_count != 1
		|| !tc.ownership_checks_thread_boundary() {
		return
	}
	join_site := tc.ownership_scoped_thread_join_site(spawn_id) or { return }
	call_id := tc.a.child(spawn_node, 0)
	call := tc.a.node(call_id)
	if call.kind != .call {
		return
	}
	info := tc.resolve_call_info(call_id, *call) or { return }
	if recv := tc.ownership_thread_receiver(tc.a.child(call, 0), info) {
		expected := info.params[0] or { tc.resolve_type(recv) }
		tc.ownership_record_scoped_thread_borrow(recv, info, 0, expected, handle, join_site)
	}
	for i in 1 .. call.children_count {
		value_id := tc.call_arg_value(tc.a.child(call, i))
		arg := tc.spread_arg_value(value_id) or { value_id }
		param_idx := tc.ownership_call_arg_decl_param_idx(info, i) +
			tc.ownership_call_arg_shift(*call, info)
		elem_idx := tc.ownership_call_arg_variadic_elem_idx(info, param_idx, value_id)
		expected := tc.ownership_call_arg_expected_type(info, param_idx, elem_idx)
		tc.ownership_record_scoped_thread_borrow(arg, info, param_idx, expected, handle, join_site)
	}
}

fn (mut tc TypeChecker) ownership_record_scoped_thread_borrow(arg flat.NodeId, info CallInfo, param_idx int, expected Type, handle string, join_site flat.NodeId) {
	if unalias_type(expected) !is Pointer || tc.call_param_is_mut(info, param_idx) {
		return
	}
	source := tc.ownership_scoped_thread_source(arg) or { return }
	mut state := tc.ownership_state()
	if borrows := state.borrowed_vars[source] {
		for borrow in borrows {
			if borrow.thread_join_site == join_site && borrow.borrower == handle {
				return
			}
		}
	}
	state.scoped_thread_loans_seen = true
	tc.ownership_add_borrow_info(source, BorrowInfo{
		borrower:         handle
		pos:              arg
		// Even an immutable pointer can be used by an atomic API. Reserve
		// the scalar for the worker until its defer has joined.
		is_mut:           true
		thread_join_site: join_site
	})
}

fn (mut tc TypeChecker) ownership_reject_scoped_thread_access(name string, id flat.NodeId) bool {
	state := tc.ownership_state()
	if !state.scoped_thread_loans_seen {
		return false
	}
	for source, borrows in state.borrowed_vars {
		for borrow in borrows {
			if !tc.valid_node_id(borrow.thread_join_site) {
				continue
			}
			if source == name {
				tc.record_error(.thread_transfer,
					'cannot use `${name}` until scoped thread `${borrow.borrower}` has joined', id)
				return true
			}
			if (borrow.borrower == name || ownership_storage_key_is_descendant(name, borrow.borrower))
				&& id != borrow.thread_join_site && id != tc.direct_parent_id(borrow.thread_join_site) {
				tc.record_error(.thread_transfer,
					'cannot use scoped thread handle `${borrow.borrower}` outside its deferred wait', id)
				return true
			}
		}
	}
	return false
}

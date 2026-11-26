module types

import v.flat

fn (tc &TypeChecker) ownership_is_scoped_callback(typ Type) bool {
	clean := unalias_type(typ)
	return clean is Struct && tc.ownership_thread_library_type(clean.name_id, 'sync/thread', [
		'sync.thread.ScopedMutCallback',
		'thread.ScopedMutCallback',
	])
}

// Only declarations in sync.thread can supply the worker and boxing guarantees.
fn (tc &TypeChecker) ownership_scoped_callback_function(name string, member string) bool {
	if name != 'sync.thread.${member}' && name != 'thread.${member}' {
		return false
	}
	module_name := tc.fn_type_modules[name] or { return false }
	decl := tc.visible_mutation_fn_decl(name, module_name) or { return false }
	return tc.ownership_thread_library_declaration(*tc.a.node(flat.NodeId(decl.idx)),
		'sync/thread')
}

fn (tc &TypeChecker) ownership_scoped_callback_call_is(id flat.NodeId, member string) bool {
	call := tc.a.node(id)
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	mut callee := tc.a.child_node(call, 0)
	if callee.kind == .index && callee.children_count > 1 && callee.value != 'range' {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind == .ident && tc.name_has_local_binding(callee.value) {
		return false
	}
	name := tc.generic_call_base_name(*callee) or { return false }
	return tc.ownership_scoped_callback_function(name, member)
}

fn (tc &TypeChecker) ownership_scoped_callback_constructor(id flat.NodeId) ?flat.NodeId {
	value_id := tc.ownership_unwrap_expr(id)
	value := tc.a.node(value_id)
	if value.kind != .ident {
		return value_id
	}
	call := tc.ownership_scoped_thread_use_parent(value_id)
	spawn_id := tc.ownership_scoped_thread_use_parent(call)
	mut stmt_id := tc.ownership_scoped_thread_use_parent(spawn_id)
	parent_id := tc.direct_parent_id(stmt_id)
	if tc.a.node(parent_id).kind == .expr_stmt {
		stmt_id = parent_id
	}
	scope := tc.a.node(tc.direct_parent_id(stmt_id))
	for i in 1 .. scope.children_count {
		if tc.a.child(scope, i) != stmt_id {
			continue
		}
		// The adjacent immutable declaration fixes the constructor and the
		// referents in its initializer at the point of the move.
		return tc.ownership_scoped_callback_binding_initializer(tc.a.child(scope, i - 1),
			value.value)
	}
	return none
}

fn (tc &TypeChecker) ownership_scoped_callback_binding_initializer(decl_id flat.NodeId, name string) ?flat.NodeId {
	decl := tc.a.node(decl_id)
	if decl.kind != .decl_assign || decl.children_count != 2 || decl.is_mut {
		return none
	}
	binding := tc.a.child_node(decl, 0)
	if binding.kind == .ident && binding.value == name && !binding.is_mut {
		return tc.ownership_unwrap_expr(tc.a.child(decl, 1))
	}
	return none
}

// The referent is checked while the box is being built, before its local
// binding exists. The consuming invocation in the next statement checks the
// payload and replaces the box's loan with its own scoped reservation.
fn (tc &TypeChecker) ownership_scoped_callback_binding_reborrows(decl_id flat.NodeId) bool {
	decl := tc.a.node(decl_id)
	if decl.kind != .decl_assign || decl.children_count != 2 {
		return false
	}
	binding := tc.a.child_node(decl, 0)
	constructor_id := tc.ownership_scoped_callback_binding_initializer(decl_id, binding.value) or {
		return false
	}
	if !tc.ownership_scoped_callback_call_is(constructor_id, 'new_scoped_mut_callback') {
		return false
	}
	scope := tc.a.node(tc.direct_parent_id(decl_id))
	for i in 1 .. scope.children_count {
		if tc.a.child(scope, i - 1) != decl_id {
			continue
		}
		stmt := tc.a.node(tc.ownership_unwrap_expr(tc.a.child(scope, i)))
		if stmt.children_count != 2
			|| (stmt.kind != .decl_assign && !(stmt.kind == .infix && stmt.op == .left_shift)) {
			return false
		}
		spawn_id := tc.ownership_unwrap_expr(tc.a.child(stmt, 1))
		spawn_node := tc.a.node(spawn_id)
		if spawn_node.kind != .spawn_expr || spawn_node.children_count != 1 {
			return false
		}
		runner_id := tc.ownership_unwrap_expr(tc.a.child(spawn_node, 0))
		runner := tc.a.node(runner_id)
		if runner.children_count != 3
			|| !tc.ownership_scoped_callback_call_is(runner_id, 'invoke_scoped_mut_callback') {
			return false
		}
		arg := tc.a.node(tc.ownership_unwrap_expr(tc.a.child(runner, 1)))
		return arg.kind == .ident && arg.value == binding.value
			&& (tc.ownership_scoped_thread_join_site(spawn_id) != none
				|| tc.ownership_scoped_thread_group(spawn_id) != none)
	}
	return false
}

// The library worker invokes the callback once and drops it before returning.
// The inline concrete state gives the checker the complete set of payload loans.
fn (tc &TypeChecker) ownership_scoped_callback_borrows(id flat.NodeId, expected Type, info CallInfo, index int) ?[]ScopedThreadBorrow {
	if index != 0 || !tc.ownership_scoped_callback_function(info.name, 'invoke_scoped_mut_callback')
		|| !semantic_types_equal(unalias_type(tc.resolve_type(id)), unalias_type(expected)) {
		return none
	}
	constructor_id := tc.ownership_scoped_callback_constructor(id) or { return none }
	constructor := tc.a.node(constructor_id)
	if constructor.children_count != 2
		|| !tc.ownership_scoped_callback_call_is(constructor_id, 'new_scoped_mut_callback') {
		return none
	}
	state_id := tc.call_arg_value(tc.a.child(constructor, 1))
	state_type := tc.resolve_type(state_id)
	// Alias methods can dispatch independently of the methods on the underlying struct.
	if state_type !is Struct {
		return none
	}
	borrows := tc.ownership_scoped_thread_argument_borrows(state_id, state_type, true) or { return none }
	if !tc.ownership_scoped_callback_method(state_type, 'call', borrows)
		|| (tc.ownership_type_has_explicit_drop(state_type.name_id)
			&& !tc.ownership_scoped_callback_method(state_type, 'drop', borrows)) {
		return none
	}
	return borrows
}

fn (tc &TypeChecker) ownership_scoped_callback_method(state_type Type, member string, borrows []ScopedThreadBorrow) bool {
	mut checked := false
	for name in receiver_method_name_candidates(state_type, member, tc.cur_module) {
		if name !in tc.fn_ret_types || !tc.method_can_be_called_on_receiver(state_type, member, name) {
			continue
		}
		method := tc.call_info(name, true)
		receiver := method.params[0] or { return false }
		if unalias_type(receiver) !is Pointer || !tc.call_param_is_mut(method, 0)
			|| !tc.ownership_scoped_thread_parameter_does_not_escape(method, 0, state_type, borrows) {
			return false
		}
		checked = true
	}
	return checked
}

fn (tc &TypeChecker) ownership_scoped_callback_move(id flat.NodeId, expected Type, info CallInfo, index int) bool {
	if !tc.ownership_is_scoped_callback(expected) {
		return false
	}
	call := tc.ownership_scoped_thread_use_parent(tc.ownership_unwrap_expr(id))
	spawn_id := tc.ownership_scoped_thread_use_parent(call)
	return (tc.ownership_scoped_thread_join_site(spawn_id) != none
		|| tc.ownership_scoped_thread_group(spawn_id) != none)
		&& tc.ownership_scoped_callback_borrows(id, expected, info, index) != none
}

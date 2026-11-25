module types

import v.flat

struct ScopedThreadGroup {
	name      string
	owner     ScopeBindingOwner
	wait_site flat.NodeId
}

fn (tc &TypeChecker) ownership_scoped_thread_group(spawn_id flat.NodeId) ?ScopedThreadGroup {
	if tc.a.node(spawn_id).kind != .spawn_expr || tc.a.node(spawn_id).value != 'spawn' {
		return none
	}
	append_id := tc.ownership_scoped_thread_use_parent(spawn_id)
	append := tc.a.node(append_id)
	if append.kind != .infix || append.op != .left_shift || append.children_count != 2 {
		return none
	}
	group_id := tc.ownership_unwrap_expr(tc.a.child(append, 0))
	group := tc.a.node(group_id)
	if group.kind != .ident || !tc.ownership_scoped_thread_group_append(group_id) {
		return none
	}
	owner := tc.cur_scope.lookup_owner(group.value) or { return none }
	if tc.binding_owner_is_global(owner) {
		return none
	}
	ary := array_type_from_receiver(tc.resolve_type(group_id)) or { return none }
	result := tc.thread_wait_return_type(ary.elem_type) or { return none }
	// A plain scalar or void wait always joins the whole array.
	if result !is Void && !ownership_scoped_thread_is_scalar(unalias_type(result)) {
		return none
	}
	mut child_id := append_id
	for {
		scope_id := tc.direct_parent_id(child_id)
		if !tc.valid_node_id(scope_id) {
			return none
		}
		scope := tc.a.node(scope_id)
		if wait_site := tc.ownership_scoped_thread_group_declaration(scope, child_id, group.value) {
			return ScopedThreadGroup{ name: group.value, owner: owner, wait_site: wait_site }
		}
		if scope.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return none
		}
		child_id = scope_id
	}
}

fn (tc &TypeChecker) ownership_scoped_thread_group_declaration(scope &flat.Node, child_id flat.NodeId, name string) ?flat.NodeId {
	if scope.kind !in [.block, .fn_decl, .fn_literal, .for_stmt, .for_in_stmt, .match_branch,
		.select_branch] || scope.children_count < 2 {
		return none
	}
	for i in 0 .. scope.children_count - 1 {
		binding_id := tc.a.child(scope, i)
		if binding_id == child_id {
			break
		}
		binding := tc.a.node(binding_id)
		if binding.kind != .decl_assign || binding.children_count != 2 {
			continue
		}
		lhs_id := tc.a.child(binding, 0)
		lhs := tc.a.node(lhs_id)
		if lhs.kind != .ident || lhs.value != name {
			continue
		}
		initializer := tc.a.child_node(binding, 1)
		if (!lhs.is_mut && !binding.is_mut) || initializer.kind != .array_init
			|| initializer.children_count != 0 {
			return none
		}
		wait_site := tc.ownership_scoped_thread_defer_receiver(tc.a.child(scope, i + 1), name) or {
			return none
		}
		for index in 0 .. scope.children_count {
			if !tc.ownership_scoped_thread_group_use(tc.a.child(scope, index), name, lhs_id, wait_site, false, 0) {
				return none
			}
		}
		return wait_site
	}
	return none
}

fn (tc &TypeChecker) ownership_scoped_thread_group_use(id flat.NodeId, name string, binding_id flat.NodeId, wait_site flat.NodeId, delayed bool, depth int) bool {
	if depth > 128 {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.goto_stmt, .asm_stmt] {
		return false
	}
	if node.kind in [.ident, .param] && node.value == name {
		return id == binding_id || id == wait_site
			|| (!delayed && tc.ownership_scoped_thread_group_append(id))
	}
	nested := delayed || node.kind in [.fn_literal, .lambda_expr, .defer_stmt]
	for i in 0 .. node.children_count {
		if !tc.ownership_scoped_thread_group_use(tc.a.child(node, i), name, binding_id, wait_site, nested, depth + 1) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) ownership_scoped_thread_group_append(id flat.NodeId) bool {
	node := tc.a.node(tc.ownership_scoped_thread_use_parent(id))
	if node.kind != .infix || node.op != .left_shift || node.children_count != 2
		|| tc.ownership_unwrap_expr(tc.a.child(node, 0)) != id {
		return false
	}
	rhs := tc.a.node(tc.ownership_unwrap_expr(tc.a.child(node, 1)))
	return rhs.kind == .spawn_expr && rhs.value == 'spawn'
}

fn (tc &TypeChecker) ownership_scoped_thread_group_source_outlives(name string, group ScopedThreadGroup) bool {
	source := tc.cur_scope.lookup_owner(name) or { return false }
	return source.belongs_to_scope_chain_until(group.owner.scope, tc.file_scope)
}

module transform

import v.flat
import v.types

fn test_guard_staging_ends_error_scope_before_pointer_backed_destination() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['IError'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('err', '&string')
	t.heaped_amp_locals['err'] = true
	tail := GuardReturnTail{
		assign_lhs_ids: [t.make_ident('err'), t.make_ident('_')]
	}
	staging := t.stage_guard_assignments(tail, true)
	assert t.lvalue_type(staging.tail.assign_lhs_ids[0]) == 'string'
	assert staging.tail.assign_lhs_ids[1] == tail.assign_lhs_ids[1]
	assert staging.tail.assign_original_lhs_ids == tail.assign_lhs_ids
	// A nested guard writes the same staging storage, rather than adding another copy.
	nested := t.stage_guard_assignments(staging.tail, true)
	assert nested.decls.len == 0
	assert nested.tail.assign_lhs_ids == staging.tail.assign_lhs_ids
	mut body := []flat.NodeId{}
	err_scope := t.enter_implicit_err_scope(true)
	t.append_implicit_err_decl(mut body, t.make_struct_init('IError'))
	t.leave_implicit_err_scope(err_scope)
	wrapper_id := t.finish_guard_assignments(body, tail, staging)
	wrapper := a.nodes[int(wrapper_id)]
	assert wrapper.value == skip_scope_drops_block_value
	assert guard_staging_scope_count(&a, wrapper_id) == 1
	scope := a.child_node(&wrapper, staging.decls.len)
	assert scope.kind == .block && scope.value != skip_scope_drops_block_value
	assignment := a.child_node(&wrapper, wrapper.children_count - 1)
	assert assignment.kind == .assign && assignment.skip_ownership_drops()
	lvalue := a.child_node(assignment, 0)
	assert lvalue.kind == .prefix && lvalue.op == .mul
	assert a.child_node(lvalue, 0).value == 'err'
	assert a.child(assignment, 1) == staging.tail.assign_lhs_ids[0]
}

fn test_owned_guard_staging_preserves_moves_through_branch_tails() {
	$if !ownership ? {
		return
	}
	for tail_kind in ['tuple', 'nested', 'call'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.collect(tc.a)
		tc.interface_names['IError'] = true
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.set_var_type('err', 'IError')
		t.set_var_type('value', 'string')
		tail := GuardReturnTail{
			assign_lhs_ids: [t.make_ident('err'), t.make_ident('value')]
		}
		staging := t.stage_guard_assignments(tail, true)
		assert staging.tail.assign_move_flags.len == 2
		assert staging.tail.assign_move_flags[0] != ''
		assert staging.tail.assign_move_flags[1] != ''
		err_scope := t.enter_implicit_err_scope(true)
		values := [t.make_ident('err'), t.make_ident('value')]
		// The checker sees equal storage spellings; the guard must distinguish bindings.
		assert tc.ownership_expr_moves_storage(values[0], tail.assign_lhs_ids[0])
		assert !t.multi_if_assign_target_was_moved(staging.tail, 0, values)
		assert t.multi_if_assign_target_was_moved(staging.tail, 1, values)
		mut branch := flat.empty_node
		if tail_kind == 'call' {
			tc.fn_param_types['pair'] = [tc.parse_type('IError'), types.Type(types.string_)]
			tc.fn_ret_types['pair'] = types.Type(types.MultiReturn{
				types: [tc.parse_type('IError'), types.Type(types.string_)]
			})
			call := t.make_call_typed('pair', values, '(IError, string)')
			branch = t.make_block([t.make_expr_stmt(call)])
		} else {
			branch = t.make_block([t.make_expr_stmt(values[0]), t.make_expr_stmt(values[1])])
			if tail_kind == 'nested' {
				other := t.make_block([t.make_expr_stmt(values[0]), t.make_expr_stmt(values[1])])
				condition := t.make_if(t.make_bool_literal(true), branch, other)
				branch = t.make_block([condition])
			}
		}
		lowered := t.multi_if_assign_block(branch, staging.tail)
		t.leave_implicit_err_scope(err_scope)
		expected_moves := if tail_kind == 'nested' { 2 } else { 1 }
		assert guard_staging_true_stores(&a, lowered, staging.tail.assign_move_flags[0]) == 0
		assert guard_staging_true_stores(&a, lowered, staging.tail.assign_move_flags[1]) == expected_moves
		assert guard_staging_drop_count(&a, lowered) == 0
		body := a.children_of(&a.nodes[int(lowered)]).clone()
		finished := t.finish_guard_assignments(body, tail, staging)
		// Only the existing source scopes consume cleanup metadata; staged copies do not.
		assert guard_staging_scope_count(&a, finished) == guard_staging_scope_count(&a,
			lowered)
		assert guard_staging_drop_count(&a, finished) == 2
		wrapper := a.nodes[int(finished)]
		mut checked_drops := 0
		for child in a.children_of(&wrapper) {
			conditional := a.nodes[int(child)]
			if conditional.kind == .if_expr {
				assert conditional.skip_ownership_drops()
				drop_block := a.child_node(&conditional, 1)
				assert drop_block.value == skip_scope_drops_block_value
				checked_drops++
			}
		}
		assert checked_drops == 2
	}
}

fn guard_staging_scope_count(a &flat.FlatAst, id flat.NodeId) int {
	node := a.nodes[int(id)]
	mut count := if node.kind == .block && node.value != skip_scope_drops_block_value {
		1
	} else {
		0
	}
	for child in a.children_of(&node) {
		count += guard_staging_scope_count(a, child)
	}
	return count
}

fn guard_staging_true_stores(a &flat.FlatAst, id flat.NodeId, name string) int {
	node := a.nodes[int(id)]
	mut count := if node.kind == .assign && node.children_count == 2
		&& a.child_node(&node, 0).value == name && a.child_node(&node, 1).value == 'true' {
		1
	} else {
		0
	}
	for child in a.children_of(&node) {
		count += guard_staging_true_stores(a, child, name)
	}
	return count
}

fn guard_staging_drop_count(a &flat.FlatAst, id flat.NodeId) int {
	node := a.nodes[int(id)]
	mut count := if node.kind == .call && node.children_count > 0
		&& a.child_node(&node, 0).value == 'drop_owned' {
		1
	} else {
		0
	}
	for child in a.children_of(&node) {
		count += guard_staging_drop_count(a, child)
	}
	return count
}

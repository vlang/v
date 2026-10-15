module transform

import v.flat
import v.types

fn test_guard_staging_ends_error_scope_before_pointer_backed_destination() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['IError'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('err', flat.source_type_id('&string'))
	t.heaped_amp_locals['err'] = true
	tail := GuardReturnTail{
		assign_lhs_ids: [t.make_ident('err'), t.make_ident('_')]
	}
	staging := t.stage_guard_assignments(guard_assignment_tail(tail, ['err']), true)
	assert t.lvalue_type(staging.tail.assign_lhs_ids[0]) == flat.source_type_string
	assert staging.tail.assign_lhs_ids[1] == tail.assign_lhs_ids[1]
	assert staging.tail.assign_original_lhs_ids == tail.assign_lhs_ids
	// A nested guard writes the same staging storage, rather than adding another copy.
	nested := t.stage_guard_assignments(staging.tail, true)
	assert nested.decls.len == 0
	assert nested.tail.assign_lhs_ids == staging.tail.assign_lhs_ids
	mut body := []flat.NodeId{}
	err_scope := t.enter_implicit_err_scope(true)
	t.append_implicit_err_decl(mut body, t.make_struct_init(flat.source_type_id('IError')))
	t.leave_implicit_err_scope(err_scope)
	wrapper_id := t.finish_guard_assignments(t.make_block(body), tail, staging)
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
		t.set_var_type('err', flat.source_type_id('IError'))
		t.set_var_type('value', flat.source_type_id('string'))
		tail := GuardReturnTail{
			assign_lhs_ids: [t.make_ident('err'), t.make_ident('value')]
		}
		staging := t.stage_guard_assignments(guard_assignment_tail(tail, ['err']), true)
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
			tc.fn_param_types['pair'] = [tc.parse_type(flat.source_type_id('IError')),
				types.Type(types.string_)]
			tc.fn_ret_types['pair'] = types.Type(types.MultiReturn{
				types: [tc.parse_type(flat.source_type_id('IError')), types.Type(types.string_)]
			})
			call := t.make_call_typed('pair', values, flat.source_type_id('(IError, string)'))
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
		finished := t.finish_guard_assignments(t.make_block(body), tail, staging)
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

fn test_owned_guard_staging_distinguishes_success_bindings() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.collect(tc.a)
	tc.interface_names['IError'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('err', flat.source_type_id('IError'))
	t.set_var_type('value', flat.source_type_id('string'))
	tail := GuardReturnTail{
		assign_lhs_ids: [t.make_ident('err'), t.make_ident('value')]
	}
	staging := t.stage_guard_assignments(tail, true)
	success := guard_assignment_tail(staging.tail, ['value'])
	values := [t.make_ident('err'), t.make_ident('value')]
	assert tc.ownership_expr_moves_storage(values[0], tail.assign_lhs_ids[0])
	assert tc.ownership_expr_moves_storage(values[1], tail.assign_lhs_ids[1])
	assert t.multi_if_assign_target_was_moved(success, 0, values)
	assert !t.multi_if_assign_target_was_moved(success, 1, values)
	branch := t.make_block([t.make_expr_stmt(values[0]), t.make_expr_stmt(values[1])])
	lowered := t.multi_if_assign_block(branch, success)
	assert guard_staging_true_stores(&a, lowered, staging.tail.assign_move_flags[0]) == 1
	assert guard_staging_true_stores(&a, lowered, staging.tail.assign_move_flags[1]) == 0
	// Nested guards add their bindings while sharing the original staging storage.
	nested := t.stage_guard_assignments(guard_assignment_tail(success, ['err', 'value', '_', '']), true)
	assert nested.decls.len == 0
	assert nested.tail.assign_shadowed_names == ['value', 'err']
	assert nested.tail.assign_lhs_ids == staging.tail.assign_lhs_ids
	assert nested.tail.assign_original_lhs_ids == tail.assign_lhs_ids
	assert nested.tail.assign_move_flags == staging.tail.assign_move_flags
	assert !t.multi_if_assign_target_was_moved(nested.tail, 0, values)
	assert !t.multi_if_assign_target_was_moved(nested.tail, 1, values)
	assert staging.tail.assign_shadowed_names.len == 0
}

fn test_guard_staging_preserves_success_cleanup_marker() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('value', flat.source_type_id('string'))
	tail := GuardReturnTail{
		assign_lhs_ids: [t.make_ident('value'), t.make_ident('_')]
	}
	staging := t.stage_guard_assignments(tail, true)
	success := t.make_block_prefix_scope_drops([
		t.make_assign(staging.tail.assign_lhs_ids[0], t.make_string_literal('selected')),
	])
	failure := t.make_block([])
	guard := t.make_if(t.make_bool_literal(true), success, failure)
	expansion := t.make_block_skip_scope_drops([guard])
	finished := t.finish_guard_assignments(expansion, tail, staging)
	wrapper := a.nodes[int(finished)]
	assert wrapper.value == skip_scope_drops_block_value
	assert a.child(&wrapper, staging.decls.len) == expansion
	assert a.child_node(&a.nodes[int(guard)], 1).value == prefix_scope_drops_block_value
	assert guard_staging_scope_count(&a, finished) == guard_staging_scope_count(&a, expansion)
	assert guard_staging_scope_count(&a, finished) == 3
}

fn guard_staging_scope_count(a &flat.FlatAst, id flat.NodeId) int {
	node := a.nodes[int(id)]
	mut count := if node.kind == .block && node.value != skip_scope_drops_block_value {
		if node.value == prefix_scope_drops_block_value { 2 } else { 1 }
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

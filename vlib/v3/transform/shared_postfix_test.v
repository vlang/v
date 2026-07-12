module transform

import v3.flat

fn shared_postfix_test_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: i32(start)
		children_count: flat.child_count(children.len)
	})
}

fn shared_postfix_test_decl(mut a flat.FlatAst, name string, is_shared bool) flat.NodeId {
	lhs := shared_postfix_test_node(mut a, .ident, name, [])
	rhs := shared_postfix_test_node(mut a, .int_literal, '0', [])
	return shared_postfix_test_node(mut a, .decl_assign, if is_shared { 'shared' } else { '' }, [
		lhs,
		rhs,
	])
}

fn shared_postfix_test_mutation(mut a flat.FlatAst, name string) (flat.NodeId, flat.NodeId) {
	base := shared_postfix_test_node(mut a, .ident, name, [])
	index_value := shared_postfix_test_node(mut a, .int_literal, '0', [])
	index := shared_postfix_test_node(mut a, .index, '', [base, index_value])
	start := a.children.len
	a.children << index
	mutation := a.add_node(flat.Node{
		kind:           .postfix
		op:             .inc
		children_start: i32(start)
		children_count: 1
	})
	return mutation, base
}

fn test_shared_decl_scan_ignores_nested_lambda_boundaries() {
	mut a := flat.FlatAst.new()
	shared_decl := shared_postfix_test_decl(mut a, 'arr', true)
	lambda := shared_postfix_test_node(mut a, .lambda_expr, '', [])
	mutation, base := shared_postfix_test_mutation(mut a, 'arr')
	shared_postfix_test_node(mut a, .fn_decl, 'main', [shared_decl, lambda, mutation])
	other_mutation, _ := shared_postfix_test_mutation(mut a, 'arr')
	shared_postfix_test_node(mut a, .fn_decl, 'other', [other_mutation])

	t := Transformer{
		a: &a
	}
	assert t.local_decl_is_shared_before('arr', mutation)
	target := t.shared_postfix_autolock_target(mutation) or {
		assert false, 'shared array postfix mutation should be autolocked'
		return
	}
	assert target == base
	assert !t.local_decl_is_shared_before('arr', other_mutation)
	if _ := t.shared_postfix_autolock_target(other_mutation) {
		assert false, 'a shared declaration from another function must not leak'
	}
}

fn test_shared_decl_scan_respects_inner_block_scope() {
	mut a := flat.FlatAst.new()
	outer_plain := shared_postfix_test_decl(mut a, 'arr', false)
	inner_shared := shared_postfix_test_decl(mut a, 'arr', true)
	inner_shared_block := shared_postfix_test_node(mut a, .block, '', [inner_shared])
	plain_outer_mutation, _ := shared_postfix_test_mutation(mut a, 'arr')
	shared_postfix_test_node(mut a, .fn_decl, 'plain_outer', [outer_plain, inner_shared_block,
		plain_outer_mutation])

	outer_shared := shared_postfix_test_decl(mut a, 'arr', true)
	inner_plain := shared_postfix_test_decl(mut a, 'arr', false)
	inner_plain_block := shared_postfix_test_node(mut a, .block, '', [inner_plain])
	shared_outer_mutation, shared_outer_base := shared_postfix_test_mutation(mut a, 'arr')
	shared_postfix_test_node(mut a, .fn_decl, 'shared_outer', [outer_shared, inner_plain_block,
		shared_outer_mutation])

	inner_target_outer := shared_postfix_test_decl(mut a, 'arr', false)
	inner_target_decl := shared_postfix_test_decl(mut a, 'arr', true)
	inner_target_mutation, inner_target_base := shared_postfix_test_mutation(mut a, 'arr')
	inner_target_block := shared_postfix_test_node(mut a, .block, '', [
		inner_target_decl,
		inner_target_mutation,
	])
	shared_postfix_test_node(mut a, .fn_decl, 'inner_target',
		[inner_target_outer, inner_target_block])

	t := Transformer{
		a: &a
	}
	assert !t.local_decl_is_shared_before('arr', plain_outer_mutation)
	assert t.local_decl_is_shared_before('arr', shared_outer_mutation)
	assert t.local_decl_is_shared_before('arr', inner_target_mutation)
	if _ := t.shared_postfix_autolock_target(plain_outer_mutation) {
		assert false, 'an inner shared binding must not autolock the outer plain array'
	}
	assert t.shared_postfix_autolock_target(shared_outer_mutation) or { flat.empty_node } == shared_outer_base
	assert t.shared_postfix_autolock_target(inner_target_mutation) or { flat.empty_node } == inner_target_base
}

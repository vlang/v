module transform

import v3.flat

fn test_shared_decl_scan_ignores_nested_lambda_boundaries() {
	mut a := flat.FlatAst.new()
	a.add_val(.fn_decl, 'main')
	lhs_id := a.add_val(.ident, 'arr')
	decl_start := a.children.len
	a.children << lhs_id
	a.add_node(flat.Node{
		kind:           .decl_assign
		value:          'shared'
		children_start: decl_start
		children_count: 1
	})
	a.add_val(.fn_literal, '')
	a.add_val(.lambda_expr, '')
	base_id := a.add_val(.ident, 'arr')
	index_value_id := a.add_val(.int_literal, '0')
	index_start := a.children.len
	a.children << base_id
	a.children << index_value_id
	index_id := a.add_node(flat.Node{
		kind:           .index
		children_start: index_start
		children_count: 2
	})
	mutation_start := a.children.len
	a.children << index_id
	mutation_id := a.add_node(flat.Node{
		kind:           .postfix
		op:             .inc
		children_start: mutation_start
		children_count: 1
	})

	t := Transformer{
		a: &a
	}
	assert t.local_decl_is_shared_before('arr', mutation_id)
	target_id := t.shared_postfix_autolock_target(mutation_id) or {
		assert false, 'shared array postfix mutation should be autolocked'
		return
	}
	assert target_id == base_id

	a.add_val(.fn_decl, 'other')
	other_base_id := a.add_val(.ident, 'arr')
	other_index_start := a.children.len
	a.children << other_base_id
	a.children << index_value_id
	other_index_id := a.add_node(flat.Node{
		kind:           .index
		children_start: other_index_start
		children_count: 2
	})
	other_mutation_start := a.children.len
	a.children << other_index_id
	other_mutation_id := a.add_node(flat.Node{
		kind:           .postfix
		op:             .inc
		children_start: other_mutation_start
		children_count: 1
	})
	assert !t.local_decl_is_shared_before('arr', other_mutation_id)
	if _ := t.shared_postfix_autolock_target(other_mutation_id) {
		assert false, 'a shared declaration from another function must not leak'
	}
}

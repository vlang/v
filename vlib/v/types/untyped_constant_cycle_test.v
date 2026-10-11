module types

import v.flat

fn test_untyped_numeric_expression_cycles_are_rejected() {
	mut a := flat.FlatAst.new()
	cyclic := a.add_val(.ident, 'cyclic')
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.const_types['cyclic'] = builtin_int_type
	tc.const_exprs['cyclic'] = cyclic
	mut states := map[flat.NodeId]u8{}
	known, has_float := tc.untyped_numeric_literal_expr_info(cyclic, mut states)
	assert !known
	assert !has_float
	assert states[cyclic] == 4
}

fn test_untyped_numeric_shared_expression_is_classified_once() {
	mut a := flat.FlatAst.new()
	mut expr := a.add_val(.int_literal, '1')
	for _ in 0 .. 24 {
		start := a.begin_children()
		a.add_child(expr)
		a.add_child(expr)
		expr = a.add_node(flat.Node{
			kind:           .infix
			op:             .plus
			children_start: start
			children_count: 2
		})
	}
	tc := TypeChecker.new(&a)
	mut states := map[flat.NodeId]u8{}
	known, has_float := tc.untyped_numeric_literal_expr_info(expr, mut states)
	assert known
	assert !has_float
	assert states.len == 25
}

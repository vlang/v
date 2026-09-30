module types

import v.flat

fn float_assignment_test_prefix(mut a flat.FlatAst, operand flat.NodeId, op flat.Op) flat.NodeId {
	children_start := a.begin_children()
	a.add_child(operand)
	return a.add_node(flat.Node{
		kind:           .prefix
		op:             op
		children_start: children_start
		children_count: 1
	})
}

fn test_float_assignment_preserves_signed_integer_literals() {
	for op in [flat.Op.plus, flat.Op.minus] {
		mut a := flat.FlatAst.new()
		literal := a.add_node(flat.Node{
			kind:  .int_literal
			value: '1'
		})
		signed := float_assignment_test_prefix(mut a, literal, op)
		children_start := a.begin_children()
		a.add_child(signed)
		parenthesized := a.add_node(flat.Node{
			kind:           .paren
			children_start: children_start
			children_count: 1
		})
		nested := float_assignment_test_prefix(mut a, parenthesized, op)
		tc := TypeChecker.new(&a)
		for rhs in [literal, signed, parenthesized, nested] {
			operand := tc.assignment_integer_literal_operand(rhs) or {
				panic('expected an integer literal operand')
			}
			assert operand == literal
			for expected in [Type(f32_), Type(f64_)] {
				assert tc.assignment_types_compatible(rhs, Type(int_), expected, .assign)
			}
		}
	}
}

fn test_float_assignment_does_not_treat_typed_integers_as_literals() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{
		kind:  .int_literal
		value: '1'
	})
	variable := a.add_node(flat.Node{
		kind:  .ident
		value: 'value'
	})
	children_start := a.begin_children()
	a.add_child(literal)
	cast := a.add_node(flat.Node{
		kind:           .cast_expr
		value:          'int'
		typ:            'int'
		children_start: children_start
		children_count: 1
	})
	negative_variable := float_assignment_test_prefix(mut a, variable, .minus)
	negative_cast := float_assignment_test_prefix(mut a, cast, .minus)
	tc := TypeChecker.new(&a)
	for rhs in [variable, cast, negative_variable, negative_cast] {
		assert tc.assignment_integer_literal_operand(rhs) == none
		assert !tc.assignment_types_compatible(rhs, Type(int_), Type(f32_), .assign)
	}
}

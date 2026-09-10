module types

import v3.flat

fn test_resolve_type_treats_return_statements_as_void() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
	})
	children_start := a.begin_children()
	a.add_child(value)
	return_with_value := a.add_node(flat.Node{
		kind: .return_stmt
		children_start: children_start
		children_count: 1
	})
	bare_return := a.add_node(flat.Node{
		kind: .return_stmt
	})
	tc := TypeChecker.new(&a)

	assert tc.resolve_type(return_with_value) is Void
	assert tc.resolve_type(bare_return) is Void
}

// gen_fn_field_call emits fn field call output for c.
fn (mut g FlatGen) gen_fn_field_call(node flat.Node, fn_node &flat.Node, base_type types.Type) bool {
	field_type := g.field_type(base_type, fn_node.value) or { return false }
	fn_type := fn_type_from(field_type) or { return false }
	field_is_ptr := fn_type_is_pointer(field_type)
	if field_is_ptr {
		g.write('(*')
	}
	// Use normal field selection for pointer-backed mut receivers and parameters.
	g.gen_expr(g.a.child(&node, 0))
	if field_is_ptr {
		g.write(')')
	}
	g.write('(')
	for i in 1 .. node.children_count {
		if i > 1 {
			g.write(', ')
		}
		arg_id := g.a.child(&node, i)
		arg_idx := i - 1
		if arg_idx < fn_type.params.len {
			g.gen_arg_for_expected_type(arg_id, fn_type.params[arg_idx])
		} else {
			g.gen_expr(arg_id)
		}
	}
	g.write(')')
	return true
}

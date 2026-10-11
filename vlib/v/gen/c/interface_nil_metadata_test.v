module c

import v.flat
import v.types

fn interface_nil_test_wrapper(mut a flat.FlatAst, kind flat.NodeKind, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		children_start: i32(start)
		children_count: flat.child_count(children.len)
	})
}

fn test_literal_nil_interface_initializes_the_entire_metadata_word() {
	mut a := flat.FlatAst.new()
	nil_id := a.add(.nil_literal)
	expression := interface_nil_test_wrapper(mut a, .expr_stmt, [nil_id])
	block := interface_nil_test_wrapper(mut a, .block, [expression])
	paren := interface_nil_test_wrapper(mut a, .paren, [block])
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	iface := types.Type(types.Interface{ name: 'Reader' })
	alias := types.Type(types.Alias{ name: 'ReaderAlias', base_type: iface })
	for target in [iface, alias] {
		for value in [nil_id, block, paren] {
			g.sb.clear()
			assert g.gen_interface_value_expr(value, target)
			output := g.sb.str()
			assert output.contains('._object = '), output
			assert output.contains('._interface_meta = NULL'), output
			assert !output.contains('._typ = '), output
		}
	}
}

fn test_literal_nil_interface_preserves_block_effects() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'record_nil_effect')
	call := interface_nil_test_wrapper(mut a, .call, [callee])
	effect := interface_nil_test_wrapper(mut a, .expr_stmt, [call])
	nil_id := a.add(.nil_literal)
	tail := interface_nil_test_wrapper(mut a, .expr_stmt, [nil_id])
	block := interface_nil_test_wrapper(mut a, .block, [effect, tail])
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.gen_interface_value_expr(block, types.Type(types.Interface{ name: 'Reader' }))
	output := g.sb.str()
	assert output.count('record_nil_effect(') == 1, output
	assert output.contains('._interface_meta = NULL'), output
}

fn test_interface_nil_detection_keeps_concrete_zero_values() {
	mut a := flat.FlatAst.new()
	nil_id := a.add(.nil_literal)
	zero := a.add_val(.int_literal, '0')
	cast := interface_nil_test_wrapper(mut a, .cast_expr, [nil_id])
	paren := interface_nil_test_wrapper(mut a, .paren, [zero])
	mut g := FlatGen.new()
	g.a = &a
	assert !g.interface_expr_is_bare_nil(zero)
	assert !g.interface_expr_is_bare_nil(paren)
	assert !g.interface_expr_is_bare_nil(cast)
	assert !g.interface_expr_is_bare_nil(flat.NodeId(-1))
	assert !g.interface_expr_is_bare_nil(flat.NodeId(a.nodes.len))
}

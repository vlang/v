module c

import v.flat
import v.types

fn if_expr_chain_node(mut a flat.FlatAst, kind flat.NodeKind, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		children_start: i32(start)
		children_count: i32(children.len)
	})
}

fn if_expr_chain_gen(depth int) (&FlatGen, flat.NodeId) {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	condition := ast.add_node(flat.Node{ kind: .bool_literal, value: 'false' })
	one := ast.add_node(flat.Node{ kind: .int_literal, value: '1' })
	mut tail := ast.add_node(flat.Node{ kind: .int_literal, value: '0' })
	for _ in 0 .. depth {
		then_expr := if_expr_chain_node(mut ast, .expr_stmt, [one])
		then_block := if_expr_chain_node(mut ast, .block, [then_expr])
		else_expr := if_expr_chain_node(mut ast, .expr_stmt, [tail])
		else_block := if_expr_chain_node(mut ast, .block, [else_expr])
		tail = if_expr_chain_node(mut ast, .if_expr, [condition, then_block, else_block])
	}
	return &g, tail
}

fn test_if_expression_chain_preserves_nested_ternaries() {
	mut g, root := if_expr_chain_gen(2)
	g.gen_expr(root)
	assert g.sb.str() == '(false ? 1 : (false ? 1 : 0))'
}

fn test_if_expression_chain_handles_deep_type_name_dispatch() {
	depth := 8192
	mut g, root := if_expr_chain_gen(depth)
	g.gen_expr(root)
	code := g.sb.str()
	assert code.count('?') == depth
	assert code.count('(') == code.count(')')
}

fn test_if_expression_chain_keeps_expression_overrides() {
	for callback in [false, true] {
		mut g, root := if_expr_chain_gen(2)
		node := g.a.nodes[int(root)]
		else_block := g.a.child_node(&node, 2)
		tail := g.a.child_node(else_block, 0)
		inner := g.a.child(tail, 0)
		if callback {
			g.callback_target_overrides[int(inner)] = '7'
		} else {
			g.assert_expr_overrides[int(inner)] = '7'
		}
		g.gen_expr(root)
		assert g.sb.str() == '(false ? 1 : 7)'
	}
}

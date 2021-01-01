module walker

import v.ast

pub interface Visitor {
	visit(node ast.Node) ?Visitor
}

struct Inspector {
	i_fn fn (node ast.Node, data voidptr) bool
mut:
	data voidptr
}

pub fn (i &Inspector) visit(node ast.Node) ?Visitor {
	if i.i_fn(node, i.data) {
		return &i
	}
	return none
}

pub fn inspect(node ast.Node, data voidptr, i_fn fn (node ast.Node, data voidptr) bool) {
	walk(Inspector{i_fn, data}, node)
}

pub fn walk(visitor Visitor, node ast.Node) {
	v := visitor.visit(node) or {
		return
	}
	children := node.children()
	for child_node in children {
		walk(v, child_node)
	}
}

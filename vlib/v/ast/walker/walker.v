module walker

import v.ast

pub interface Visitor {
	visit(node ast.Node) ?
}

pub type InspectorFn = fn (node ast.Node, data voidptr) bool

struct Inspector {
	inspector_callback InspectorFn
mut:
	data voidptr
}

pub fn (i &Inspector) visit(node ast.Node) ? {
	if i.inspector_callback(node, i.data) {
		return
	}
	return none
}

pub fn inspect(node ast.Node, data voidptr, inspector_callback InspectorFn) {
	walk(Inspector{inspector_callback, data}, node)
}

pub fn walk(visitor Visitor, node ast.Node) {
	visitor.visit(node) or {
		return
	}
	children := node.children()
	for child_node in children {
		walk(visitor, child_node)
	}
}

module walker

import v.ast

pub interface Visitor {
	visit(node ast.Node) ?Visitor
}

pub type InspectorFn = fn (node ast.Node, data voidptr) bool

struct Inspector {
	inspector_callback InspectorFn
mut:
	data voidptr
}

pub fn (i &Inspector) visit(node ast.Node) ?Visitor {
	if i.inspector_callback(node, i.data) {
		return &i
	}
	return none
}

pub fn inspect(node ast.Node, data voidptr, inspector_callback InspectorFn) {
	walk(Inspector{inspector_callback, data}, node)
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

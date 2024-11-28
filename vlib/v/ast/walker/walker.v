module walker

import v.ast

// Visitor defines a visit method which is invoked by the walker in each node it encounters.
pub interface Visitor {
mut:
	visit(node &ast.Node) !
}

pub type InspectorFn = fn (node &ast.Node, data voidptr) bool

fn empty_callback(node &ast.Node, data voidptr) bool {
	panic('empty ast.walker, node: ${voidptr(node)}, data: ${voidptr(data)}')
}

struct Inspector {
	inspector_callback InspectorFn = empty_callback
mut:
	data voidptr
}

pub fn (i &Inspector) visit(node &ast.Node) ! {
	if i.inspector_callback(node, i.data) {
		return
	}
	return error('')
}

// inspect traverses and checks the AST node on a depth-first order and based on the data given
pub fn inspect(node &ast.Node, data voidptr, inspector_callback InspectorFn) {
	mut inspector := Inspector{inspector_callback, data}
	walk(mut inspector, node)
}

// walk traverses the AST using the given visitor
pub fn walk(mut visitor Visitor, node &ast.Node) {
	visitor.visit(node) or { return }
	children := node.children()
	for child_node in children {
		walk(mut visitor, &child_node)
	}
}

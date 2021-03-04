module walker

import v.ast

// Visitor defines a visit method which is invoked by the walker in each node it encounters.
pub interface Visitor {
mut:
	visit(node ast.Node) ?
}

pub type InspectorFn = fn (node ast.Node, data voidptr) bool

struct Inspector {
	inspector_callback InspectorFn
mut:
	data voidptr
}

pub fn (mut i Inspector) visit(node ast.Node) ? {
	if i.inspector_callback(node, i.data) {
		return
	}
	return none
}

// inspect traverses and checks the AST node on a depth-first order and based on the data given
pub fn inspect(node ast.Node, data voidptr, inspector_callback InspectorFn) {
	walk(mut Inspector{inspector_callback, data}, node)
}

// walk traverses the AST using the given visitor
pub fn walk(mut visitor Visitor, node ast.Node) {
	visitor.visit(node) or { return }
	children := node.children()
	for child_node in children {
		walk(mut visitor, &child_node)
	}
}


// [deprecated: 'use `Visitor.walk()` instead']
// pub fn walk(visitor Visitor, node ast.Node) {
// 	mut visitor_ := visitor
// 	visitor_.walk(node)
// }

// // walk traverses the AST using the given visitor
// pub fn (mut v Visitor) walk(node ast.Node) {
// 	v.visit(node) or { return }
// 	children := node.children()
// 	for child_node in children {
// 		v.walk(child_node)
// 	}
// }

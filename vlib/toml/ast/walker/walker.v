module walker

import toml.ast

// Visitor defines a visit method which is invoked by the walker in each node it encounters.
pub interface Visitor {
	visit(node &ast.Node) ?
}

pub type InspectorFn = fn (node &ast.Node, data voidptr) ?

struct Inspector {
	inspector_callback InspectorFn
mut:
	data voidptr
}

pub fn (i &Inspector) visit(node &ast.Node) ? {
	i.inspector_callback(node, i.data) or { return err }
}

// inspect traverses and checks the AST node on a depth-first order and based on the data given
pub fn inspect(node &ast.Node, data voidptr, inspector_callback InspectorFn) ? {
	walk(Inspector{inspector_callback, data}, node) ?
}

// walk traverses the AST using the given visitor
pub fn walk(visitor Visitor, node &ast.Node) ? {
	if node is map[string]ast.Node {
		n := node as map[string]ast.Node
		for _, nn in n {
			walk(visitor, &nn) ?
		}
	} else {
		visitor.visit(node) ?
	}
}

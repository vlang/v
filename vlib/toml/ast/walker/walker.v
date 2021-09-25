module walker

import toml.ast

// Visitor defines a visit method which is invoked by the walker in each Value node it encounters.
pub interface Visitor {
	visit(value &ast.Value) ?
}

pub type InspectorFn = fn (value &ast.Value, data voidptr) ?

struct Inspector {
	inspector_callback InspectorFn
mut:
	data voidptr
}

pub fn (i &Inspector) visit(value &ast.Value) ? {
	i.inspector_callback(value, i.data) or { return err }
}

// inspect traverses and checks the AST Value node on a depth-first order and based on the data given
pub fn inspect(value &ast.Value, data voidptr, inspector_callback InspectorFn) ? {
	walk(Inspector{inspector_callback, data}, value) ?
}

// walk traverses the AST using the given visitor
pub fn walk(visitor Visitor, value &ast.Value) ? {
	if value is map[string]ast.Value {
		value_map := value as map[string]ast.Value
		for _, val in value_map {
			walk(visitor, &val) ?
		}
	} else {
		visitor.visit(value) ?
	}
}

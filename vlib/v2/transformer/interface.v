// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module transformer

import v2.ast
import v2.types

// is_interface_var checks if a variable is an interface type by looking up its type in scope
fn (t &Transformer) is_interface_var(name string) bool {
	// Special case: 'err' in or-blocks is always IError interface
	if name == 'err' {
		return true
	}
	typ := t.lookup_var_type(name) or { return false }
	return typ is types.Interface
}

// is_interface_receiver checks if an expression's type is an interface type
fn (t &Transformer) is_interface_receiver(expr ast.Expr) bool {
	if expr is ast.Ident {
		// Use scope lookup to check if variable's type is an interface
		return t.is_interface_var(expr.name)
	}
	return false
}

// is_interface_cast checks if an expression is an interface cast like Calculator(value)
fn (t &Transformer) is_interface_cast(expr ast.Expr) bool {
	// Interface casts appear as CallOrCastExpr: InterfaceType(value)
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			// Look up the type name in the environment
			type_name := (expr.lhs as ast.Ident).name
			mut scope := t.get_current_scope() or { return false }
			obj := scope.lookup_parent(type_name, 0) or { return false }
			if obj is types.Type {
				return obj is types.Interface
			}
		}
	}
	return false
}

// get_expr_type_name returns the concrete type name for an expression using the type checker env
fn (t &Transformer) get_expr_type_name(expr ast.Expr) ?string {
	if typ := t.get_expr_type(expr) {
		name := t.type_to_c_name(typ)
		if name != '' {
			return name
		}
	}
	// For simple identifiers, try scope lookup
	if expr is ast.Ident {
		if typ := t.lookup_var_type(expr.name) {
			name := t.type_to_c_name(typ)
			if name != '' {
				return name
			}
		}
	}
	return none
}

// get_interface_concrete_type returns the concrete type name for an interface variable
// if it was tracked during interface cast lowering (native backend only)
fn (t &Transformer) get_interface_concrete_type(name string) ?string {
	if concrete := t.interface_concrete_types[name] {
		return concrete
	}
	return none
}

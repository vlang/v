// Copyright (c) 2019-2024 Felipe Pena All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module type_resolver

import v.ast

// unwrap_generic_expr retrieves the concrete type from a generic expr
pub fn (mut ct TypeResolver) unwrap_generic_expr(expr ast.Expr, default_typ ast.Type) ast.Type {
	match expr {
		ast.StringLiteral, ast.StringInterLiteral {
			return ast.string_type
		}
		ast.ParExpr {
			return ct.unwrap_generic_expr(expr.expr, default_typ)
		}
		ast.CastExpr {
			return expr.typ
		}
		ast.InfixExpr {
			if ct.info.is_comptime(expr.left) {
				return ct.resolver.unwrap_generic(ct.get_type(expr.left))
			}
			if ct.info.is_comptime(expr.right) {
				return ct.resolver.unwrap_generic(ct.get_type(expr.right))
			}
			return default_typ
		}
		ast.Ident {
			return if expr.ct_expr {
				ct.resolver.unwrap_generic(ct.get_type(expr))
			} else {
				default_typ
			}
		}
		ast.AsCast {
			return ct.resolver.unwrap_generic(expr.typ)
		}
		else {
			return default_typ
		}
	}
}

// is_generic_param_var checks if the var is related to generic parameter
@[inline]
pub fn (t &TypeResolver) is_generic_param_var(node ast.Expr) bool {
	return node is ast.Ident && node.info is ast.IdentVar && node.obj is ast.Var
		&& (node.obj as ast.Var).ct_type_var == .generic_param
}

// is_generic_expr checks if the expr relies on fn generic argument
pub fn (t &TypeResolver) is_generic_expr(node ast.Expr) bool {
	return match node {
		ast.Ident {
			// variable declared as generic type
			t.is_generic_param_var(node)
		}
		ast.IndexExpr {
			// generic_var[N]
			t.is_generic_param_var(node.left)
		}
		ast.CallExpr {
			// fn which has any generic dependent expr
			if node.args.any(t.is_generic_param_var(it.expr)) {
				return true
			}
			if node.is_static_method && node.left_type.has_flag(.generic) {
				return true
			}
			// fn[T]() or generic_var.fn[T]()
			node.concrete_types.any(it.has_flag(.generic))
		}
		ast.SelectorExpr {
			// generic_var.property
			t.is_generic_param_var(node.expr)
		}
		ast.AsCast {
			// var as T
			node.typ.has_flag(.generic)
		}
		else {
			false
		}
	}
}

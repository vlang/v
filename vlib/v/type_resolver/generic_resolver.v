module type_resolver

import v.ast

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
		else {
			return default_typ
		}
	}
}

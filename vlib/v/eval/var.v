module eval

import v.ast

pub struct Var {
pub mut:
	val Object
pub:
	scope_idx int
	typ       ast.Type
}

pub fn (mut e Eval) open_scope() {
	e.scope_idx++
}

pub fn (mut e Eval) close_scope() {
	e.scope_idx--
	for name, var in e.local_vars {
		if var.scope_idx > e.scope_idx {
			e.local_vars.delete(name)
		}
	}
}

pub fn (mut e Eval) set(expr ast.Expr, val Object, init bool, typ ast.Type) {
	match expr {
		ast.Ident {
			if init {
				e.local_vars[expr.name] = Var{
					val: val
					scope_idx: e.scope_idx
					typ: typ
				}
			} else {
				e.local_vars[expr.name].val = val
			}
		}
		ast.IndexExpr {
			panic('>>${expr.pos}, ${e.cur_file}')

			// if init {
			// 	e.error('index init assignment')
			// } else {
			// 	mut x := (e.local_vars[(expr.left as ast.Ident).name].val)
			// 	if x is Array {
			// 		x.val[(e.expr(expr.index, ast.int_type_idx) as Int).val] = val
			// 	}
			// }
		}
		else {
			panic('unknown left value to assign statment: ${expr.type_name()}')
		}
	}
}

// val and expr must be both numeric types, or both string
pub fn (mut e Eval) add(expr ast.Expr, val Object) {
	match expr {
		ast.Ident {
			e.local_vars[expr.name].val = e.infix_expr(e.local_vars[expr.name].val, val,
				.plus, e.local_vars[expr.name].typ)
		}
		else {
			panic('unknown left value to add statment: ${expr.type_name()}')
		}
	}
}

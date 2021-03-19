module eval

import v.ast

pub struct Var {
pub mut:
	val Object
pub:
	scope_idx int
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

pub fn (mut e Eval) set(expr ast.Expr, val Object, init bool) {
	match expr {
		ast.Ident {
			if init {
				e.local_vars[expr.name] = Var{
					val: val
					scope_idx: e.scope_idx
				}
			} else {
				e.local_vars[expr.name].val = val
			}
		}
		else {
			panic('unknown left value to assign statment: $expr.type_name()')
		}
	}
}

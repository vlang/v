// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import v.ast

fn (mut g Gen) comptime_at(node ast.AtExpr) string {
	return node.val
}

fn (mut g Gen) comptime_conditional(node ast.IfExpr) ?ast.IfBranch {
	return node.branches.filter((node.has_else && it == node.branches.last())
		|| g.comptime_is_truthy(it.cond))[0] or { return none }
}

fn (mut g Gen) should_emit_hash_stmt(node ast.HashStmt) bool {
	if node.ct_low_level_cond.len > 0 {
		if !g.comptime_ident(node.ct_low_level_cond, false) {
			return false
		}
	}
	return node.ct_conds.all(g.comptime_is_truthy(it))
}

fn (mut g Gen) comptime_is_truthy(cond ast.Expr) bool {
	match cond {
		ast.BoolLiteral {
			return cond.val
		}
		ast.ParExpr {
			return g.comptime_is_truthy(cond.expr)
		}
		ast.PrefixExpr {
			match cond.op {
				.not {
					return !g.comptime_is_truthy(cond.right)
				}
				else {
					g.n_error('${@LOCATION} Compile time infix expr `${cond}` is not handled by the native backed.')
				}
			}
		}
		ast.PostfixExpr {
			return g.comptime_ident((cond.expr as ast.Ident).name, true)
		}
		ast.InfixExpr {
			match cond.op {
				.logical_or {
					return g.comptime_is_truthy(cond.left) || g.comptime_is_truthy(cond.right)
				}
				.and {
					return g.comptime_is_truthy(cond.left) && g.comptime_is_truthy(cond.right)
				}
				.eq {
					return g.comptime_is_truthy(cond.left) == g.comptime_is_truthy(cond.right)
				}
				.ne {
					return g.comptime_is_truthy(cond.left) != g.comptime_is_truthy(cond.right)
				}
				.key_is {
					// TODO: implement properly, to support @[flag_enum_fn] functions
					return true
				}
				else {
					g.n_error('${@LOCATION} Compile time infix expr `${cond}` is not handled by the native backend.')
				}
			}
		}
		ast.Ident {
			return g.comptime_ident(cond.name, false)
		}
		ast.ComptimeCall {
			g.n_error('${@LOCATION} Comptime calls are not implemented')
		}
		else {
			// should be unreachable
			g.n_error('${@LOCATION} Compile time conditional `${cond}` is not handled by the native backend.')
		}
	}

	return false
}

fn (mut g Gen) comptime_ident(name string, is_comptime_option bool) bool {
	if is_comptime_option {
		return name in g.pref.compile_defines
	}
	match name {
		'no_segfault_handler', 'no_backtrace' {
			return false
		}
		'no_main' {
			return g.pref.is_script
		}
		'threads' {
			return g.table.gostmts > 0
		}
		else {}
	}

	return ast.eval_comptime_not_user_defined_ident(name, g.pref) or {
		if name in g.pref.compile_defines {
			return true
		}
		g.n_error('${@LOCATION} Unhandled os ifdef name "${name}".')
		return false
	}
}

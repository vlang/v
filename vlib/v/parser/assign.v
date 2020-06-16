// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast


fn (mut p Parser) assign_stmt() ast.Stmt {
	return p.partial_assign_stmt(p.expr_list())
}

fn (mut p Parser) check_undefined_variables(exprs []ast.Expr, val ast.Expr) {
	match val {
		ast.Ident {
			for expr in exprs {
				if expr is ast.Ident {
					ident := expr as ast.Ident
					if ident.name == it.name {
						p.error_with_pos('undefined variable: `$it.name`', it.pos)
					}
				}
			}
		}
		ast.InfixExpr {
			p.check_undefined_variables(exprs, it.left)
			p.check_undefined_variables(exprs, it.right)
		}
		ast.ParExpr {
			p.check_undefined_variables(exprs, it.expr)
		}
		ast.PostfixExpr {
			p.check_undefined_variables(exprs, it.expr)
		}
		ast.PrefixExpr {
			p.check_undefined_variables(exprs, it.right)
		}
		ast.StringInterLiteral {
			for expr_ in it.exprs {
				p.check_undefined_variables(exprs, expr_)
			}
		}
		else {}
	}
}

fn (mut p Parser) check_cross_variables(exprs []ast.Expr, val ast.Expr) bool {
	match val {
		ast.Ident {
			for expr in exprs {
				if expr is ast.Ident {
					ident := expr as ast.Ident
					if ident.name == it.name { return true }
				}
			}
		}
		ast.InfixExpr {
			return p.check_cross_variables(exprs, it.left) || p.check_cross_variables(exprs, it.right)
		}
		ast.PrefixExpr {
			return p.check_cross_variables(exprs, it.right)
		}
		ast.PostfixExpr {
			return p.check_cross_variables(exprs, it.expr)
		}
		else {}
	}
	return false
}

fn (mut p Parser) partial_assign_stmt(left []ast.Expr) ast.Stmt {
	p.is_stmt_ident = false
	op := p.tok.kind
	pos := p.tok.position()
	p.next()
	right := p.expr_list()
	mut has_cross_var := false
	if right.len > 1 {
		for r in right {
			has_cross_var = p.check_cross_variables(left, r)
		}
	}
	for i, lx in left {
		match lx {
			ast.Ident {
				if op == .decl_assign {
					p.check_undefined_variables(right, lx)
				}
				if op == .decl_assign {
					if left.len == right.len {
						p.scope.register(it.name, ast.Var{
							name: it.name
							expr: right[i]
							is_mut: it.is_mut || p.inside_for
							pos: it.pos
						})
					} else {
						p.scope.register(it.name, ast.Var{
							name: it.name
							is_mut: it.is_mut || p.inside_for
							pos: it.pos
						})
					}
				}
			}
			ast.IndexExpr {
				it.is_setter = true
			}
			ast.ParExpr {}
			ast.PrefixExpr {}
			ast.SelectorExpr {
				if op == .decl_assign {
					p.error_with_pos('struct fields can only be declared during the initialization', it.pos)
				}
			}
			else {}
			// TODO: parexpr ( check vars)
			// else { p.error_with_pos('unexpected `${typeof(lx)}`', lx.position()) }
		}
	}
	return ast.AssignStmt{op: op, left: left, right: right, pos: pos, has_cross_var: has_cross_var, is_simple: p.inside_for && p.tok.kind == .lcbr}
}

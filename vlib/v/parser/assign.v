// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast

fn (mut p Parser) assign_stmt() ast.Stmt {
	mut defer_vars := p.defer_vars.clone()
	p.defer_vars = []ast.Ident{}

	exprs, comments := p.expr_list()

	if !(p.inside_defer && p.tok.kind == .decl_assign) {
		defer_vars << p.defer_vars
	}
	p.defer_vars = defer_vars
	return p.partial_assign_stmt(exprs, comments)
}

const max_expr_level = 100

fn (mut p Parser) check_undefined_variables(names []string, val ast.Expr) ! {
	p.expr_level++
	defer {
		p.expr_level--
	}
	if p.expr_level > parser.max_expr_level {
		return error('expr level > $parser.max_expr_level')
	}
	match val {
		ast.Ident {
			for name in names {
				if name == val.name && val.kind != .blank_ident {
					p.error_with_pos('undefined variable: `$val.name`', val.pos)
					return error('undefined variable: `$val.name`')
				}
			}
		}
		ast.ArrayInit {
			if val.has_cap {
				p.check_undefined_variables(names, val.cap_expr)!
			}
			if val.has_len {
				p.check_undefined_variables(names, val.len_expr)!
			}
			if val.has_default {
				p.check_undefined_variables(names, val.default_expr)!
			}
			for expr in val.exprs {
				p.check_undefined_variables(names, expr)!
			}
		}
		ast.CallExpr {
			p.check_undefined_variables(names, val.left)!
			for arg in val.args {
				p.check_undefined_variables(names, arg.expr)!
			}
		}
		ast.CastExpr {
			p.check_undefined_variables(names, val.expr)!
			p.check_undefined_variables(names, val.arg)!
		}
		ast.IndexExpr {
			p.check_undefined_variables(names, val.left)!
			p.check_undefined_variables(names, val.index)!
		}
		ast.InfixExpr {
			p.check_undefined_variables(names, val.left)!
			p.check_undefined_variables(names, val.right)!
		}
		ast.IfExpr {
			p.check_undefined_variables(names, val.left)!
			for branch in val.branches {
				p.check_undefined_variables(names, branch.cond)!
				for stmt in branch.stmts {
					if stmt is ast.ExprStmt {
						p.check_undefined_variables(names, stmt.expr)!
					}
				}
			}
		}
		ast.MapInit {
			for key in val.keys {
				p.check_undefined_variables(names, key)!
			}
			for value in val.vals {
				p.check_undefined_variables(names, value)!
			}
		}
		ast.MatchExpr {
			p.check_undefined_variables(names, val.cond)!
			for branch in val.branches {
				for expr in branch.exprs {
					p.check_undefined_variables(names, expr)!
				}
				for stmt in branch.stmts {
					if stmt is ast.ExprStmt {
						p.check_undefined_variables(names, stmt.expr)!
					}
				}
			}
		}
		ast.ParExpr {
			p.check_undefined_variables(names, val.expr)!
		}
		ast.PostfixExpr {
			p.check_undefined_variables(names, val.expr)!
		}
		ast.PrefixExpr {
			p.check_undefined_variables(names, val.right)!
		}
		ast.SelectorExpr {
			p.check_undefined_variables(names, val.expr)!
		}
		ast.StringInterLiteral {
			for expr_ in val.exprs {
				p.check_undefined_variables(names, expr_)!
			}
		}
		ast.StructInit {
			for field in val.fields {
				p.check_undefined_variables(names, field.expr)!
			}
		}
		ast.UnsafeExpr {
			p.check_undefined_variables(names, val.expr)!
		}
		else {}
	}
}

fn (mut p Parser) check_cross_variables(exprs []ast.Expr, val ast.Expr) bool {
	val_str := val.str()
	match val {
		ast.Ident {
			for expr in exprs {
				if expr is ast.Ident {
					if expr.name == val.name {
						return true
					}
				}
			}
		}
		ast.IndexExpr {
			for expr in exprs {
				if expr.str() == val_str {
					return true
				}
			}
		}
		ast.InfixExpr {
			return p.check_cross_variables(exprs, val.left)
				|| p.check_cross_variables(exprs, val.right)
		}
		ast.ParExpr {
			return p.check_cross_variables(exprs, val.expr)
		}
		ast.CallExpr {
			for arg in val.args {
				if p.check_cross_variables(exprs, arg.expr) {
					return true
				}
			}
		}
		ast.PrefixExpr {
			return p.check_cross_variables(exprs, val.right)
		}
		ast.PostfixExpr {
			return p.check_cross_variables(exprs, val.expr)
		}
		ast.SelectorExpr {
			for expr in exprs {
				if expr.str() == val_str {
					return true
				}
			}
		}
		else {}
	}
	return false
}

fn (mut p Parser) partial_assign_stmt(left []ast.Expr, left_comments []ast.Comment) ast.Stmt {
	p.is_stmt_ident = false
	op := p.tok.kind
	mut pos := p.tok.pos()
	p.next()
	mut comments := []ast.Comment{cap: 2 * left_comments.len + 1}
	comments << left_comments
	comments << p.eat_comments()
	mut right_comments := []ast.Comment{}
	mut right := []ast.Expr{cap: left.len}
	right, right_comments = p.expr_list()
	comments << right_comments
	end_comments := p.eat_comments(same_line: true)
	mut has_cross_var := false
	mut is_static := false
	mut is_volatile := false
	for i, lx_ in left {
		mut lx := unsafe { lx_ }
		match mut lx {
			ast.Ident {
				if op == .decl_assign {
					if p.scope.known_var(lx.name) {
						return p.error_with_pos('redefinition of `$lx.name`', lx.pos)
					}
					mut share := unsafe { ast.ShareType(0) }
					if mut lx.info is ast.IdentVar {
						share = lx.info.share
						if lx.info.is_static {
							if !p.pref.translated && !p.is_translated && !p.pref.is_fmt
								&& !p.inside_unsafe_fn {
								return p.error_with_pos('static variables are supported only in translated mode or in [unsafe] fn',
									lx.pos)
							}
							is_static = true
						}
						if lx.info.is_volatile {
							is_volatile = true
						}
					}
					mut v := ast.Var{
						name: lx.name
						expr: if left.len == right.len { right[i] } else { ast.empty_expr }
						share: share
						is_mut: lx.is_mut || p.inside_for
						pos: lx.pos
						is_stack_obj: p.inside_for
					}
					if p.pref.autofree {
						r0 := right[0]
						if r0 is ast.CallExpr {
							// Set correct variable position (after the or block)
							// so that autofree doesn't free it in cgen before
							// it's declared. (`Or` variables are declared after the or block).
							if r0.or_block.pos.pos > 0 && r0.or_block.stmts.len > 0 {
								v.is_or = true
								// v.pos = r0.or_block.pos.
							}
						}
					}
					obj := ast.ScopeObject(v)
					lx.obj = obj
					p.scope.register(obj)
				}
			}
			ast.IndexExpr {
				if op == .decl_assign {
					return p.error_with_pos('non-name `$lx.left[$lx.index]` on left side of `:=`',
						lx.pos)
				}
				lx.is_setter = true
			}
			ast.ParExpr {}
			ast.PrefixExpr {}
			ast.SelectorExpr {
				if op == .decl_assign {
					return p.error_with_pos('use assignment `=` instead of declaration `:=` when modifying struct fields',
						pos)
				}
			}
			else {}
		}
	}
	if op == .decl_assign {
		// a, b := a + 1, b
		for r in right {
			p.check_undefined_variables(left.map(it.str()), r) or {
				return p.error_with_pos(err.msg(), pos)
			}
		}
	} else if left.len > 1 {
		// a, b = b, a
		for r in right {
			has_cross_var = p.check_cross_variables(left, r)
			if op !in [.assign, .decl_assign] {
				return p.unexpected_with_pos(pos, got: op.str(), expecting: ':= or = or comma')
			}
			if has_cross_var {
				break
			}
		}
	}
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.AssignStmt{
		op: op
		left: left
		right: right
		comments: comments
		end_comments: end_comments
		pos: pos
		has_cross_var: has_cross_var
		is_simple: p.inside_for && p.tok.kind == .lcbr
		is_static: is_static
		is_volatile: is_volatile
	}
}

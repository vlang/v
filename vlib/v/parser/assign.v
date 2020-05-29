// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast

fn (mut p Parser) assign_stmt() ast.Stmt {
	return p.partial_assign_stmt([])
}

fn (mut p Parser) check_undefined_variables(idents []ast.Ident, expr ast.Expr) {
	match expr {
		ast.Ident {
			for ident in idents {
				if ident.name == it.name {
					p.error_with_pos('undefined variable: `$it.name`', it.pos)
				}
			}
		}
		ast.InfixExpr {
			p.check_undefined_variables(idents, it.left)
			p.check_undefined_variables(idents, it.right)
		}
		ast.ParExpr {
			p.check_undefined_variables(idents, it.expr)
		}
		ast.PostfixExpr {
			p.check_undefined_variables(idents, it.expr)
		}
		ast.PrefixExpr {
			p.check_undefined_variables(idents, it.right)
		}
		ast.StringInterLiteral {
			for expr_ in it.exprs {
				p.check_undefined_variables(idents, expr_)
			}
		}
		else {}
	}
}

fn (mut p Parser) check_cross_variables(idents []ast.Ident, expr ast.Expr) bool {
	match expr {
		ast.Ident {
			for ident in idents {
				if ident.name == it.name { return true }
			}
		}
		ast.InfixExpr {
			if p.check_cross_variables(idents, it.left) { return true }
			if p.check_cross_variables(idents, it.right) { return true }
		}
		ast.PrefixExpr {
			if p.check_cross_variables(idents, it.right) { return true }
		}
		ast.PostfixExpr {
			if p.check_cross_variables(idents, it.expr) { return true }
		}
		else {}
	}
	return false
}

fn (mut p Parser) partial_assign_stmt(known_lhs []ast.Ident) ast.Stmt {
	mut idents := known_lhs
	mut op := p.tok.kind
	// read (more) idents until assignment sign
	for op !in [.decl_assign, .assign] {
		idents << p.parse_assign_ident()
		if p.tok.kind == .comma {
			p.next()
		}
		op = p.tok.kind
	}
	p.next()
	pos := p.tok.position()
	exprs := p.parse_assign_rhs()
	is_decl := op == .decl_assign
	mut has_cross_var := false
	if is_decl {
		// a, b := a + 1, b
		for expr in exprs {
			p.check_undefined_variables(idents, expr)
		}
	} else if idents.len > 1 {
		// a, b = b, a
		for expr in exprs {
			if p.check_cross_variables(idents, expr) {
				has_cross_var = true
			}
		}
	}
	for i, ident in idents {
		known_var := p.scope.known_var(ident.name)
		if !is_decl && !known_var {
			p.error('unknown variable `$ident.name`')
		}
		if is_decl && ident.kind != .blank_ident {
			if p.scope.known_var(ident.name) {
				p.error('redefinition of `$ident.name`')
			}
			if idents.len == exprs.len {
				p.scope.register(ident.name, ast.Var{
					name: ident.name
					expr: exprs[i]
					is_mut: ident.is_mut || p.inside_for
					pos: ident.pos
				})
			} else {
				p.scope.register(ident.name, ast.Var{
					name: ident.name
					is_mut: ident.is_mut || p.inside_for
					pos: ident.pos
				})
			}
		}
	}
	return ast.AssignStmt{
		left: idents
		right: exprs
		op: op
		pos: pos
		is_static: false // individual idents may be static
		has_cross_var: has_cross_var
	}
}

// TODO: is it possible to merge with AssignStmt?
pub fn (mut p Parser) assign_expr(left ast.Expr) ast.AssignExpr {
	op := p.tok.kind
	pos := p.tok.position()
	p.next()
	val := p.expr(0)
	match left {
		ast.IndexExpr {
			// it.mark_as_setter()
			it.is_setter = true
		}
		else {}
	}
	node := ast.AssignExpr{
		left: left
		val: val
		op: op
		pos: pos
	}
	return node
}

fn (mut p Parser) parse_assign_ident() ast.Ident {
	/// returns a single parsed ident
	return p.parse_ident(.v)
}

// right hand side of `=` or `:=` in `a,b,c := 1,2,3`
fn (mut p Parser) parse_assign_rhs() []ast.Expr {
	mut exprs := []ast.Expr{}
	for {
		expr := p.expr(0)
		exprs << expr
		if p.tok.kind == .comma {
			p.next()
		} else {
			break
		}
	}
	return exprs
}

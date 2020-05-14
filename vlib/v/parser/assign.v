// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token

fn (mut p Parser) assign_stmt() ast.Stmt {
	return p.partial_assign_stmt([])
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
	return p.parse_ident(false, false)
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

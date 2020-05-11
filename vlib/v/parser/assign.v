// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token

fn (mut p Parser) assign_stmt() ast.Stmt {
    return p.assign_stmt_with_lhs([])
}

fn (mut p Parser) assign_stmt_with_lhs(lhs []ast.Ident) ast.Stmt {
	is_static := p.tok.kind == .key_static
	if is_static {
		p.next()
	}
	mut op := p.tok.kind
	mut idents := lhs
	if lhs.len == 0 {
		idents_, _ := p.parse_assign_lhs_or_conc_expr(true)
		idents = idents_
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
		is_static: is_static
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

fn (mut p Parser) parse_assign_lhs_or_conc_expr(known_assign bool) ([]ast.Ident, []ast.Expr) {
	mut idents := []ast.Ident{}
	mut exprs := []ast.Expr{}
	mut can_be_assign := true
	mut can_be_conc_expr := !known_assign
	for {
		is_mut := p.tok.kind == .key_mut
		if is_mut {
			can_be_conc_expr = false
			p.next()
		}
		is_static := p.tok.kind == .key_static
		if is_static {
			can_be_conc_expr = false
			p.next()
		}
		if p.tok.kind == .name && can_be_assign {
			mut ident := p.parse_ident(false, false)
			ident.is_mut = is_mut
			ident.info = ast.IdentVar{
				is_mut: is_mut
				is_static: is_static
			}
			idents << ident
			if can_be_conc_expr {
				exprs << ident
			}
		} else {
			exprs << p.expr(0)
			can_be_assign = false
		}

		if p.tok.kind == .comma {
			p.next()
		} else {
			break
		}
	}
	if p.tok.kind in [.assign, .decl_assign] {
		can_be_conc_expr = false
	} else {
		can_be_assign = false
	}
	if !can_be_assign {
		idents = []ast.Ident{}
	}
	if !can_be_conc_expr {
		exprs = []ast.Expr{}
	}
	if !can_be_assign && !can_be_conc_expr {
		p.error_with_pos('cannot differentiate assignment from concat-expression', p.peek_tok.position())
	}
	return idents, exprs
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

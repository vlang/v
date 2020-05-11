// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token

fn (mut p Parser) if_expr() ast.IfExpr {
	was_inside_if_expr := p.inside_if_expr
	defer {
		p.inside_if_expr = was_inside_if_expr
	}
	p.inside_if_expr = true
	pos := p.tok.position()
	mut branches := []ast.IfBranch{}
	mut has_else := false
	for p.tok.kind in [.key_if, .key_else] {
		p.inside_if = true
		start_pos := p.tok.position()
		mut comment := ast.Comment{}
		if p.tok.kind == .key_if {
			p.next()
		} else {
			// if p.tok.kind == .comment {
			// p.error('place comments inside {}')
			// }
			// comment = p.check_comment()
			p.check(.key_else)
			if p.tok.kind == .key_if {
				p.next()
			} else {
				has_else = true
				p.inside_if = false
				end_pos := p.prev_tok.position()
				branches << ast.IfBranch{
					stmts: p.parse_block()
					pos: start_pos.extend(end_pos)
					comment: comment
				}
				break
			}
		}
		mut cond := ast.Expr{}
		mut is_or := false
		// `if x := opt() {`
		if p.peek_tok.kind == .decl_assign {
			is_or = true
			p.open_scope()
			var_pos := p.tok.position()
			var_name := p.check_name()
			p.check(.decl_assign)
			expr := p.expr(0)
			p.scope.register(var_name, ast.Var{
				name: var_name
				expr: expr
				pos: var_pos
			})
			cond = ast.IfGuardExpr{
				var_name: var_name
				expr: expr
			}
		} else {
			cond = p.expr(0)
		}
		end_pos := p.prev_tok.position()
		p.inside_if = false
		stmts := p.parse_block()
		if is_or {
			p.close_scope()
		}
		branches << ast.IfBranch{
			cond: cond
			stmts: stmts
			pos: start_pos.extend(end_pos)
			comment: ast.Comment{}
		}
		if p.tok.kind != .key_else {
			break
		}
	}
	return ast.IfExpr{
		branches: branches
		pos: pos
		has_else: has_else
	}
}

fn (mut p Parser) match_expr() ast.MatchExpr {
	match_first_pos := p.tok.position()
	p.inside_match = true
	p.check(.key_match)
	is_mut := p.tok.kind == .key_mut
	mut is_sum_type := false
	if is_mut {
		p.next()
	}
	cond_pos := p.tok.position()
	cond := p.expr(0)
	p.inside_match = false
	p.check(.lcbr)
	mut branches := []ast.MatchBranch{}
	for {
		branch_first_pos := p.tok.position()
		comment := p.check_comment() // comment before {}
		mut exprs := []ast.Expr{}
		p.open_scope()
		// final else
		mut is_else := false
		if p.tok.kind == .key_else {
			is_else = true
			p.next()
		} else if p.tok.kind == .name && !(p.tok.lit == 'C' && p.peek_tok.kind == .dot) &&
			(p.tok.lit in table.builtin_type_names || (p.tok.lit[0].is_capital() && !p.tok.lit.is_upper()) ||
			p.peek_tok.kind == .dot) {
			// Sum type match
			typ := p.parse_type()
			exprs << ast.Type{
				typ: typ
			}
			p.scope.register('it', ast.Var{
				name: 'it'
				typ: typ.to_ptr()
				pos: cond_pos
				is_used: true
				is_mut: is_mut
			})
			// TODO
			if p.tok.kind == .comma {
				p.next()
				p.parse_type()
			}
			is_sum_type = true
			// Make sure a variable used for the sum type match
			mut var_name := ''
			match cond {
				ast.Ident {
					var_name = it.name
				}
				else {
					// p.error('only variables can be used in sum types matches')
				}
			}
			if var_name != '' {
				// Register a shadow variable with the actual type
				// (this replaces the old `it`)
				// TODO doesn't work right now (fixed, uncomment when merging)
				// p.scope.register(var_name, ast.Var{
				// name: var_name
				// typ: typ.to_ptr()
				// })
				// println(var_name)
			}
		} else {
			// Expression match
			for {
				p.inside_match_case = true
				expr := p.expr(0)
				p.inside_match_case = false
				exprs << expr
				if p.tok.kind != .comma {
					break
				}
				p.check(.comma)
			}
		}
		branch_last_pos := p.tok.position()
		// p.warn('match block')
		p.inside_match_body = true
		stmts := p.parse_block()
		p.inside_match_body = false
		pos := token.Position{
			line_nr: branch_first_pos.line_nr
			pos: branch_first_pos.pos
			len: branch_last_pos.pos - branch_first_pos.pos + branch_last_pos.len
		}
		branches << ast.MatchBranch{
			exprs: exprs
			stmts: stmts
			pos: pos
			comment: comment
			is_else: is_else
		}
		p.close_scope()
		if p.tok.kind == .rcbr {
			break
		}
	}
	match_last_pos := p.tok.position()
	pos := token.Position{
		line_nr: match_first_pos.line_nr
		pos: match_first_pos.pos
		len: match_last_pos.pos - match_first_pos.pos + match_last_pos.len
	}
	p.check(.rcbr)
	return ast.MatchExpr{
		branches: branches
		cond: cond
		is_sum_type: is_sum_type
		pos: pos
		is_mut: is_mut
	}
}

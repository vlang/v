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
	mut comments := []ast.Comment{}
	mut prev_guard := false
	for p.tok.kind in [.key_if, .key_else] {
		p.inside_if = true
		start_pos := p.tok.position()
		if p.tok.kind == .key_if {
			p.next()
		} else {
			comments << p.eat_comments()
			p.check(.key_else)
			comments << p.eat_comments()
			if p.tok.kind == .key_if {
				p.next()
			} else {
				// else {
				has_else = true
				p.inside_if = false
				end_pos := p.prev_tok.position()
				body_pos := p.tok.position()
				// only declare `err` if previous branch was an `if` guard
				if prev_guard {
					p.open_scope()
					p.scope.register('errcode', ast.Var{
						name: 'errcode'
						typ: table.int_type
						pos: body_pos
						is_used: true
					})
					p.scope.register('err', ast.Var{
						name: 'err'
						typ: table.string_type
						pos: body_pos
						is_used: true
					})
				}
				branches << ast.IfBranch{
					stmts: if prev_guard { p.parse_block_no_scope(false) } else { p.parse_block() }
					pos: start_pos.extend(end_pos)
					body_pos: body_pos.extend(p.tok.position())
					comments: comments
				}
				if prev_guard {
					p.close_scope()
				}
				comments = []
				break
			}
		}
		mut cond := ast.Expr{}
		mut is_guard := false
		comments << p.eat_comments()
		// `if x := opt() {`
		if p.peek_tok.kind == .decl_assign {
			p.open_scope()
			is_guard = true
			var_pos := p.tok.position()
			var_name := p.check_name()
			comments << p.eat_comments()
			p.check(.decl_assign)
			comments << p.eat_comments()
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
			prev_guard = true
		} else {
			prev_guard = false
			cond = p.expr(0)
		}
		comments << p.eat_comments()
		mut left_as_name := ''
		if cond is ast.InfixExpr as infix {
			// if sum is T
			is_is_cast := infix.op == .key_is
			is_ident := infix.left is ast.Ident
			left_as_name = if is_is_cast && p.tok.kind == .key_as {
				p.next()
				p.check_name()
			} else if is_ident {
				ident := infix.left as ast.Ident
				ident.name
			} else {
				''
			}
		}
		end_pos := p.prev_tok.position()
		body_pos := p.tok.position()
		p.inside_if = false
		stmts := p.parse_block()
		if is_guard {
			p.close_scope()
		}
		branches << ast.IfBranch{
			cond: cond
			stmts: stmts
			pos: start_pos.extend(end_pos)
			body_pos: body_pos.extend(p.prev_tok.position())
			comments: comments
			left_as_name: left_as_name
		}
		comments = p.eat_comments()
		if p.tok.kind != .key_else {
			break
		}
	}
	return ast.IfExpr{
		branches: branches
		post_comments: comments
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
	mut var_name := ''
	if p.tok.kind == .key_as {
		p.next()
		var_name = p.check_name()
	}
	no_lcbr := p.tok.kind != .lcbr
	if !no_lcbr {
		p.check(.lcbr)
	}
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
		} else if p.tok.kind == .name && !(p.tok.lit == 'C' &&
			p.peek_tok.kind == .dot) && (p.tok.lit in table.builtin_type_names || p.tok.lit[0].is_capital() ||
			(p.peek_tok.kind == .dot && p.peek_tok2.lit[0].is_capital())) {
			if var_name.len == 0 {
				match cond {
					ast.Ident {
						// shadow match cond variable
						var_name = cond.name
					}
					else {
						// ast.SelectorExpr {
						// p.error('expecting `as` (eg. `match user.attribute as user_attr`) when matching struct fields')
						// }
						// p.error('only variables can be used in sum types matches')
					}
				}
			}
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
			if var_name.len > 0 {
				// Register shadow variable or `as` variable with actual type
				p.scope.register(var_name, ast.Var{
					name: var_name
					typ: typ.to_ptr()
					pos: cond_pos
					is_used: true
					is_mut: is_mut
				})
			}
			// TODO
			if p.tok.kind == .comma {
				p.next()
				p.parse_type()
			}
			is_sum_type = true
		} else {
			// Expression match
			for {
				p.inside_match_case = true
				expr := p.expr(0)
				p.inside_match_case = false
				if p.tok.kind == .dotdot {
					p.error_with_pos('match only supports inclusive (`...`) ranges, not exclusive (`..`)',
						p.tok.position())
				} else if p.tok.kind == .ellipsis {
					p.next()
					expr2 := p.expr(0)
					exprs << ast.RangeExpr{
						low: expr
						high: expr2
						has_low: true
						has_high: true
					}
				} else {
					exprs << expr
				}
				if p.tok.kind != .comma {
					break
				}
				p.check(.comma)
			}
		}
		branch_last_pos := p.tok.position()
		// p.warn('match block')
		p.inside_match_body = true
		stmts := p.parse_block_no_scope(false)
		p.close_scope()
		p.inside_match_body = false
		pos := token.Position{
			line_nr: branch_first_pos.line_nr
			pos: branch_first_pos.pos
			len: branch_last_pos.pos - branch_first_pos.pos + branch_last_pos.len
		}
		post_comments := p.eat_comments()
		branches << ast.MatchBranch{
			exprs: exprs
			stmts: stmts
			pos: pos
			comment: comment
			is_else: is_else
			post_comments: post_comments
		}
		if p.tok.kind == .rcbr || (is_else && no_lcbr) {
			break
		}
	}
	match_last_pos := p.tok.position()
	pos := token.Position{
		line_nr: match_first_pos.line_nr
		pos: match_first_pos.pos
		len: match_last_pos.pos - match_first_pos.pos + match_last_pos.len
	}
	if p.tok.kind == .rcbr {
		p.check(.rcbr)
	}
	// return ast.StructInit{}
	return ast.MatchExpr{
		branches: branches
		cond: cond
		is_sum_type: is_sum_type
		pos: pos
		is_mut: is_mut
		var_name: var_name
	}
}

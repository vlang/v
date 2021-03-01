// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token

fn (mut p Parser) if_expr(is_comptime bool) ast.IfExpr {
	was_inside_if_expr := p.inside_if_expr
	was_inside_ct_if_expr := p.inside_ct_if_expr
	defer {
		p.inside_if_expr = was_inside_if_expr
		p.inside_ct_if_expr = was_inside_ct_if_expr
	}
	p.inside_if_expr = true
	is_expr := p.prev_tok.kind == .key_return
	mut pos := p.tok.position()
	if is_comptime {
		p.inside_ct_if_expr = true
		p.next() // `$`
		pos = p.prev_tok.position().extend(p.tok.position())
	}
	mut branches := []ast.IfBranch{}
	mut has_else := false
	mut comments := []ast.Comment{}
	mut prev_guard := false
	for p.tok.kind in [.key_if, .key_else] {
		p.inside_if = true
		start_pos := if is_comptime {
			p.prev_tok.position().extend(p.tok.position())
		} else {
			p.tok.position()
		}
		if p.tok.kind == .key_else {
			comments << p.eat_comments({})
			p.check(.key_else)
			comments << p.eat_comments({})
			if p.tok.kind == .key_match {
				p.error('cannot use `match` with `if` statements')
				return ast.IfExpr{}
			}
			if p.tok.kind == .lcbr {
				// else {
				has_else = true
				p.inside_if = false
				end_pos := p.prev_tok.position()
				body_pos := p.tok.position()
				p.open_scope()
				// only declare `err` if previous branch was an `if` guard
				if prev_guard {
					p.scope.register(ast.Var{
						name: 'err'
						typ: table.error_type
						pos: p.tok.position()
						is_used: true
					})
				}
				branches << ast.IfBranch{
					stmts: p.parse_block_no_scope(false)
					pos: start_pos.extend(end_pos)
					body_pos: body_pos.extend(p.tok.position())
					comments: comments
					scope: p.scope
				}
				p.close_scope()
				comments = []
				break
			}
			if is_comptime {
				p.check(.dollar)
			}
		}
		// `if` or `else if`
		p.check(.key_if)
		if p.tok.kind == .key_match {
			p.error('cannot use `match` with `if` statements')
			return ast.IfExpr{}
		}
		comments << p.eat_comments({})
		mut cond := ast.Expr{}
		mut is_guard := false
		// `if x := opt() {`
		if !is_comptime && p.peek_tok.kind == .decl_assign {
			p.open_scope()
			is_guard = true
			var_pos := p.tok.position()
			var_name := p.check_name()
			comments << p.eat_comments({})
			p.check(.decl_assign)
			comments << p.eat_comments({})
			expr := p.expr(0)
			cond = ast.IfGuardExpr{
				var_name: var_name
				expr: expr
			}
			p.scope.register(ast.Var{
				name: var_name
				expr: cond
				pos: var_pos
			})
			prev_guard = true
		} else {
			prev_guard = false
			cond = p.expr(0)
		}
		comments << p.eat_comments({})
		end_pos := p.prev_tok.position()
		body_pos := p.tok.position()
		p.inside_if = false
		p.open_scope()
		stmts := p.parse_block_no_scope(false)
		branches << ast.IfBranch{
			cond: cond
			stmts: stmts
			pos: start_pos.extend(end_pos)
			body_pos: body_pos.extend(p.prev_tok.position())
			comments: comments
			scope: p.scope
		}
		p.close_scope()
		if is_guard {
			p.close_scope()
		}
		comments = p.eat_comments({})
		if is_comptime {
			if p.tok.kind == .key_else {
				p.error('use `\$else` instead of `else` in compile-time `if` branches')
				return ast.IfExpr{}
			}
			if p.peek_tok.kind == .key_else {
				p.check(.dollar)
			}
		}
		if p.tok.kind != .key_else {
			break
		}
	}
	pos.update_last_line(p.prev_tok.line_nr)
	if comments.len > 0 {
		pos.last_line = comments.last().pos.last_line
	}
	return ast.IfExpr{
		is_comptime: is_comptime
		branches: branches
		post_comments: comments
		pos: pos
		has_else: has_else
		is_expr: is_expr
	}
}

fn (mut p Parser) match_expr() ast.MatchExpr {
	match_first_pos := p.tok.position()
	p.inside_match = true
	p.check(.key_match)
	mut is_sum_type := false
	cond := p.expr(0)
	p.inside_match = false
	no_lcbr := p.tok.kind != .lcbr
	if !no_lcbr {
		p.check(.lcbr)
	}
	comments := p.eat_comments({}) // comments before the first branch
	mut branches := []ast.MatchBranch{}
	for p.tok.kind != .eof {
		branch_first_pos := p.tok.position()
		mut exprs := []ast.Expr{}
		mut ecmnts := [][]ast.Comment{}
		p.open_scope()
		// final else
		mut is_else := false
		if p.tok.kind == .key_else {
			is_else = true
			p.next()
		} else if (p.tok.kind == .name && !(p.tok.lit == 'C' && p.peek_tok.kind == .dot)
			&& (p.tok.lit in table.builtin_type_names || p.tok.lit[0].is_capital()
			|| (p.peek_tok.kind == .dot && p.peek_token(2).lit.len > 0
			&& p.peek_token(2).lit[0].is_capital()))) || p.tok.kind == .lsbr {
			mut types := []table.Type{}
			for {
				// Sum type match
				parsed_type := p.parse_type()
				ecmnts << p.eat_comments({})
				types << parsed_type
				exprs << ast.Type{
					typ: parsed_type
					pos: p.prev_tok.position()
				}
				if p.tok.kind != .comma {
					break
				}
				p.check(.comma)
			}
			is_sum_type = true
		} else {
			// Expression match
			for {
				p.inside_match_case = true
				expr := p.expr(0)
				ecmnts << p.eat_comments({})
				p.inside_match_case = false
				if p.tok.kind == .dotdot {
					p.error_with_pos('match only supports inclusive (`...`) ranges, not exclusive (`..`)',
						p.tok.position())
					return ast.MatchExpr{}
				} else if p.tok.kind == .ellipsis {
					p.next()
					expr2 := p.expr(0)
					exprs << ast.RangeExpr{
						low: expr
						high: expr2
						has_low: true
						has_high: true
						pos: p.tok.position()
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
		branch_last_pos := p.prev_tok.position()
		// p.warn('match block')
		p.inside_match_body = true
		stmts := p.parse_block_no_scope(false)
		branch_scope := p.scope
		p.close_scope()
		p.inside_match_body = false
		pos := branch_first_pos.extend_with_last_line(branch_last_pos, p.prev_tok.line_nr)
		post_comments := p.eat_comments({})
		branches << ast.MatchBranch{
			exprs: exprs
			ecmnts: ecmnts
			stmts: stmts
			pos: pos
			is_else: is_else
			post_comments: post_comments
			scope: branch_scope
		}
		if is_else && branches.len == 1 {
			p.warn_with_pos('`match` must have at least one non `else` branch', pos)
		}
		if p.tok.kind == .rcbr || (is_else && no_lcbr) {
			break
		}
	}
	match_last_pos := p.tok.position()
	mut pos := token.Position{
		line_nr: match_first_pos.line_nr
		pos: match_first_pos.pos
		len: match_last_pos.pos - match_first_pos.pos + match_last_pos.len
	}
	if p.tok.kind == .rcbr {
		p.check(.rcbr)
	}
	// return ast.StructInit{}
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.MatchExpr{
		branches: branches
		cond: cond
		is_sum_type: is_sum_type
		pos: pos
		comments: comments
	}
}

fn (mut p Parser) select_expr() ast.SelectExpr {
	match_first_pos := p.tok.position()
	p.check(.key_select)
	no_lcbr := p.tok.kind != .lcbr
	if !no_lcbr {
		p.check(.lcbr)
	}
	mut branches := []ast.SelectBranch{}
	mut has_else := false
	mut has_timeout := false
	for {
		branch_first_pos := p.tok.position()
		comment := p.check_comment() // comment before {}
		p.open_scope()
		// final else
		mut is_else := false
		mut is_timeout := false
		mut stmt := ast.Stmt{}
		if p.tok.kind == .key_else {
			if has_timeout {
				p.error_with_pos('timeout `> t` and `else` are mutually exclusive `select` keys',
					p.tok.position())
				return ast.SelectExpr{}
			}
			if has_else {
				p.error_with_pos('at most one `else` branch allowed in `select` block',
					p.tok.position())
				return ast.SelectExpr{}
			}
			is_else = true
			has_else = true
			p.next()
		} else if p.tok.kind == .gt {
			if has_else {
				p.error_with_pos('`else` and timeout `> t` are mutually exclusive `select` keys',
					p.tok.position())
				return ast.SelectExpr{}
			}
			if has_timeout {
				p.error_with_pos('at most one timeout `> t` branch allowed in `select` block',
					p.tok.position())
				return ast.SelectExpr{}
			}
			is_timeout = true
			has_timeout = true
			p.next()
			p.inside_match = true
			expr := p.expr(0)
			p.inside_match = false
			stmt = ast.ExprStmt{
				expr: expr
				pos: expr.position()
				comments: [comment]
				is_expr: true
			}
		} else {
			p.inside_match = true
			p.inside_select = true
			exprs, comments := p.expr_list()
			if exprs.len != 1 {
				p.error('only one expression allowed as `select` key')
				return ast.SelectExpr{}
			}
			if p.tok.kind in [.assign, .decl_assign] {
				stmt = p.partial_assign_stmt(exprs, comments)
			} else {
				stmt = ast.ExprStmt{
					expr: exprs[0]
					pos: exprs[0].position()
					comments: [comment]
					is_expr: true
				}
			}
			p.inside_match = false
			p.inside_select = false
			match mut stmt {
				ast.ExprStmt {
					if !stmt.is_expr {
						p.error_with_pos('select: invalid expression', stmt.pos)
						return ast.SelectExpr{}
					} else {
						match mut stmt.expr {
							ast.InfixExpr {
								if stmt.expr.op != .arrow {
									p.error_with_pos('select key: `<-` operator expected',
										stmt.expr.pos)
									return ast.SelectExpr{}
								}
							}
							else {
								p.error_with_pos('select key: send expression (`ch <- x`) expected',
									stmt.pos)
								return ast.SelectExpr{}
							}
						}
					}
				}
				ast.AssignStmt {
					expr := stmt.right[0]
					match expr {
						ast.PrefixExpr {
							if expr.op != .arrow {
								p.error_with_pos('select key: `<-` operator expected',
									expr.pos)
								return ast.SelectExpr{}
							}
						}
						else {
							p.error_with_pos('select key: receive expression expected',
								stmt.right[0].position())
							return ast.SelectExpr{}
						}
					}
				}
				else {
					p.error_with_pos('select: transmission statement expected', stmt.position())
					return ast.SelectExpr{}
				}
			}
		}
		branch_last_pos := p.tok.position()
		p.inside_match_body = true
		stmts := p.parse_block_no_scope(false)
		p.close_scope()
		p.inside_match_body = false
		mut pos := token.Position{
			line_nr: branch_first_pos.line_nr
			pos: branch_first_pos.pos
			len: branch_last_pos.pos - branch_first_pos.pos + branch_last_pos.len
		}
		post_comments := p.eat_comments({})
		pos.update_last_line(p.prev_tok.line_nr)
		if post_comments.len > 0 {
			pos.last_line = post_comments.last().pos.last_line
		}
		branches << ast.SelectBranch{
			stmt: stmt
			stmts: stmts
			pos: pos
			comment: comment
			is_else: is_else
			is_timeout: is_timeout
			post_comments: post_comments
		}
		if p.tok.kind == .rcbr || ((is_else || is_timeout) && no_lcbr) {
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
	return ast.SelectExpr{
		branches: branches
		pos: pos.extend_with_last_line(p.prev_tok.position(), p.prev_tok.line_nr)
		has_exception: has_else || has_timeout
	}
}

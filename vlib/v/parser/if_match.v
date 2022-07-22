// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
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
	mut pos := p.tok.pos()
	if is_comptime {
		p.inside_ct_if_expr = true
		p.next() // `$`
		pos = p.prev_tok.pos().extend(p.tok.pos())
	}
	mut branches := []ast.IfBranch{}
	mut has_else := false
	mut comments := []ast.Comment{}
	mut prev_guard := false
	for p.tok.kind in [.key_if, .key_else] {
		p.inside_if = true
		if is_comptime {
			p.inside_comptime_if = true
		}
		start_pos := if is_comptime { p.prev_tok.pos().extend(p.tok.pos()) } else { p.tok.pos() }
		if p.tok.kind == .key_else {
			comments << p.eat_comments()
			p.check(.key_else)
			comments << p.eat_comments()
			if p.tok.kind == .key_match {
				p.error('cannot use `match` with `if` statements')
				return ast.IfExpr{}
			}
			if p.tok.kind == .lcbr {
				// else {
				has_else = true
				p.inside_if = false
				p.inside_comptime_if = false
				end_pos := p.prev_tok.pos()
				body_pos := p.tok.pos()
				p.open_scope()
				// only declare `err` if previous branch was an `if` guard
				if prev_guard {
					p.scope.register(ast.Var{
						name: 'err'
						typ: ast.error_type
						pos: p.tok.pos()
						is_used: true
						is_stack_obj: true
					})
				}
				branches << ast.IfBranch{
					stmts: p.parse_block_no_scope(false)
					pos: start_pos.extend(end_pos)
					body_pos: body_pos.extend(p.tok.pos())
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
		comments << p.eat_comments()
		mut cond := ast.empty_expr
		mut is_guard := false

		// if guard `if x,y := opt() {`
		if !is_comptime && p.peek_token_after_var_list().kind == .decl_assign {
			p.open_scope()
			is_guard = true
			mut vars := []ast.IfGuardVar{}
			for {
				mut var := ast.IfGuardVar{}
				mut is_mut := false
				if p.tok.kind == .key_mut {
					is_mut = true
					p.next()
				}
				var.is_mut = is_mut
				var.pos = p.tok.pos()
				var.name = p.check_name()

				if p.scope.known_var(var.name) {
					p.error_with_pos('redefinition of `$var.name`', var.pos)
				}
				vars << var
				if p.tok.kind != .comma {
					break
				}
				p.next()
			}
			comments << p.eat_comments()
			p.check(.decl_assign)
			comments << p.eat_comments()
			expr := p.expr(0)
			if expr !in [ast.CallExpr, ast.IndexExpr, ast.PrefixExpr] {
				p.error_with_pos('if guard condition expression is illegal, it should return optional',
					expr.pos())
			}

			cond = ast.IfGuardExpr{
				vars: vars
				expr: expr
			}
			for var in vars {
				p.scope.register(ast.Var{
					name: var.name
					is_mut: var.is_mut
					expr: cond
					pos: var.pos
				})
			}
			prev_guard = true
		} else {
			prev_guard = false
			p.comptime_if_cond = true
			p.inside_if_cond = true
			cond = p.expr(0)
			p.inside_if_cond = false
			if p.if_cond_comments.len > 0 {
				comments << p.if_cond_comments
				p.if_cond_comments = []
			}
			p.comptime_if_cond = false
		}
		comments << p.eat_comments()
		end_pos := p.prev_tok.pos()
		body_pos := p.tok.pos()
		p.inside_if = false
		p.inside_comptime_if = false
		p.open_scope()
		stmts := p.parse_block_no_scope(false)
		branches << ast.IfBranch{
			cond: cond
			stmts: stmts
			pos: start_pos.extend(end_pos)
			body_pos: body_pos.extend(p.prev_tok.pos())
			comments: comments
			scope: p.scope
		}
		p.close_scope()
		if is_guard {
			p.close_scope()
		}
		comments = p.eat_comments()
		if is_comptime {
			if p.tok.kind == .key_else {
				p.error('use `\$else` instead of `else` in compile-time `if` branches')
				return ast.IfExpr{}
			}
			if p.tok.kind != .rcbr && p.peek_tok.kind == .key_else {
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

fn (mut p Parser) is_only_array_type() bool {
	if p.tok.kind == .lsbr {
		for i in 1 .. 20 {
			if p.peek_token(i).kind == .rsbr {
				next_kind := p.peek_token(i + 1).kind
				if next_kind == .name {
					return true
				} else if next_kind == .lsbr {
					continue
				} else {
					return false
				}
			}
		}
	}
	return false
}

fn (mut p Parser) match_expr() ast.MatchExpr {
	match_first_pos := p.tok.pos()
	p.inside_match = true
	p.check(.key_match)
	mut is_sum_type := false
	cond := p.expr(0)
	p.inside_match = false
	no_lcbr := p.tok.kind != .lcbr
	if !no_lcbr {
		p.check(.lcbr)
	}
	comments := p.eat_comments() // comments before the first branch
	mut branches := []ast.MatchBranch{}
	for p.tok.kind != .eof {
		branch_first_pos := p.tok.pos()
		mut exprs := []ast.Expr{}
		mut ecmnts := [][]ast.Comment{}
		p.open_scope()
		// final else
		mut is_else := false
		if p.tok.kind == .key_else {
			is_else = true
			p.next()
		} else if (p.tok.kind == .name && !(p.tok.lit == 'C' && p.peek_tok.kind == .dot)
			&& (((ast.builtin_type_names_matcher.find(p.tok.lit) > 0 || p.tok.lit[0].is_capital())
			&& p.peek_tok.kind != .lpar) || (p.peek_tok.kind == .dot && p.peek_token(2).lit.len > 0
			&& p.peek_token(2).lit[0].is_capital()))) || p.is_only_array_type() {
			mut types := []ast.Type{}
			for {
				// Sum type match
				parsed_type := p.parse_type()
				ecmnts << p.eat_comments()
				types << parsed_type
				exprs << ast.TypeNode{
					typ: parsed_type
					pos: p.prev_tok.pos()
				}
				if p.tok.kind != .comma {
					break
				}
				p.check(.comma)
				if p.pref.is_fmt {
					if p.tok.kind == .lcbr {
						break
					}
					for p.tok.kind == .comma {
						p.next()
					}
				}
			}
			is_sum_type = true
		} else {
			// Expression match
			for {
				p.inside_match_case = true
				expr := p.expr(0)
				ecmnts << p.eat_comments()
				p.inside_match_case = false
				if p.tok.kind == .dotdot {
					p.error_with_pos('match only supports inclusive (`...`) ranges, not exclusive (`..`)',
						p.tok.pos())
					return ast.MatchExpr{}
				} else if p.tok.kind == .ellipsis {
					p.next()
					expr2 := p.expr(0)
					exprs << ast.RangeExpr{
						low: expr
						high: expr2
						has_low: true
						has_high: true
						pos: p.tok.pos()
					}
				} else {
					exprs << expr
				}
				if p.tok.kind != .comma {
					break
				}

				p.check(.comma)
				if p.pref.is_fmt {
					if p.tok.kind == .lcbr {
						break
					}
					for p.tok.kind == .comma {
						p.next()
					}
				}
			}
		}
		branch_last_pos := p.prev_tok.pos()
		// p.warn('match block')
		p.inside_match_body = true
		stmts := p.parse_block_no_scope(false)
		branch_scope := p.scope
		p.close_scope()
		p.inside_match_body = false
		pos := branch_first_pos.extend_with_last_line(branch_last_pos, p.prev_tok.line_nr)
		branch_pos := branch_first_pos.extend_with_last_line(p.tok.pos(), p.tok.line_nr)
		post_comments := p.eat_comments()
		branches << ast.MatchBranch{
			exprs: exprs
			ecmnts: ecmnts
			stmts: stmts
			pos: pos
			branch_pos: branch_pos
			is_else: is_else
			post_comments: post_comments
			scope: branch_scope
		}
		if is_else && branches.len == 1 {
			p.error_with_pos('`match` must have at least one non `else` branch', pos)
		}
		if p.tok.kind == .rcbr || (is_else && no_lcbr) {
			break
		}
	}
	match_last_pos := p.tok.pos()
	mut pos := token.Pos{
		line_nr: match_first_pos.line_nr
		pos: match_first_pos.pos
		len: match_last_pos.pos - match_first_pos.pos + match_last_pos.len
		col: match_first_pos.col
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
	match_first_pos := p.tok.pos()
	p.check(.key_select)
	no_lcbr := p.tok.kind != .lcbr
	if !no_lcbr {
		p.check(.lcbr)
	}
	mut branches := []ast.SelectBranch{}
	mut has_else := false
	mut has_timeout := false
	for {
		branch_first_pos := p.tok.pos()
		comment := p.check_comment() // comment before {}
		p.open_scope()
		// final else
		mut is_else := false
		mut is_timeout := false
		mut stmt := ast.empty_stmt
		if p.tok.kind == .key_else {
			if has_timeout {
				p.error_with_pos('timeout `> t` and `else` are mutually exclusive `select` keys',
					p.tok.pos())
				return ast.SelectExpr{}
			}
			if has_else {
				p.error_with_pos('at most one `else` branch allowed in `select` block',
					p.tok.pos())
				return ast.SelectExpr{}
			}
			is_else = true
			has_else = true
			p.next()
		} else {
			mut is_gt := false
			if p.tok.kind == .gt {
				is_gt = true
				p.note_with_pos('`>` is deprecated and will soon be forbidden - just state the timeout in nanoseconds',
					p.tok.pos())
				p.next()
			}
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
					pos: exprs[0].pos()
					comments: [comment]
					is_expr: true
				}
			}
			p.inside_match = false
			p.inside_select = false
			match mut stmt {
				ast.ExprStmt {
					mut check_timeout := false
					if !stmt.is_expr {
						p.error_with_pos('select: invalid expression', stmt.pos)
						return ast.SelectExpr{}
					} else {
						match mut stmt.expr {
							ast.InfixExpr {
								if stmt.expr.op != .arrow {
									check_timeout = true
								} else if is_gt {
									p.error_with_pos('send expression cannot be used as timeout',
										stmt.pos)
								}
							}
							else {
								check_timeout = true
							}
						}
					}
					if check_timeout {
						if has_else {
							p.error_with_pos('`else` and timeout value are mutually exclusive `select` keys',
								stmt.pos)
							return ast.SelectExpr{}
						}
						if has_timeout {
							p.error_with_pos('at most one timeout branch allowed in `select` block',
								stmt.pos)
							return ast.SelectExpr{}
						}
						is_timeout = true
						has_timeout = true
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
								stmt.right[0].pos())
							return ast.SelectExpr{}
						}
					}
				}
				else {
					p.error_with_pos('select: transmission statement, timeout (in ns) or `else` expected',
						stmt.pos)
					return ast.SelectExpr{}
				}
			}
		}
		branch_last_pos := p.tok.pos()
		p.inside_match_body = true
		stmts := p.parse_block_no_scope(false)
		p.close_scope()
		p.inside_match_body = false
		mut pos := token.Pos{
			line_nr: branch_first_pos.line_nr
			pos: branch_first_pos.pos
			len: branch_last_pos.pos - branch_first_pos.pos + branch_last_pos.len
			col: branch_first_pos.col
		}
		post_comments := p.eat_comments()
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
	match_last_pos := p.tok.pos()
	pos := token.Pos{
		line_nr: match_first_pos.line_nr
		pos: match_first_pos.pos
		len: match_last_pos.pos - match_first_pos.pos + match_last_pos.len
		col: match_first_pos.col
	}
	if p.tok.kind == .rcbr {
		p.check(.rcbr)
	}
	return ast.SelectExpr{
		branches: branches
		pos: pos.extend_with_last_line(p.prev_tok.pos(), p.prev_tok.line_nr)
		has_exception: has_else || has_timeout
	}
}

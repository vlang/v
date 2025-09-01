// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token
import v.pref
import v.pkgconfig

fn (mut p Parser) if_expr(is_comptime bool, is_expr bool) ast.IfExpr {
	was_inside_if_expr := p.inside_if_expr
	was_inside_ct_if_expr := p.inside_ct_if_expr
	defer {
		p.inside_if_expr = was_inside_if_expr
		p.inside_ct_if_expr = was_inside_ct_if_expr
	}
	p.inside_if_expr = true
	is_expr_ := p.prev_tok.kind == .key_return || is_expr
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
	mut comptime_skip_curr_stmts := false
	mut comptime_has_true_branch := false
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
						name:         'err'
						typ:          ast.error_type
						pos:          p.tok.pos()
						is_used:      false
						is_stack_obj: true
						is_special:   true
					})
				}
				if is_comptime && comptime_has_true_branch && !p.pref.is_fmt
					&& !p.pref.output_cross_c {
					p.skip_scope()
					branches << ast.IfBranch{
						pos:      start_pos.extend(end_pos)
						body_pos: body_pos.extend(p.tok.pos())
						comments: comments
						scope:    p.scope
					}
				} else {
					branches << ast.IfBranch{
						stmts:    p.parse_block_no_scope(false)
						pos:      start_pos.extend(end_pos)
						body_pos: body_pos.extend(p.tok.pos())
						comments: comments
						scope:    p.scope
					}
				}
				p.close_scope()
				comments = []
				break
			}
			if is_comptime {
				p.check(.dollar)
			}
		}
		if_pos := p.tok.pos()
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
			mut var_names := []string{}
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
				var_names << var.name

				if p.scope.known_var(var.name) {
					p.error_with_pos('redefinition of `${var.name}`', var.pos)
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
			old_assign_rhs := p.inside_assign_rhs
			p.inside_assign_rhs = true
			expr := p.expr(0)
			p.inside_assign_rhs = old_assign_rhs
			if expr !in [ast.CallExpr, ast.IndexExpr, ast.PrefixExpr, ast.SelectorExpr, ast.Ident] {
				p.error_with_pos('if guard condition expression is illegal, it should return an Option',
					expr.pos())
			}
			p.check_undefined_variables(var_names, expr) or {
				p.error_with_pos(err.msg(), pos)
				break
			}
			cond = ast.IfGuardExpr{
				vars: vars
				expr: expr
			}
			for var in vars {
				p.scope.register(ast.Var{
					name:   var.name
					is_mut: var.is_mut
					expr:   cond
					pos:    var.pos
				})
			}
			prev_guard = true
		} else {
			prev_guard = false
			p.comptime_if_cond = true
			p.inside_if_cond = true
			cond = p.expr(0)
			if is_comptime && p.is_in_top_level_comptime(p.inside_assign_rhs) {
				comptime_skip_curr_stmts = !p.comptime_if_cond(mut cond)
				if !comptime_skip_curr_stmts {
					comptime_has_true_branch = true
				}
			}
			if mut cond is ast.InfixExpr && !is_comptime {
				if cond.op in [.key_is, .not_is] {
					if mut cond.left is ast.Ident {
						if cond.left.name.len == 1 && cond.left.name.is_capital()
							&& cond.right is ast.TypeNode {
							p.error_with_pos('use `\$if` instead of `if`', if_pos)
							return ast.IfExpr{}
						}
					}
				}
			}
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
		if p.opened_scopes > p.max_opened_scopes {
			p.should_abort = true
			p.error('too many nested conditionals, scopes: ${p.opened_scopes}')
			return ast.IfExpr{}
		}
		p.open_scope()
		if is_comptime && comptime_skip_curr_stmts && !p.pref.is_fmt && !p.pref.output_cross_c {
			p.skip_scope()
			branches << ast.IfBranch{
				cond:     cond
				pos:      start_pos.extend(end_pos)
				body_pos: body_pos.extend(p.prev_tok.pos())
				comments: comments
				scope:    p.scope
			}
		} else {
			stmts := p.parse_block_no_scope(false)
			branches << ast.IfBranch{
				cond:     cond
				stmts:    stmts
				pos:      start_pos.extend(end_pos)
				body_pos: body_pos.extend(p.prev_tok.pos())
				comments: comments
				scope:    p.scope
			}
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
		is_comptime:   is_comptime
		branches:      branches
		post_comments: comments
		pos:           pos
		has_else:      has_else
		is_expr:       is_expr_
	}
}

fn (mut p Parser) is_only_array_type() bool {
	if p.tok.kind == .lsbr {
		for i in 1 .. 20 {
			if p.peek_token(i).kind == .rsbr {
				next_kind := p.peek_token(i + 1).kind
				if next_kind == .name {
					return true
				} else if next_kind == .question && p.peek_token(i + 2).kind == .name {
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

fn (mut p Parser) is_match_sumtype_type() bool {
	is_option := p.tok.kind == .question
	name_tok := if is_option { p.peek_tok } else { p.tok }
	next_tok_kind := if is_option { p.peek_token(2).kind } else { p.peek_tok.kind }
	next_next_idx := if is_option { 3 } else { 2 }
	next_next_tok := p.peek_token(next_next_idx)
	return name_tok.kind == .name && !(name_tok.lit == 'C' && next_tok_kind == .dot)
		&& (((ast.builtin_type_names_matcher.matches(name_tok.lit) || name_tok.lit[0].is_capital())
		&& next_tok_kind != .lpar && !(next_tok_kind == .dot && next_next_tok.kind == .name
		&& p.peek_token(next_next_idx + 1).kind == .lpar)) || (next_tok_kind == .dot
		&& next_next_tok.lit.len > 0 && next_next_tok.lit[0].is_capital()))
}

fn (mut p Parser) match_expr(is_comptime bool) ast.MatchExpr {
	mut match_first_pos := p.tok.pos()
	if is_comptime {
		p.next() // `$`
		match_first_pos = p.prev_tok.pos().extend(p.tok.pos())
	}
	old_inside_match := p.inside_match
	p.inside_match = true
	p.check(.key_match)
	mut is_sum_type := false
	cond := p.expr(0)
	p.inside_match = old_inside_match
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
		if is_comptime {
			if p.tok.kind == .key_else {
				p.error('use `\$else` instead of `else` in compile-time `match` branches')
				return ast.MatchExpr{}
			}
			if p.tok.kind != .rcbr && p.peek_tok.kind == .key_else {
				p.check(.dollar)
			}
		}
		if p.tok.kind == .key_else {
			is_else = true
			p.next()
		} else if p.is_match_sumtype_type() || p.is_only_array_type()
			|| p.tok.kind == .key_fn
			|| (p.tok.kind == .lsbr && p.peek_token(2).kind == .amp) {
			mut types := []ast.Type{}
			for {
				// Sum type match
				parsed_type := p.parse_type()
				types << parsed_type
				exprs << ast.TypeNode{
					typ: parsed_type
					pos: p.prev_tok.pos()
				}
				if p.tok.kind != .comma {
					ecmnts << p.eat_comments()
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
					ecmnts << p.eat_comments()
				}
			}
			is_sum_type = true
		} else {
			if p.tok.kind == .rcbr {
				p.next()
				return ast.MatchExpr{
					pos: match_first_pos
				}
			}
			// Expression match
			for {
				p.inside_match_case = true
				mut range_pos := p.tok.pos()
				expr := p.expr(0)
				p.inside_match_case = false
				if p.tok.kind == .dotdot {
					p.error_with_pos('match only supports inclusive (`...`) ranges, not exclusive (`..`)',
						p.tok.pos())
					return ast.MatchExpr{}
				} else if p.tok.kind == .ellipsis {
					p.next()
					p.inside_match_case = true
					expr2 := p.expr(0)
					p.inside_match_case = false
					exprs << ast.RangeExpr{
						low:      expr
						high:     expr2
						has_low:  true
						has_high: true
						pos:      range_pos.extend(p.prev_tok.pos())
					}
				} else {
					exprs << expr
				}
				if p.tok.kind != .comma {
					ecmnts << p.eat_comments()
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
					ecmnts << p.eat_comments()
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
			exprs:         exprs
			ecmnts:        ecmnts
			stmts:         stmts
			pos:           pos
			branch_pos:    branch_pos
			is_else:       is_else
			post_comments: post_comments
			scope:         branch_scope
		}
		if p.tok.kind == .rcbr || (is_else && no_lcbr) {
			break
		}
	}
	match_last_pos := p.tok.pos()
	mut pos := token.Pos{
		line_nr: match_first_pos.line_nr
		pos:     match_first_pos.pos
		len:     match_last_pos.pos - match_first_pos.pos + match_last_pos.len
		col:     match_first_pos.col
	}
	if p.tok.kind == .rcbr {
		p.check(.rcbr)
	}
	// return ast.StructInit{}
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.MatchExpr{
		is_comptime: is_comptime
		branches:    branches
		cond:        cond
		is_sum_type: is_sum_type
		pos:         pos
		comments:    comments
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
			exprs := p.expr_list(true)
			if exprs.len != 1 {
				p.error('only one expression allowed as `select` key')
				return ast.SelectExpr{}
			}
			if p.tok.kind in [.assign, .decl_assign] {
				stmt = p.partial_assign_stmt(exprs)
			} else {
				stmt = ast.ExprStmt{
					expr:     exprs[0]
					pos:      exprs[0].pos()
					comments: [comment]
					is_expr:  true
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
		p.inside_for = false
		stmts := p.parse_block_no_scope(false)
		p.close_scope()
		p.inside_match_body = false
		mut pos := token.Pos{
			line_nr: branch_first_pos.line_nr
			pos:     branch_first_pos.pos
			len:     branch_last_pos.pos - branch_first_pos.pos + branch_last_pos.len
			col:     branch_first_pos.col
		}
		post_comments := p.eat_comments()
		pos.update_last_line(p.prev_tok.line_nr)
		if post_comments.len > 0 {
			pos.last_line = post_comments.last().pos.last_line
		}
		branches << ast.SelectBranch{
			stmt:          stmt
			stmts:         stmts
			pos:           pos
			comment:       comment
			is_else:       is_else
			is_timeout:    is_timeout
			post_comments: post_comments
		}
		if p.tok.kind == .rcbr || ((is_else || is_timeout) && no_lcbr) {
			break
		}
	}
	match_last_pos := p.tok.pos()
	pos := token.Pos{
		line_nr: match_first_pos.line_nr
		pos:     match_first_pos.pos
		len:     match_last_pos.pos - match_first_pos.pos + match_last_pos.len
		col:     match_first_pos.col
	}
	if p.tok.kind == .rcbr {
		p.check(.rcbr)
	}
	p.register_auto_import('sync')
	return ast.SelectExpr{
		branches:      branches
		pos:           pos.extend_with_last_line(p.prev_tok.pos(), p.prev_tok.line_nr)
		has_exception: has_else || has_timeout
	}
}

fn (mut p Parser) comptime_if_cond(mut cond ast.Expr) bool {
	mut is_true := false
	match mut cond {
		ast.BoolLiteral {
			return cond.val
		}
		ast.ParExpr {
			return p.comptime_if_cond(mut cond.expr)
		}
		ast.PrefixExpr {
			if cond.op != .not {
				p.error('invalid \$if prefix operator, only allow `!`.')
				return false
			}
			return !p.comptime_if_cond(mut cond.right)
		}
		ast.PostfixExpr {
			if cond.op != .question {
				p.error('invalid \$if postfix operator, only allow `?`.')
				return false
			}
			if cond.expr !is ast.Ident {
				p.error('invalid \$if postfix condition, only allow `Indent`.')
				return false
			}
			cname := (cond.expr as ast.Ident).name
			return cname in p.pref.compile_defines
		}
		ast.InfixExpr {
			match cond.op {
				.and, .logical_or {
					l := p.comptime_if_cond(mut cond.left)
					r := p.comptime_if_cond(mut cond.right)
					// if at least one of the cond has `keep_stmts`, we should keep stmts
					return if cond.op == .and { l && r } else { l || r }
				}
				.eq, .ne, .gt, .lt, .ge, .le {
					match mut cond.left {
						ast.Ident {
							// $if version == 2
							match mut cond.right {
								ast.StringLiteral {
									match cond.op {
										.eq {
											is_true = cond.left.str() == cond.right.str()
										}
										.ne {
											is_true = cond.left.str() != cond.right.str()
										}
										else {
											p.error('string type only support `==` and `!=` operator')
											return false
										}
									}
								}
								ast.IntegerLiteral {
									match cond.op {
										.eq {
											is_true = cond.left.str().i64() == cond.right.val.i64()
										}
										.ne {
											is_true = cond.left.str().i64() != cond.right.val.i64()
										}
										.gt {
											is_true = cond.left.str().i64() > cond.right.val.i64()
										}
										.lt {
											is_true = cond.left.str().i64() < cond.right.val.i64()
										}
										.ge {
											is_true = cond.left.str().i64() >= cond.right.val.i64()
										}
										.le {
											is_true = cond.left.str().i64() <= cond.right.val.i64()
										}
										else {
											p.error('int type only support `==` `!=` `>` `<` `>=` and `<=` operator')
											return false
										}
									}
								}
								ast.BoolLiteral {
									match cond.op {
										.eq {
											is_true = cond.left.str().bool() == cond.right.val
										}
										.ne {
											is_true = cond.left.str().bool() != cond.right.val
										}
										else {
											p.error('bool type only support `==` and `!=` operator')
											return false
										}
									}
								}
								else {
									p.error('compare only support string int and bool type')
									return false
								}
							}
							return is_true
						}
						else {
							p.error('invalid \$if condition')
							return false
						}
					}
					p.error('invalid \$if condition')
					return false
				}
				else {
					p.error('invalid \$if operator: ${cond.op}')
					return false
				}
			}
		}
		ast.Ident {
			cname := cond.name
			if cname in ast.valid_comptime_if_os {
				if cname_enum_val := pref.os_from_string(cname) {
					if cname_enum_val == p.pref.os {
						is_true = true
					}
				}
			} else if cname in ast.valid_comptime_if_compilers {
				is_true = pref.cc_from_string(cname) == p.pref.ccompiler_type
			} else if cname in ast.valid_comptime_if_platforms {
				if cname == 'aarch64' {
					p.note('use `arm64` instead of `aarch64`')
				}
				match cname {
					'amd64' {
						is_true = p.pref.arch == .amd64
					}
					'i386' {
						is_true = p.pref.arch == .i386
					}
					'aarch64' {
						is_true = p.pref.arch == .arm64
					}
					'arm64' {
						is_true = p.pref.arch == .arm64
					}
					'arm32' {
						is_true = p.pref.arch == .arm32
					}
					'rv64' {
						is_true = p.pref.arch == .rv64
					}
					'rv32' {
						is_true = p.pref.arch == .rv32
					}
					's390x' {
						is_true = p.pref.arch == .s390x
					}
					'ppc64le' {
						is_true = p.pref.arch == .ppc64le
					}
					'loongarch64' {
						is_true = p.pref.arch == .loongarch64
					}
					else {
						p.error('invalid \$if condition: unknown platforms `${cname}`')
						return false
					}
				}
			} else if cname in ast.valid_comptime_if_cpu_features {
				match cname {
					'x64' {
						is_true = p.pref.m64
					}
					'x32' {
						is_true = !p.pref.m64
					}
					'little_endian' {
						is_true = $if little_endian { true } $else { false }
					}
					'big_endian' {
						is_true = $if big_endian { true } $else { false }
					}
					else {
						p.error('invalid \$if condition: unknown cpu_features `${cname}`')
						return false
					}
				}
			} else if cname in ast.valid_comptime_if_other {
				match cname {
					'apk' {
						is_true = p.pref.is_apk
					}
					'js' {
						is_true = p.pref.backend.is_js()
					}
					'debug' {
						is_true = p.pref.is_debug
					}
					'prod' {
						is_true = p.pref.is_prod
					}
					'test' {
						is_true = p.pref.is_test
					}
					'glibc' {
						is_true = p.pref.is_glibc
					}
					'prealloc' {
						is_true = p.pref.prealloc
					}
					'no_bounds_checking' {
						is_true = p.pref.no_bounds_checking
					}
					'freestanding' {
						is_true = p.pref.is_bare && !p.pref.output_cross_c
					}
					'threads' {
						is_true = p.table.gostmts > 0
					}
					'js_node' {
						is_true = p.pref.backend == .js_node
					}
					'js_browser' {
						is_true = p.pref.backend == .js_browser
					}
					'js_freestanding' {
						is_true = p.pref.backend == .js_freestanding
					}
					'interpreter' {
						is_true = p.pref.backend == .interpret
					}
					'es5' {
						is_true = p.pref.output_es5
					}
					'profile' {
						is_true = p.pref.is_prof
					}
					'wasm32' {
						is_true = p.pref.arch == .wasm32
					}
					'wasm32_wasi' {
						is_true = p.pref.os == .wasm32_wasi
					}
					'fast_math' {
						is_true = p.pref.fast_math
					}
					'native' {
						is_true = p.pref.backend == .native
					}
					'autofree' {
						is_true = p.pref.autofree
					}
					else {
						p.error('invalid \$if condition: unknown other indent `${cname}`')
						return false
					}
				}
			} else {
				p.error('invalid \$if condition: unknown indent `${cname}`')
				return false
			}
			return is_true
		}
		ast.ComptimeCall {
			if cond.kind == .pkgconfig {
				if mut m := pkgconfig.main([cond.args_var]) {
					if _ := m.run() {
						is_true = true
					} else {
						// pkgconfig not found, do not issue error, just set false
						is_true = false
					}
				} else {
					p.error(err.msg())
					is_true = false
				}
				return is_true
			}
			if cond.kind == .d {
				is_true = cond.compile_value.bool()
				return is_true
			}
			p.error('invalid \$if condition: unknown ComptimeCall')
			return false
		}
		else {
			p.error('invalid \$if condition ${cond}')
			return false
		}
	}

	return is_true
}

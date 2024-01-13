// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast

fn (mut p Parser) for_stmt() ast.Stmt {
	p.check(.key_for)
	mut pos := p.tok.pos()
	p.open_scope()
	p.inside_for = true
	if p.tok.kind == .key_match {
		return p.error('cannot use `match` in `for` loop')
	}
	// defer { p.close_scope() }
	// Infinite loop
	mut comments := []ast.Comment{}
	comments << p.eat_comments()
	if p.tok.kind == .lcbr {
		p.inside_for = false
		stmts := p.parse_block_no_scope(false)
		pos.update_last_line(p.prev_tok.line_nr)
		for_stmt := ast.ForStmt{
			stmts: stmts
			pos: pos
			comments: comments
			is_inf: true
			scope: p.scope
		}
		p.close_scope()
		return for_stmt
	} else if p.peek_tok.kind in [.decl_assign, .assign, .semicolon]
		|| (p.peek_tok.kind in [.inc, .dec] && p.peek_token(2).kind in [.semicolon, .comma])
		|| p.peek_tok.kind.is_assign() || p.tok.kind == .semicolon
		|| (p.peek_tok.kind == .comma && p.peek_token(2).kind != .key_mut
		&& p.peek_token(3).kind != .key_in) {
		// `for i := 0; i < 10; i++ {` or `for a,b := 0,1; a < 10; a++ {`
		if p.tok.kind == .key_mut {
			return p.error('`mut` is not needed in `for ;;` loops: use `for i := 0; i < n; i ++ {`')
		}
		mut init := ast.empty_stmt
		mut cond := p.new_true_expr()
		mut inc := ast.empty_stmt
		mut has_init := false
		mut has_cond := false
		mut has_inc := false
		mut is_multi := p.peek_tok.kind == .comma && p.peek_token(2).kind != .key_mut
			&& p.peek_token(3).kind != .key_in
		if p.peek_tok.kind in [.assign, .decl_assign] || p.peek_tok.kind.is_assign() || is_multi {
			init = p.assign_stmt()
			has_init = true
		} else if p.peek_tok.kind in [.inc, .dec] {
			init = p.stmt(false)
			has_init = true
		}
		comments << p.eat_comments()
		// Allow `for ;; i++ {`
		// Allow `for i = 0; i < ...`
		p.check(.semicolon)
		if p.tok.kind != .semicolon {
			// Disallow `for i := 0; i++; i < ...`
			if p.tok.kind == .name && p.peek_tok.kind in [.inc, .dec] {
				return p.error('cannot use ${p.tok.lit}${p.peek_tok.kind} as value')
			}
			comments << p.eat_comments()
			cond = p.expr(0)
			has_cond = true
		}
		comments << p.eat_comments()
		p.check(.semicolon)
		if !is_multi {
			is_multi = p.peek_tok.kind == .comma
		}
		comments << p.eat_comments()
		if p.tok.kind != .lcbr {
			inc = p.stmt(false)
			has_inc = true
		}
		comments << p.eat_comments()
		p.inside_for = false
		stmts := p.parse_block_no_scope(false)
		pos.update_last_line(p.prev_tok.line_nr)
		for_c_stmt := ast.ForCStmt{
			stmts: stmts
			has_init: has_init
			has_cond: has_cond
			has_inc: has_inc
			is_multi: is_multi
			init: init
			cond: cond
			inc: inc
			pos: pos
			comments: comments
			scope: p.scope
		}
		p.close_scope()
		return for_c_stmt
	} else if p.peek_tok.kind in [.key_in, .comma]
		|| (p.tok.kind == .key_mut && p.peek_token(2).kind in [.key_in, .comma]) {
		// `for i in vals`, `for i in start .. end`, `for mut user in users`, `for i, mut user in users`
		mut val_is_mut := p.tok.kind == .key_mut
		mut_pos := p.tok.pos()
		if val_is_mut {
			p.next()
		}
		key_var_pos := p.tok.pos()
		mut val_var_pos := p.tok.pos()
		mut key_var_name := ''
		mut val_var_name := p.check_name()
		if p.tok.kind == .comma {
			if val_is_mut {
				p.error_with_pos('index of array or key of map cannot be mutated', mut_pos)
			}
			p.next()
			if p.tok.kind == .key_mut {
				// `for i, mut user in users {`
				p.next()
				val_is_mut = true
			}
			key_var_name = val_var_name
			val_var_pos = p.tok.pos()
			val_var_name = p.check_name()
			if key_var_name == val_var_name && key_var_name != '_' {
				return p.error_with_pos('key and value in a for loop cannot be the same',
					val_var_pos)
			}
			if p.scope.known_var(key_var_name) {
				return p.error_with_pos('redefinition of key iteration variable `${key_var_name}`',
					key_var_pos)
			}
			if p.scope.known_var(val_var_name) {
				return p.error_with_pos('redefinition of value iteration variable `${val_var_name}`',
					val_var_pos)
			}
			p.scope.register(ast.Var{
				name: key_var_name
				typ: ast.int_type
				pos: key_var_pos
				is_tmp: true
				is_stack_obj: true
			})
		} else if p.scope.known_var(val_var_name) {
			return p.error_with_pos('redefinition of value iteration variable `${val_var_name}`, use `for (${val_var_name} in array) {` if you want to check for a condition instead',
				val_var_pos)
		}
		comments << p.eat_comments()
		p.check(.key_in)
		if p.tok.kind == .name && p.tok.lit in [key_var_name, val_var_name] {
			return p.error('in a `for x in array` loop, the key or value iteration variable `${p.tok.lit}` can not be the same as the array variable')
		}
		comments << p.eat_comments()
		// arr_expr
		cond := p.expr(0)
		// 0 .. 10
		// start := p.tok.lit.int()
		// TODO use RangeExpr
		mut high_expr := ast.empty_expr
		mut is_range := false
		if p.tok.kind == .ellipsis {
			p.error_with_pos('for loop only supports exclusive (`..`) ranges, not inclusive (`...`)',
				p.tok.pos())
		} else if p.tok.kind == .dotdot {
			is_range = true
			p.next()
			high_expr = p.expr(0)
			p.scope.register(ast.Var{
				name: val_var_name
				typ: ast.int_type
				pos: val_var_pos
				is_tmp: true
				is_stack_obj: true
			})
			if key_var_name.len > 0 {
				return p.error_with_pos('cannot declare index variable with range `for`',
					key_var_pos)
			}
			if val_is_mut {
				return p.error_with_pos('variable in range `for` cannot be mut', mut_pos)
			}
		} else {
			// this type will be set in checker
			p.scope.register(ast.Var{
				name: val_var_name
				pos: val_var_pos
				is_mut: val_is_mut
				is_auto_deref: val_is_mut
				is_tmp: true
				is_stack_obj: true
			})
		}
		comments << p.eat_comments()
		p.inside_for = false
		stmts := p.parse_block_no_scope(false)
		pos.update_last_line(p.prev_tok.line_nr)
		// println('nr stmts=$stmts.len')
		for_in_stmt := ast.ForInStmt{
			stmts: stmts
			cond: cond
			key_var: key_var_name
			val_var: val_var_name
			high: high_expr
			is_range: is_range
			pos: pos
			kv_pos: key_var_pos
			comments: comments
			val_is_mut: val_is_mut
			scope: p.scope
		}
		p.close_scope()
		return for_in_stmt
	}
	// `for cond {`
	cond := p.expr(0)
	p.inside_for = false
	// extra scope for the body
	p.open_scope()
	stmts := p.parse_block_no_scope(false)
	pos.update_last_line(p.prev_tok.line_nr)
	for_stmt := ast.ForStmt{
		cond: cond
		stmts: stmts
		pos: pos
		scope: p.scope
	}
	p.close_scope()
	p.close_scope()
	return for_stmt
}

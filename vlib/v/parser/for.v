// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token

fn (var p Parser) for_stmt() ast.Stmt {
	p.check(.key_for)
	pos := p.tok.position()
	p.open_scope()
	p.inside_for = true
	// defer { p.close_scope() }
	// Infinite loop
	if p.tok.kind == .lcbr {
		p.inside_for = false
		stmts := p.parse_block()
		p.close_scope()
		return ast.ForStmt{
			stmts: stmts
			pos: pos
			is_inf: true
		}
	} else if p.tok.kind in [.key_mut, .key_var] {
		p.error('`var` is not needed in for loops')
	} else if p.peek_tok.kind in [.decl_assign, .assign, .semicolon] || p.tok.kind == .semicolon {
		// `for i := 0; i < 10; i++ {`
		var init := ast.Stmt{}
		var cond := p.new_true_expr()
		// mut inc := ast.Stmt{}
		var inc := ast.Expr{}
		var has_init := false
		var has_cond := false
		var has_inc := false
		if p.peek_tok.kind in [.assign, .decl_assign] {
			init = p.assign_stmt()
			has_init = true
		} else if p.tok.kind != .semicolon {
		}
		// allow `for ;; i++ {`
		// Allow `for i = 0; i < ...`
		p.check(.semicolon)
		if p.tok.kind != .semicolon {
			var typ := table.void_type
			cond = p.expr(0)
			has_cond = true
		}
		p.check(.semicolon)
		if p.tok.kind != .lcbr {
			// inc = p.stmt()
			inc = p.expr(0)
			has_inc = true
		}
		p.inside_for = false
		stmts := p.parse_block()
		p.close_scope()
		return ast.ForCStmt{
			stmts: stmts
			has_init: has_init
			has_cond: has_cond
			has_inc: has_inc
			init: init
			cond: cond
			inc: inc
			pos: pos
		}
	} else if p.peek_tok.kind in [.key_in, .comma] {
		// `for i in vals`, `for i in start .. end`
		var key_var_name := ''
		var val_var_name := p.check_name()
		if p.tok.kind == .comma {
			p.check(.comma)
			key_var_name = val_var_name
			val_var_name = p.check_name()
			if p.scope.known_var(key_var_name) {
				p.error('redefinition of `$key_var_name`')
			}
			if p.scope.known_var(val_var_name) {
				p.error('redefinition of `$val_var_name`')
			}
			p.scope.register(key_var_name, ast.Var{
				name: key_var_name
				typ: table.int_type
			})
		} else if p.scope.known_var(val_var_name) {
			p.error('redefinition of `$val_var_name`')
		}
		p.check(.key_in)
		if p.tok.kind == .name && p.tok.lit in [key_var_name, val_var_name] {
			p.error('redefinition of `$p.tok.lit`')
		}
		// arr_expr
		cond := p.expr(0)
		// 0 .. 10
		// start := p.tok.lit.int()
		// TODO use RangeExpr
		var high_expr := ast.Expr{}
		var is_range := false
		if p.tok.kind == .dotdot {
			is_range = true
			p.check(.dotdot)
			high_expr = p.expr(0)
			p.scope.register(val_var_name, ast.Var{
				name: val_var_name
				typ: table.int_type
			})
		} else {
			// this type will be set in checker
			p.scope.register(val_var_name, ast.Var{
				name: val_var_name
			})
		}
		p.inside_for = false
		stmts := p.parse_block()
		// println('nr stmts=$stmts.len')
		p.close_scope()
		return ast.ForInStmt{
			stmts: stmts
			cond: cond
			key_var: key_var_name
			val_var: val_var_name
			high: high_expr
			is_range: is_range
			pos: pos
		}
	}
	// `for cond {`
	cond := p.expr(0)
	p.inside_for = false
	stmts := p.parse_block()
	p.close_scope()
	return ast.ForStmt{
		cond: cond
		stmts: stmts
		pos: pos
	}
}

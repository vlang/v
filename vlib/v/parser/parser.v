// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import (
	v.scanner
	v.ast
	v.token
	v.table
	v.types
	term
)

struct Parser {
	scanner     &scanner.Scanner
mut:
	tok         token.Token
	peek_tok    token.Token
	// vars []string
	table       &table.Table
	return_type types.Type
}

pub fn parse_stmt(text string, table &table.Table) ast.Stmt {
	s := scanner.new_scanner(text)
	mut p := Parser{
		scanner: s
		table: table
	}
	p.read_first_token()
	return p.stmt()
}

pub fn (p mut Parser) get_type() types.Type {
	defer {
		p.next()
	}
	match p.tok.lit {
		'int' {
			return types.int_type
		}
		'f64' {
			return types.f64_type
		}
		'string' {
			return types.string_type
		}
		else {
			p.error('bad type lit')
			exit(0)
		}
	}
}

pub fn parse_file(text string, table &table.Table) ast.Program {
	mut stmts := []ast.Stmt
	mut p := Parser{
		scanner: scanner.new_scanner(text)
		table: table
	}
	p.read_first_token()
	for {
		// res := s.scan()
		if p.tok.kind == .eof {
			break
		}
		// println('expr at ' + p.tok.str())
		s := p.stmt()
		// println(s)
		stmts << s // p.stmt()
	}
	// println('nr stmts = $stmts.len')
	// println(stmts[0])
	return ast.Program{
		stmts: stmts
	}
}

pub fn (p mut Parser) read_first_token() {
	// need to call next() twice to get peek token and current token
	p.next()
	p.next()
}

pub fn (p mut Parser) parse_block() []ast.Stmt {
	mut stmts := []ast.Stmt
	for {
		// res := s.scan()
		if p.tok.kind in [.eof, .rcbr] {
			break
		}
		// println('expr at ' + p.tok.str())
		stmts << p.stmt()
	}
	p.next()
	// println('nr exprs in block = $exprs.len')
	return stmts
}

/*
pub fn parse_stmt(text string) ast.Stmt {
	mut s := scanner.new_scanner(text)
	res := s.scan()
	mut p := Parser{
		scanner: s
		tok: res.tok
		lit: res.lit
	}
	return p.stmt()
}
*/


fn (p mut Parser) next() {
	p.tok = p.peek_tok
	p.peek_tok = p.scanner.scan()
	// println(p.tok.str())
}

fn (p mut Parser) check(expected token.TokenKind) {
	if p.tok.kind != expected {
		s := 'syntax error: unexpected `${p.tok.kind.str()}`, expecting `${expected.str()}`'
		p.error(s)
	}
	p.next()
}

fn (p mut Parser) check_name() string {
	name := p.tok.lit
	p.check(.name)
	return name
}

pub fn (p mut Parser) stmt() ast.Stmt {
	// println('stmt at ' + p.tok.str())
	// `x := ...`
	if p.tok.kind == .name {
		if p.peek_tok.kind == .decl_assign {
			return p.var_decl()
		}
		else if p.peek_tok.is_assign() {
			return p.assign_stmt()
		}
	}
	match p.tok.kind {
		.key_module {
			return p.module_decl()
		}
		.key_import {
			return p.import_stmt()
		}
		.key_fn {
			return p.fn_decl()
		}
		.key_return {
			return p.return_stmt()
		}
		.key_mut {
			return p.var_decl()
		}
		else {
			expr,_ := p.expr(0)
			return ast.ExprStmt{
				expr: expr
			}
		}
	}
}

pub fn (p mut Parser) assign_stmt() ast.AssignStmt {
	name := p.tok.lit
	// println('looking for $name')
	var := p.table.find_var(name) or {
		p.error('unknown variable `$name`')
		exit(1)
	}
	if !var.is_mut {
		p.error('`$var.name` is immutable, declare it with `mut $var.name := ...`')
	}
	left_expr,left_type := p.expr(0)
	op := p.tok.kind
	// println('assignn_stmt() ' + op.str())
	p.next()
	right_expr,right_type := p.expr(0)
	if !types.check(left_type, right_type) {
		p.error('oops')
	}
	return ast.AssignStmt{
		left: left_expr
		right: right_expr
		op: op
	}
}

pub fn (p &Parser) error(s string) {
	println(term.bold(term.red('x.v:$p.tok.line_nr: $s')))
	exit(1)
}

pub fn (p mut Parser) call_expr() (ast.CallExpr,types.Type) {
	// println('got fn call')
	fn_name := p.tok.lit
	f := p.table.find_fn(fn_name) or {
		p.error('unknown function `$p.tok.lit`')
		exit(0)
	}
	p.check(.name)
	p.check(.lpar)
	mut args := []ast.Expr
	for i, arg in f.args {
		e,typ := p.expr(0)
		if !types.check(arg.typ, typ) {
			p.error('cannot used type `$typ.name` as type `$arg.typ.name` in argument to `$fn_name`')
		}
		args << e
		if i < f.args.len - 1 {
			p.check(.comma)
		}
	}
	if p.tok.kind == .comma {
		p.error('too many arguments in call to `$fn_name`')
	}
	p.check(.rpar)
	node := ast.CallExpr{
		name: fn_name
		args: args
	}
	return node,types.int_type
}

// Implementation of Pratt Precedence
pub fn (p mut Parser) expr(rbp int) (ast.Expr,types.Type) {
	// println('expr at ' + p.tok.str())
	// null denotation (prefix)
	mut node := ast.Expr{}
	mut typ := types.void_type
	
	ptok := p.tok
	match p.tok.kind {
		.name {
			/*
			sym := p.table.find_symbol(p.tok.lit)
			if sym.cat == .function {
				return
			}
			*/

			// fn call
			if p.peek_tok.kind == .lpar {
				x,typ2 := p.call_expr() // TODO `node,typ :=` should work
				node = x
				typ = typ2
			}
			else {
				// name expr
				node = ast.Ident{
					name: p.tok.lit
				}
				typ = types.int_type
				p.next()
			}
		}
		.key_true {
			node = ast.BoolLiteral{
				val: true
			}
			typ = types.bool_type
			p.next()
		}
		.str {
			node,typ = p.parse_string_literal()
		}
		.number {
			node,typ = p.parse_number_literal()
		}
		.key_if {
			node,typ = p.if_expr()
		}
		.lpar {
			node,typ = p.expr(0)
			p.check(.rpar)
		}
		else {
			if p.tok.is_unary() {
				expr,_ := p.expr(token.highest_prec)
				node = ast.UnaryExpr{
					left: expr
				}
			}
			else {
				verror('!unknown token ' + p.tok.str())
			}	
		}
	}
	// left binding power
	for rbp < p.tok.precedence() {
		prev_tok := p.tok
		p.next()
		mut t2 := types.Type{}
		// left denotation (infix / postfix)
		if prev_tok.is_right_assoc() {
			mut expr := ast.Expr{}
			expr,t2 = p.expr(prev_tok.precedence() - 1)
			node = ast.BinaryExpr{
				left: node
				op: prev_tok.kind
				right: expr
			}
			if !types.check(&typ, &t2) {
				println('tok: $prev_tok.str()')
				p.error('cannot convert `$t2.name` to `$typ.name`')
			}
		}
		else if prev_tok.is_left_assoc() {
			// postfix (`++` | `--`)
			if prev_tok.kind in [.inc, .dec] {
				node = ast.UnaryExpr{
					left: node
					op: prev_tok.kind
				}
			} else {
				mut expr := ast.Expr{}
				expr,t2 = p.expr(prev_tok.precedence())
				node = ast.BinaryExpr{
					left: node
					op: prev_tok.kind
					right: expr
				}	
			}
		}
	}

	// println('TOK: $p.tok')
	return node,typ
}

fn (p mut Parser) if_expr() (ast.Expr,types.Type) {
	mut node := ast.Expr{}
	p.check(.key_if)
	cond,typ := p.expr(0)
	if !types.check(types.bool_type, typ) {
		p.error('non-bool used as if condition')
	}
	p.check(.lcbr)
	stmts := p.parse_block()
	node = ast.IfExpr{
		cond: cond
		stmts: stmts
	}
	return node,types.void_type
}

fn (p mut Parser) parse_string_literal() (ast.Expr,types.Type) {
	mut node := ast.Expr{}
	node = ast.StringLiteral{
		val: p.tok.lit
	}
	p.next()
	return node,types.string_type
}

fn (p mut Parser) parse_number_literal() (ast.Expr,types.Type) {
	lit := p.tok.lit
	mut node := ast.Expr{}
	mut typ := types.int_type
	if lit.contains('.') {
		node = ast.FloatLiteral{
			// val: lit.f64()
			val: lit
		}
		typ = types.f64_type
	}
	else {
		node = ast.IntegerLiteral{
			val: lit.int()
		}
		typ = types.int_type
	}
	p.next()
	return node,typ
}

fn (p mut Parser) module_decl() ast.Stmt {
	// p.check(.key_module)
	p.next()
	return ast.Module{}
}

fn (p mut Parser) import_stmt() ast.Import {
	// p.check(.key_import)
	p.next()
	return ast.Import{}
}

fn (p mut Parser) fn_decl() ast.FnDecl {
	p.table.clear_vars()
	p.check(.key_fn)
	name := p.tok.lit
	// println('fn decl $name')
	p.check(.name)
	p.check(.lpar)
	// Args
	mut args := []table.Var
	mut ast_args := []ast.Arg
	for p.tok.kind != .rpar {
		arg_name := p.check_name()
		typ := p.get_type()
		args << table.Var{
			name: arg_name
			typ: typ
		}
		ast_args << ast.Arg{
			typ: typ
			name: arg_name
		}
	}
	p.check(.rpar)
	// Return type
	mut typ := types.void_type
	if p.tok.kind == .name {
		typ = p.get_type()
		p.return_type = typ
	}
	p.check(.lcbr)
	p.table.register_fn(table.Fn{
		name: name
		args: args
	})
	// p.check(.rcbr)
	stmts := p.parse_block()
	return ast.FnDecl{
		name: name
		stmts: stmts
		typ: typ
		args: ast_args
	}
}

fn (p mut Parser) return_stmt() ast.Return {
	// println('return st')
	p.next()
	expr,t := p.expr(0)
	if !types.check(p.return_type, t) {
		p.error('bad ret type')
	}
	return ast.Return{
		expr: expr
	}
}

fn (p mut Parser) var_decl() ast.VarDecl {
	is_mut := p.tok.kind == .key_mut // || p.prev_tok == .key_for
	// is_static := p.tok.kind == .key_static
	if p.tok.kind == .key_mut {
		p.check(.key_mut)
		// p.fspace()
	}
	if p.tok.kind == .key_static {
		p.check(.key_static)
		// p.fspace()
	}
	name := p.tok.lit
	p.read_first_token()
	expr,t := p.expr(token.lowest_prec)
	if _ := p.table.find_var(name) {
		p.error('redefinition of `$name`')
	}
	p.table.register_var(table.Var{
		name: name
		is_mut: is_mut
	})
	// println(p.table.names)
	// println('added $name')
	return ast.VarDecl{
		name: name
		expr: expr // p.expr(token.lowest_prec)
		
		typ: t
	}
}

fn verror(s string) {
	println(s)
	exit(1)
}

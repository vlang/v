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
	p.next()
	p.next()
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
			verror('bad type lit')
			exit(1)
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
		println(s)
		stmts << s // p.stmt()
	}
	println('nr stmts = $stmts.len')
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
		verror(s)
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
	//println('looking for $name')
	var := p.table.find_var(name) or {
		p.error('unknown variable `$name`')
		exit(1)
	}
	if !var.is_mut {
		p.error('`$var.name` is immutable, declare it with `mut $var.name := ...`')
	}
	left_expr,left_type := p.expr(0)
	op := p.tok.kind
	//println('assignn_stmt() ' + op.str())
	p.next()
	right_expr,right_type := p.expr(0)
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

// Implementation of Pratt Precedence
pub fn (p mut Parser) expr(rbp int) (ast.Expr,types.Type) {
	// println('expr at ' + p.tok.str())
	// null denotation (prefix)
	mut node := ast.Expr{}
	mut typ := types.void_type
	match p.tok.kind {
		.name {
			// name expr
			node = ast.Ident{
				name: p.tok.lit
			}
			typ = types.int_type
			p.next()
		}
		.str {
			node,typ = p.parse_string_literal()
		}
		.number {
			node,typ = p.parse_number_literal()
		}
		.lpar {
			node,typ = p.expr(0)
			p.check(.rpar)
		}
		else {
			p.next()
			if p.tok.is_unary() {
				expr,_ := p.expr(token.highest_prec)
				node = ast.UnaryExpr{
					// left: p.expr(token.highest_prec)
					left: expr
					op: p.tok.kind
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
		// left denotation (infix)
		if prev_tok.is_right_assoc() {
			mut expr := ast.Expr{}
			expr,t2 = p.expr(prev_tok.precedence() - 1)
			node = ast.BinaryExpr{
				left: node
				op: prev_tok.kind
				right: expr
			}
			if !types.check(&typ, &t2) {
				verror('cannot convert `$t2.name` to `$typ.name`')
			}
		}
		else if prev_tok.is_left_assoc() {
			mut expr := ast.Expr{}
			expr,t2 = p.expr(prev_tok.precedence())
			node = ast.BinaryExpr{
				left: node
				op: prev_tok.kind
				right: expr
			}
		}
	}
	return node,typ
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
	p.check(.rpar)
	// Return type
	mut typ := types.void_type
	if p.tok.kind == .name {
		typ = p.get_type()
		p.return_type = typ
	}
	p.check(.lcbr)
	// p.check(.rcbr)
	stmts := p.parse_block()
	return ast.FnDecl{
		name: name
		stmts: stmts
		typ: typ
	}
}

fn (p mut Parser) return_stmt() ast.Return {
	// println('return st')
	p.next()
	expr,t := p.expr(0)
	if !types.check(p.return_type, t) {
		verror('bad ret type')
	}
	return ast.Return{
		expr: expr
	}
}

fn (p mut Parser) var_decl() ast.VarDecl {
	is_mut := p.tok.kind == .key_mut // || p.prev_tok == .key_for
	is_static := p.tok.kind == .key_static
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
		verror('redefinition of `$name`')
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

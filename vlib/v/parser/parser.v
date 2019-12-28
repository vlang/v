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
)

struct Parser {
	scanner &scanner.Scanner
mut:
	tok      token.Token
	peek_tok token.Token
	//vars []string
	table &table.Table
	return_type types.Type
}

pub fn parse_expr(text string, table &table.Table) ast.Expr {
	s := scanner.new_scanner(text)
	mut p := Parser{
		scanner: s
		table: table
	}
	p.next()
	p.next()
	expr,_ := p.expr(token.lowest_prec)
	return expr
}

pub fn (p mut Parser) get_type() types.Type {
	defer {
	p.next()
	}
	match p.tok.lit {
		'int'    { return types.int_type }
		'f64'    { return types.f64_type }
		'string' { return types.string_type }
		else {
			verror('bad type lit')
			exit(1)
		}
	}
}

pub fn parse_file(text string, table &table.Table) ast.Program {
	s := scanner.new_scanner(text)
	mut exprs := []ast.Expr
	mut p := Parser{
		scanner: s
		table: table
	}
	p.next()
	p.next()
	for {
		//res := s.scan()
		if p.tok.kind == .eof {
			break
		}
		//println('expr at ' + p.tok.str())
		expr,_ := p.expr(token.lowest_prec)
		exprs << expr
	}
	println('nr exprs = $exprs.len')
	println(exprs[0])
	return ast.Program{exprs}
}

pub fn (p mut Parser)  parse_block() []ast.Expr {
	mut exprs := []ast.Expr

	for {
		//res := s.scan()
		if p.tok.kind in [.eof, .rcbr] {
			break
		}
		//println('expr at ' + p.tok.str())
		expr,_ := p.expr(token.lowest_prec)
		exprs << expr
	}
	p.next()
	println('nr exprs in block = $exprs.len')
	return exprs

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

// Implementation of Pratt Precedence
pub fn (p mut Parser) expr(rbp int) (ast.Expr,types.Type) {
	// null denotation (prefix)
	mut node := ast.Expr{}
	mut typ := types.void_type
	match p.tok.kind {
		.key_module { return p.module_decl() }
		.key_import { return p.import_stmt() }
		.key_fn     { return p.fn_decl() }
		.key_return { return p.return_stmt() }
		.name {
			if p.peek_tok.kind == .decl_assign { 
				return p.var_decl()
			}
		}
		.str    { node, typ = p.parse_string_literal() }
		.number { node, typ = p.parse_number_literal() }
		.lpar {
			node,typ = p.expr(0)
			if p.tok.kind != .rpar {
				panic('Parse Error: expected )')
			}
		}
		else {
			p.next()
			if p.tok.kind.is_unary() {
				expr,_ := p.expr(token.highest_prec)
				node = ast.UnaryExpr{
					// left: p.expr(token.highest_prec)
					left: expr
					op: p.tok.kind
				}
			}
		}
	}

	// left binding power
	for rbp < p.tok.kind.precedence() {
		prev_tok := p.tok
		p.next()
		mut t2 := types.Type{}
		// left denotation (infix)
		if prev_tok.kind.is_right_assoc() {
			mut expr := ast.Expr{}
			expr,t2 = p.expr(prev_tok.kind.precedence() - 1)
			node = ast.BinaryExpr{
				left: node
				//left_type: t1
				op: prev_tok.kind
				// right: p.expr(prev_tok.precedence() - 1)
				right: expr
			}
			if !types.check(&typ, &t2) {
				verror('cannot convert `$t2.name` to `$typ.name`')
			}
		}
		else if prev_tok.kind.is_left_assoc() {
			mut expr := ast.Expr{}
			expr,t2 = p.expr(prev_tok.kind.precedence())
			node = ast.BinaryExpr{
				left: node
				op: prev_tok.kind
				right: expr
			}
		}
	}
	return node, typ
}

/*
fn (p mut Parser) stmt() ast.Stmt {
	if p.tok == .name {
		name := p.lit
		p.next()
		if p.tok == .decl_assign {
			p.next()
			return ast.VarDecl{
				name: name
				expr: p.expr(token.lowest_prec)
			}
		}
	}
	/*
	match node {
		Ident {


		}

	}
	*/

	return ast.VarDecl{}
}
*/

fn (p mut Parser) parse_string_literal() (ast.Expr,types.Type) {
	mut node := ast.Expr{}
	node = ast.StringLiteral{
		val: p.tok.lit
	}
	p.next()
	return node, types.string_type
}

fn (p mut Parser) parse_number_literal() (ast.Expr,types.Type) {
	lit := p.tok.lit
	mut node := ast.Expr{}
	mut typ := types.int_type
	if lit.contains('.') {
		node = ast.FloatLiteral{
			//val: lit.f64()
			val: lit
		}
		typ = types.int_type
	} else {
		node = ast.IntegerLiteral{
			val: lit.int()
		}
		typ = types.int_type
	}
	p.next()
	return node, typ
}

fn (p mut Parser) module_decl() (ast.Expr,types.Type) {
	p.next()
	return ast.Expr{}, types.void_type
}	

fn (p mut Parser) import_stmt() (ast.Expr,types.Type) {
	p.next()
	return ast.Expr{}, types.void_type
}

fn (p mut Parser) fn_decl() (ast.Expr,types.Type) {
	p.check(.key_fn)
	name := p.tok.lit
	println('fn decl $name')
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
	//p.check(.rcbr)
	println('OK!')
	exprs := p.parse_block()

	mut node := ast.Expr{}
	node = ast.FnDecl{name: name, exprs: exprs, typ: typ}
	return node, types.void_type
}

fn (p mut Parser) return_stmt() (ast.Expr,types.Type) {
	println('return st')
	p.next()
	expr, t := p.expr(0)
	if !types.check(p.return_type, t) {
		verror('bad ret type')
	}
	mut node := ast.Expr{}
	node = ast.Return{expr: expr}
	return node, types.void_type
}

fn (p mut Parser) var_decl() (ast.Expr,types.Type) {
	name := p.tok.lit
	p.next()
	p.next()
	expr,t :=p.expr(token.lowest_prec)
	if name in p.table.names {
		verror('redefinition of `$name`')
	}
	p.table.names << name
	println(p.table.names)
	println('added $name')
	mut node := ast.Expr{}
	// TODO can't return VarDecl{}
	node = ast.VarDecl{
		name: name
		expr: expr//p.expr(token.lowest_prec)
		typ: t
	}//, ast.void_type
	return node, types.void_type
}

fn verror(s string) {
	println(s)
	exit(1)
}

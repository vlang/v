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
	tok     token.Token
	lit     string
	//vars []string
	table &table.Table
	return_type types.Type
}

pub fn parse_expr(text string, table &table.Table) ast.Expr {
	mut s := scanner.new_scanner(text)
	res := s.scan()
	mut p := Parser{
		scanner: s
		tok: res.tok
		lit: res.lit
		table: table
	}
	expr,_ := p.expr(token.lowest_prec)
	return expr
}

pub fn (p mut Parser) get_type() types.Type {
	defer {
	p.next()
	}
	if p.lit == 'int' { return types.int_type }
	else if p.lit == 'string' { return types.string_type }
	else if p.lit == 'f64' { return types.f64_type }
	else {
		verror('bad type lit')
		exit(1)
	}

}

pub fn parse_file(text string, table &table.Table) ast.Program {
	s := scanner.new_scanner(text)
	mut exprs := []ast.Expr
	mut p := Parser{
		scanner: s
		//tok: res.tok
		//lit: res.lit
		table: table
	}
	p.next()
	for {
		//res := s.scan()
		if p.tok == .eof {
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
		if p.tok == .eof || p.tok == .rcbr {
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
	res := p.scanner.scan()
	p.tok = res.tok
	// println(p.tok.str())
	p.lit = res.lit
}

fn (p mut Parser) check(expected token.Token) {
	if p.tok != expected {
		s := 'syntax error: unexpected `${p.tok.str()}`, expecting `${expected.str()}`'
		verror(s)
	}
	p.next()
}

fn (p mut Parser) check_name() string {
	name := p.lit
	p.check(.name)
	return name
}

// Implementation of Pratt Precedence
pub fn (p mut Parser) expr(rbp int) (ast.Expr,types.Type) {
	// null denotation (prefix)
	tok := p.tok
	lit := p.lit
	if p.tok == .key_fn {
		p.next()
		name := p.lit
		println('fn decl $name')
		p.check(.name)
		p.check(.lpar)
		p.check(.rpar)
		// Return type
		mut typ := types.void_type
		if p.tok == .name {
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
	else if p.tok == .key_return {
		p.next()
		mut node := ast.Expr{}
		expr, typ := p.expr(0)
		if !types.check(p.return_type, typ) {
			verror('bad ret type')
		}
		node = ast.Return{expr: expr}
		return node, types.void_type
	}
	else if p.tok == .name {
		name := p.lit
		p.next()
		if p.tok == .decl_assign {
			p.next()
			mut node := ast.Expr{}
			expr,t :=p.expr(token.lowest_prec)
			if name in p.table.names {
				verror('redefinition of `$name`')
			}
			p.table.names << name
			println(p.table.names)
			println('added $name')
			// TODO can't return VarDecl{}
			node = ast.VarDecl{
				name: name
				expr: expr//p.expr(token.lowest_prec)
				typ: t
			}//, ast.void_type
			return node, types.void_type
		}
	} else {
		p.next()
	}
	mut node := ast.Expr{}
	mut typ := types.void_type
	match tok {
		.lpar {
			node,typ = p.expr(0)
			if p.tok != .rpar {
				panic('Parse Error: expected )')
			}
			p.next()
		}
		.str {
			node = ast.StringLiteral{
				val: lit
			}
			typ = types.string_type

		}
		.number {
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
		}
		else {
			// TODO: fix bug. note odd conditon instead of else if (same below)
			if tok.is_unary() {
				expr,_ := p.expr(token.highest_prec)
				node = ast.UnaryExpr{
					// left: p.expr(token.highest_prec)
					left: expr
					op: tok
				}
			}
		}
	}
	// left binding power
	for rbp < p.tok.precedence() {
		tok2 := p.tok
		p.next()
		mut t2 := types.Type{}
		// left denotation (infix)
		if tok2.is_right_assoc() {
			mut expr := ast.Expr{}
			expr,t2 = p.expr(tok2.precedence() - 1)
			node = ast.BinaryExpr{
				left: node
				//left_type: t1
				op: tok2
				// right: p.expr(tok2.precedence() - 1)
				right: expr
			}
			if !types.check(&typ, &t2) {
				verror('cannot convert `$t2.name` to `$typ.name`')
			}
		}
		if !tok2.is_right_assoc() && tok2.is_left_assoc() {
			mut expr := ast.Expr{}
			expr,t2 = p.expr(tok2.precedence())
			node = ast.BinaryExpr{
				left: node
				op: tok2
				right: expr
			}
		}
	}
	return node,typ
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


fn verror(s string) {
	println(s)
	exit(1)
}

// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import (
	compiler2.scanner
	compiler2.ast
	compiler2.token
)

struct Parser {
	scanner &scanner.Scanner
mut:
	tok     token.Token
	lit     string
}

pub fn parse_expr(text string) ast.Expr {
	mut s := scanner.new_scanner(text)
	res := s.scan()
	mut p := Parser{
		scanner: s
		tok: res.tok
		lit: res.lit
	}
	// return p.expr()
	return p.expr(token.lowest_prec)
}

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

fn (p mut Parser) next() {
	res := p.scanner.scan()
	p.tok = res.tok
	// println(p.tok.str())
	p.lit = res.lit
}

// Implementation of Pratt Precedence
pub fn (p mut Parser) expr(rbp int) ast.Expr {
	// null denotation (prefix)
	tok := p.tok
	lit := p.lit
	p.next()
	mut node := ast.Expr{}
	match tok {
		.lpar {
			node = p.expr(0)
			if p.tok != .rpar {
				panic('Parse Error: expected )')
			}
			p.next()
		}
		else {
			// TODO: fix bug. note odd conditon instead of else if (same below)
			if tok.is_scalar() {
				if tok == .str {
					node = ast.StringLiteral {
						val: lit
					}
				}	if tok == .number {
					node = ast.IntegerLiteral {
						val: lit.int()
					}
				}
				//else {
					//verror('bad scalar token')
				//}
			}
			if !tok.is_scalar() && tok.is_unary() {
				node = ast.UnaryExpr{
					left: p.expr(token.highest_prec)
					op: tok
				}
			}
		}}
	// left binding power
	for rbp < p.tok.precedence() {
		tok2 := p.tok
		p.next()
		// left denotation (infix)
		if tok2.is_right_assoc() {
			node = ast.BinaryExpr{
				left: node
				op: tok2
				right: p.expr(tok2.precedence() - 1)
			}
		}
		if !tok2.is_right_assoc() && tok2.is_left_assoc() {
			node = ast.BinaryExpr{
				left: node
				op: tok2
				right: p.expr(tok2.precedence())
			}
		}
	}
	return node
}

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

fn verror(s string) {
	println(s)
	exit(1)
}

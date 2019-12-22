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
	tok token.Token
	lit string
}

pub fn parse_expr(text string) ast.Expr {
	mut s := scanner.new_scanner(text)
	res := s.scan()
	mut p := Parser{
		scanner: s
		tok: res.tok
		lit: res.lit
	}
	return p.expr()
}

fn (p mut Parser) next() {
	res := p.scanner.scan()
	p.tok = res.tok
	//println(p.tok.str())
	p.lit = res.lit
}

fn (p mut Parser) expr() ast.Expr {
	//println('\n\nexpr()')
	mut node := p.term()
	for p.tok == .plus || p.tok == .minus {
		op := p.tok
		p.next()
		node = ast.BinaryExpr {
			left: node
			op: op
			right: p.term()
		}
	}
	return node
}

fn (p mut Parser) term() ast.Expr {
	mut node := p.factor()
	for p.tok == .mul || p.tok == .div || p.tok == .mod {
		op := p.tok
		p.next()
		node = ast.BinaryExpr {
			left: node
			op: op
			right: p.factor()
		}
	}
	return node
	//return ast.BinaryExpr{}
	//return ast.Expr.Binary(ast.BinaryExpr{})
}

fn (p mut Parser) factor() ast.Expr {
	if p.tok == .number {
		val := p.lit.int()
		p.next()
		return ast.IntegerExpr { val: val }
	} else {
		println('bad factor token')
		println(p.tok)
		exit(1)
	}
}




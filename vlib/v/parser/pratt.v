// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token

pub fn (mut p Parser) expr(precedence int) ast.Expr {
	// println('\n\nparser.expr()')
	mut typ := table.void_type
	mut node := ast.Expr{}
	is_stmt_ident := p.is_stmt_ident
	p.is_stmt_ident = false
	// Prefix
	match p.tok.kind {
		.key_mut, .key_static {
			node = p.parse_assign_ident()
		}
		.name {
			node = p.name_expr()
			p.is_stmt_ident = is_stmt_ident
		}
		.string {
			node = p.string_expr()
		}
		.dot {
			// .enum_val
			node = p.enum_val()
		}
		.chartoken {
			node = ast.CharLiteral{
				val: p.tok.lit
				pos: p.tok.position()
			}
			p.next()
		}
		.minus, .amp, .mul, .not, .bit_not {
			// -1, -a, !x, &x, ~x
			node = p.prefix_expr()
		}
		.key_true, .key_false {
			node = ast.BoolLiteral{
				val: p.tok.kind == .key_true
				pos: p.tok.position()
			}
			p.next()
		}
		.key_match {
			node = p.match_expr()
		}
		.number {
			node = p.parse_number_literal()
		}
		.lpar {
			p.check(.lpar)
			node = p.expr(0)
			p.check(.rpar)
			node = ast.ParExpr{
				expr: node
			}
		}
		.key_if {
			node = p.if_expr()
		}
		.lsbr {
			if p.expecting_type {
				// parse json.decode type (`json.decode([]User, s)`)
				node = p.name_expr()
			} else {
				node = p.array_init()
			}
		}
		.key_none {
			p.next()
			node = ast.None{}
		}
		.key_sizeof {
			p.next() // sizeof
			p.check(.lpar)
			sizeof_type := p.parse_type()
			if p.tok.lit == 'C' {
				p.next()
				p.check(.dot)
				node = ast.SizeOf{
					type_name: p.check_name()
					typ: sizeof_type
				}
			} else {
				node = ast.SizeOf{
					typ: sizeof_type
				}
			}
			p.check(.rpar)
		}
		.key_typeof {
			p.next()
			p.check(.lpar)
			expr := p.expr(0)
			p.check(.rpar)
			node = ast.TypeOf{
				expr: expr
			}
		}
		.lcbr {
			// Map `{"age": 20}` or `{ x | foo:bar, a:10 }`
			p.next()
			if p.tok.kind == .string {
				node = p.map_init()
			} else {
				// it should be a struct
				if p.peek_tok.kind == .pipe {
					node = p.assoc()
				} else if p.peek_tok.kind == .colon || p.tok.kind == .rcbr {
					node = p.struct_init(true) // short_syntax: true
				} else if p.tok.kind == .name {
					p.next()
					lit := if p.tok.lit != '' { p.tok.lit } else { p.tok.kind.str() }
					p.error('unexpected `$lit`, expecting `:`')
				} else {
					p.error('unexpected `$p.tok.lit`, expecting struct key')
				}
			}
			p.check(.rcbr)
		}
		.key_fn {
			// Anonymous function
			node = p.anon_fn()
			return node
		}
		else {
			if p.tok.kind == .comment {
				println(p.tok.lit)
			}
			p.error('expr(): bad token `$p.tok.kind.str()`')
		}
	}
	// Infix
	for precedence < p.tok.precedence() {
		if p.tok.kind.is_assign() && !p.is_stmt_ident {
			node = p.assign_expr(node)
		} else if p.tok.kind == .dot {
			node = p.dot_expr(node)
			p.is_stmt_ident = is_stmt_ident
		} else if p.tok.kind == .lsbr {
			node = p.index_expr(node)
		} else if p.tok.kind == .key_as {
			pos := p.tok.position()
			p.next()
			typ = p.parse_type()
			node = ast.AsCast{
				expr: node
				typ: typ
				pos: pos
			}
		} else if p.tok.kind == .left_shift && p.is_stmt_ident {
			// arr << elem
			tok := p.tok
			pos := tok.position()
			p.next()
			right := p.expr(precedence - 1)
			node = ast.InfixExpr{
				left: node
				right: right
				op: tok.kind
				pos: pos
			}
		} else if p.tok.kind.is_infix() {
			// return early for deref assign `*x = 2` goes to prefix expr
			if p.tok.kind == .mul && p.tok.line_nr != p.prev_tok.line_nr && p.peek_tok2.kind ==
				.assign {
				return node
			}
			// continue on infix expr
			node = p.infix_expr(node)
		} else if p.tok.kind in [.inc, .dec] {
			// Postfix
			node = ast.PostfixExpr{
				op: p.tok.kind
				expr: node
				pos: p.tok.position()
			}
			p.next()
			// return node // TODO bring back, only allow ++/-- in exprs in translated code
		} else {
			return node
		}
	}
	return node
}

fn (mut p Parser) infix_expr(left ast.Expr) ast.Expr {
	op := p.tok.kind
	// mut typ := p.
	// println('infix op=$op.str()')
	precedence := p.tok.precedence()
	pos := p.tok.position()
	p.next()
	mut right := ast.Expr{}
	if op == .key_is {
		p.expecting_type = true
	}
	right = p.expr(precedence)
	return ast.InfixExpr{
		left: left
		right: right
		op: op
		pos: pos
	}
}

fn (mut p Parser) prefix_expr() ast.PrefixExpr {
	pos := p.tok.position()
	op := p.tok.kind
	if op == .amp {
		p.is_amp = true
	}
	// if op == .mul && !p.inside_unsafe {
	// p.warn('unsafe')
	// }
	p.next()
	mut right := ast.Expr{}
	if op == .minus {
		right = p.expr(token.Precedence.call)
	} else {
		right = p.expr(token.Precedence.prefix)
	}
	p.is_amp = false
	return ast.PrefixExpr{
		op: op
		right: right
		pos: pos
	}
}

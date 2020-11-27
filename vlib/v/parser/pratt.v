// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token

pub fn (mut p Parser) expr(precedence int) ast.Expr {
	$if trace_parser ? {
		tok_pos := p.tok.position()
		eprintln('parsing file: ${p.file_name:-30} | tok.kind: ${p.tok.kind:-10} | tok.lit: ${p.tok.lit:-10} | tok_pos: ${tok_pos.str():-45} | expr($precedence)')
	}
	// println('\n\nparser.expr()')
	mut node := ast.Expr{}
	is_stmt_ident := p.is_stmt_ident
	p.is_stmt_ident = false
	if !p.pref.is_fmt {
		p.eat_comments()
	}
	// Prefix
	match p.tok.kind {
		.key_mut, .key_shared, .key_atomic, .key_static {
			node = p.name_expr()
			p.is_stmt_ident = is_stmt_ident
		}
		.name {
			if p.tok.lit == 'sql' && p.peek_tok.kind == .name {
				p.inside_match = true // reuse the same var for perf instead of inside_sql TODO rename
				node = p.sql_expr()
				p.inside_match = false
			} else {
				node = p.name_expr()
				p.is_stmt_ident = is_stmt_ident
			}
		}
		.string {
			node = p.string_expr()
		}
		.comment {
			node = p.comment()
		}
		.dot {
			// .enum_val
			node = p.enum_val()
		}
		.at {
			node = p.at()
		}
		.dollar {
			match p.peek_tok.kind {
				.name { return p.vweb() }
				.key_if { return p.if_expr(true) }
				else { p.error('unexpected $') }
			}
		}
		.chartoken {
			node = ast.CharLiteral{
				val: p.tok.lit
				pos: p.tok.position()
			}
			p.next()
		}
		.minus, .amp, .mul, .not, .bit_not, .arrow {
			// -1, -a, !x, &x, ~x, <-a
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
		.key_select {
			node = p.select_expr()
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
				pos: p.tok.position()
			}
		}
		.key_if {
			node = p.if_expr(false)
		}
		.key_unsafe {
			// unsafe {
			pos := p.tok.position()
			p.next()
			if p.inside_unsafe {
				p.error_with_pos('already inside `unsafe` block', pos)
			}
			p.inside_unsafe = true
			p.check(.lcbr)
			node = ast.UnsafeExpr{
				expr: p.expr(0)
				pos: pos
			}
			p.check(.rcbr)
			p.inside_unsafe = false
		}
		.key_lock, .key_rlock {
			node = p.lock_expr()
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
			pos := p.tok.position()
			p.next()
			node = ast.None{
				pos: pos
			}
		}
		.key_sizeof {
			pos := p.tok.position()
			p.next() // sizeof
			p.check(.lpar)
			is_known_var := p.mark_var_as_used(p.tok.lit)
			if is_known_var {
				expr := p.parse_ident(table.Language.v)
				node = ast.SizeOf{
					is_type: false
					expr: expr
					pos: pos
				}
			} else {
				save_expr_mod := p.expr_mod
				p.expr_mod = ''
				sizeof_type := p.parse_type()
				p.expr_mod = save_expr_mod
				node = ast.SizeOf{
					is_type: true
					typ: sizeof_type
					type_name: p.table.get_type_symbol(sizeof_type).name
					pos: pos
				}
			}
			p.check(.rpar)
		}
		.key_typeof {
			spos := p.tok.position()
			p.next()
			p.check(.lpar)
			expr := p.expr(0)
			p.check(.rpar)
			node = ast.TypeOf{
				expr: expr
				pos: spos.extend(p.tok.position())
			}
		}
		.key_likely, .key_unlikely {
			is_likely := p.tok.kind == .key_likely
			p.next()
			p.check(.lpar)
			lpos := p.tok.position()
			expr := p.expr(0)
			p.check(.rpar)
			node = ast.Likely{
				expr: expr
				pos: lpos
				is_likely: is_likely
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
			if p.expecting_type {
				// Anonymous function type
				start_pos := p.tok.position()
				return ast.Type{
					typ: p.parse_type()
					pos: start_pos.extend(p.prev_tok.position())
				}
			} else {
				// Anonymous function
				node = p.anon_fn()
				// its a call
				// NOTE: this could be moved to just before the pratt loop
				// then anything can be a call, eg. `index[2]()` or `struct.field()`
				// but this would take a bit of modification
				if p.tok.kind == .lpar {
					p.next()
					pos := p.tok.position()
					args := p.call_args()
					p.check(.rpar)
					node = ast.CallExpr{
						name: 'anon'
						left: node
						args: args
						pos: pos
					}
				}
				return node
			}
		}
		else {
			p.error('expr(): bad token `$p.tok.kind.str()`')
		}
	}
	return p.expr_with_left(node, precedence, is_stmt_ident)
}

pub fn (mut p Parser) expr_with_left(left ast.Expr, precedence int, is_stmt_ident bool) ast.Expr {
	mut node := left
	// Infix
	for precedence < p.tok.precedence() {
		if p.tok.kind == .dot {
			node = p.dot_expr(node)
			p.is_stmt_ident = is_stmt_ident
		} else if p.tok.kind == .lsbr {
			node = p.index_expr(node)
			p.is_stmt_ident = is_stmt_ident
		} else if p.tok.kind == .key_as {
			// sum type as cast `x := SumType as Variant`
			pos := p.tok.position()
			p.next()
			typ := p.parse_type()
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
			if p.tok.kind.is_prefix() && p.tok.line_nr != p.prev_tok.line_nr {
				// return early for deref assign `*x = 2` goes to prefix expr
				if p.tok.kind == .mul && p.peek_tok2.kind == .assign {
					return node
				}
				// added 10/2020: LATER this will be parsed as PrefixExpr instead
				p.warn_with_pos('move infix `$p.tok.kind` operator before new line (if infix intended) or use brackets for a prefix expression',
					p.tok.position())
			}
			// continue on infix expr
			node = p.infix_expr(node)
			// return early `if bar is SumType as b {`
			if p.tok.kind == .key_as && p.inside_if {
				return node
			}
		} else if p.tok.kind in [.inc, .dec] || (p.tok.kind == .question && p.inside_ct_if_expr) {
			// Postfix
			// detect `f(x++)`, `a[x++]`
			if p.peek_tok.kind in [.rpar, .rsbr] && p.mod !in ['builtin', 'regex', 'strconv'] { // temp
				p.warn_with_pos('`$p.tok.kind` operator can only be used as a statement',
					p.peek_tok.position())
			}
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
	if op == .arrow {
		p.register_auto_import('sync')
	}
	// mut typ := p.
	// println('infix op=$op.str()')
	precedence := p.tok.precedence()
	pos := p.tok.position()
	p.next()
	mut right := ast.Expr{}
	prev_expecting_type := p.expecting_type
	if op in [.key_is, .not_is] {
		p.expecting_type = true
	}
	right = p.expr(precedence)
	p.expecting_type = prev_expecting_type
	if p.pref.is_vet && op in [.key_in, .not_in] && right is ast.ArrayInit && (right as ast.ArrayInit).exprs.len ==
		1 {
		p.vet_error('Use `var == value` instead of `var in [value]`', pos.line_nr)
	}
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
	if op == .arrow {
		p.register_auto_import('sync')
	}
	// if op == .mul && !p.inside_unsafe {
	// p.warn('unsafe')
	// }
	p.next()
	mut right := if op == .minus { p.expr(token.Precedence.call) } else { p.expr(token.Precedence.prefix) }
	p.is_amp = false
	if mut right is ast.CastExpr {
		right.in_prexpr = true
	}
	mut or_stmts := []ast.Stmt{}
	mut or_kind := ast.OrKind.absent
	mut or_pos := p.tok.position()
	// allow `x := <-ch or {...}` to handle closed channel
	if op == .arrow {
		if p.tok.kind == .key_orelse {
			p.next()
			p.open_scope()
			p.scope.register('errcode', ast.Var{
				name: 'errcode'
				typ: table.int_type
				pos: p.tok.position()
				is_used: true
			})
			p.scope.register('err', ast.Var{
				name: 'err'
				typ: table.string_type
				pos: p.tok.position()
				is_used: true
			})
			or_kind = .block
			or_stmts = p.parse_block_no_scope(false)
			or_pos = or_pos.extend(p.prev_tok.position())
			p.close_scope()
		}
		if p.tok.kind == .question {
			p.next()
			or_kind = .propagate
		}
	}
	return ast.PrefixExpr{
		op: op
		right: right
		pos: pos
		or_block: ast.OrExpr{
			stmts: or_stmts
			kind: or_kind
			pos: or_pos
		}
	}
}

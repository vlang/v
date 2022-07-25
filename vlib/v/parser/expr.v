// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.vet
import v.token

pub fn (mut p Parser) expr(precedence int) ast.Expr {
	return p.check_expr(precedence) or {
		if token.is_decl(p.tok.kind) && p.disallow_declarations_in_script_mode() {
			return ast.empty_expr
		}
		p.error_with_pos('invalid expression: unexpected $p.tok', p.tok.pos())
	}
}

pub fn (mut p Parser) check_expr(precedence int) ?ast.Expr {
	p.trace_parser('expr($precedence)')
	mut node := ast.empty_expr
	is_stmt_ident := p.is_stmt_ident
	p.is_stmt_ident = false
	if !p.pref.is_fmt {
		p.eat_comments()
	}
	if p.inside_if_cond {
		p.if_cond_comments << p.eat_comments()
	}
	inside_array_lit := p.inside_array_lit
	p.inside_array_lit = false
	defer {
		p.inside_array_lit = inside_array_lit
	}
	// Prefix
	match p.tok.kind {
		.key_mut, .key_shared, .key_atomic, .key_static, .key_volatile {
			ident := p.parse_ident(ast.Language.v)
			node = ident
			if p.inside_defer {
				if !p.defer_vars.any(it.name == ident.name && it.mod == ident.mod)
					&& ident.name != 'err' {
					p.defer_vars << ident
				}
			}
			p.is_stmt_ident = is_stmt_ident
		}
		.name, .question {
			if p.tok.lit == 'sql' && p.peek_tok.kind == .name {
				p.inside_match = true // reuse the same var for perf instead of inside_sql TODO rename
				node = p.sql_expr()
				p.inside_match = false
			} else if p.tok.lit == 'map' && p.peek_tok.kind == .lcbr && !(p.builtin_mod
				&& p.file_base in ['map.v', 'map_d_gcboehm_opt.v']) {
				p.error_with_pos("deprecated map syntax, use syntax like `{'age': 20}`",
					p.tok.pos())
			} else {
				if p.inside_comptime_if && p.is_generic_name() && p.peek_tok.kind != .dot {
					// $if T is string {}
					p.expecting_type = true
				}
				node = p.name_expr()
				p.is_stmt_ident = is_stmt_ident
			}
		}
		.string {
			node = p.string_expr()
		}
		.comment {
			node = p.comment()
			return node
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
				.name {
					if p.peek_tok.lit in comptime_types {
						node = p.parse_comptime_type()
					} else {
						node = p.comptime_call()
					}
					p.is_stmt_ident = is_stmt_ident
				}
				.key_if {
					return p.if_expr(true)
				}
				else {
					return p.error_with_pos('unexpected `$`', p.peek_tok.pos())
				}
			}
		}
		.chartoken {
			node = ast.CharLiteral{
				val: p.tok.lit
				pos: p.tok.pos()
			}
			p.next()
		}
		.amp, .mul, .not, .bit_not, .arrow {
			// &x, *x, !x, ~x, <-x
			node = p.prefix_expr()
		}
		.minus {
			// -1, -a
			if p.peek_tok.kind == .number {
				node = p.parse_number_literal()
			} else {
				node = p.prefix_expr()
			}
		}
		.key_go {
			mut go_expr := p.go_expr()
			go_expr.is_expr = true
			node = go_expr
		}
		.key_true, .key_false {
			node = ast.BoolLiteral{
				val: p.tok.kind == .key_true
				pos: p.tok.pos()
			}
			p.next()
		}
		.key_match {
			node = p.match_expr()
		}
		.key_select {
			node = p.select_expr()
		}
		.key_nil {
			node = ast.Nil{
				pos: p.tok.pos()
			}
			p.next()
		}
		.number {
			node = p.parse_number_literal()
		}
		.lpar {
			mut pos := p.tok.pos()
			p.check(.lpar)
			node = p.expr(0)
			p.check(.rpar)
			node = ast.ParExpr{
				expr: node
				pos: pos.extend(p.prev_tok.pos())
			}
		}
		.key_if {
			node = p.if_expr(false)
		}
		.key_unsafe {
			// unsafe {
			mut pos := p.tok.pos()
			p.next()
			if p.inside_unsafe {
				return p.error_with_pos('already inside `unsafe` block', pos)
			}
			p.inside_unsafe = true
			p.check(.lcbr)
			e := p.expr(0)
			p.check(.rcbr)
			pos.update_last_line(p.prev_tok.line_nr)
			node = ast.UnsafeExpr{
				expr: e
				pos: pos
			}
			p.inside_unsafe = false
		}
		.key_lock, .key_rlock {
			node = p.lock_expr()
		}
		.lsbr {
			if p.expecting_type {
				// parse json.decode type (`json.decode([]User, s)`)
				node = p.name_expr()
			} else if p.is_amp && p.peek_tok.kind == .rsbr && p.peek_token(3).kind != .lcbr {
				pos := p.tok.pos()
				typ := p.parse_type()
				typname := p.table.sym(typ).name
				p.check(.lpar)
				expr := p.expr(0)
				p.check(.rpar)
				node = ast.CastExpr{
					typ: typ
					typname: typname
					expr: expr
					pos: pos
				}
			} else {
				node = p.array_init()
			}
		}
		.key_none {
			pos := p.tok.pos()
			p.next()
			node = ast.None{
				pos: pos
			}
		}
		.key_sizeof, .key_isreftype {
			is_reftype := p.tok.kind == .key_isreftype
			p.next() // sizeof
			p.check(.lpar)
			pos := p.tok.pos()
			mut is_known_var := p.mark_var_as_used(p.tok.lit)
				|| p.table.global_scope.known_const(p.mod + '.' + p.tok.lit)
			//|| p.table.known_fn(p.mod + '.' + p.tok.lit)
			// assume `mod.` prefix leads to a type
			mut is_type := p.known_import(p.tok.lit) || p.tok.kind.is_start_of_type()
				|| (p.tok.lit.len > 0 && p.tok.lit[0].is_capital())

			if p.tok.lit in ['c', 'r'] && p.peek_tok.kind == .string {
				is_known_var = false
				is_type = false
			}
			if is_known_var || !is_type {
				expr := p.expr(0)
				if is_reftype {
					node = ast.IsRefType{
						is_type: false
						expr: expr
						pos: pos
					}
				} else {
					node = ast.SizeOf{
						is_type: false
						expr: expr
						pos: pos
					}
				}
			} else {
				if p.tok.kind == .name {
					p.register_used_import(p.tok.lit)
				}
				save_expr_mod := p.expr_mod
				p.expr_mod = ''
				arg_type := p.parse_type()
				p.expr_mod = save_expr_mod
				if is_reftype {
					node = ast.IsRefType{
						is_type: true
						typ: arg_type
						pos: pos
					}
				} else {
					node = ast.SizeOf{
						is_type: true
						typ: arg_type
						pos: pos
					}
				}
			}
			p.check(.rpar)
		}
		.key_typeof {
			spos := p.tok.pos()
			p.next()
			p.check(.lpar)
			expr := p.expr(0)
			p.check(.rpar)
			if p.tok.kind != .dot && p.tok.line_nr == p.prev_tok.line_nr {
				p.warn_with_pos('use e.g. `typeof(expr).name` or `sum_type_instance.type_name()` instead',
					spos)
			}
			node = ast.TypeOf{
				expr: expr
				pos: spos.extend(p.tok.pos())
			}
		}
		.key_dump {
			spos := p.tok.pos()
			p.next()
			p.check(.lpar)
			expr := p.expr(0)
			if p.tok.kind == .comma && p.peek_tok.kind == .rpar {
				p.next()
			}
			p.check(.rpar)
			node = ast.DumpExpr{
				expr: expr
				pos: spos.extend(p.tok.pos())
			}
		}
		.key_offsetof {
			pos := p.tok.pos()
			p.next() // __offsetof
			p.check(.lpar)
			st := p.parse_type()
			p.check(.comma)
			if p.tok.kind != .name {
				return p.error_with_pos('unexpected `$p.tok.lit`, expecting struct field',
					p.tok.pos())
			}
			field := p.tok.lit
			p.next()
			p.check(.rpar)
			node = ast.OffsetOf{
				struct_type: st
				field: field
				pos: pos
			}
		}
		.key_likely, .key_unlikely {
			is_likely := p.tok.kind == .key_likely
			p.next()
			p.check(.lpar)
			lpos := p.tok.pos()
			expr := p.expr(0)
			p.check(.rpar)
			node = ast.Likely{
				expr: expr
				pos: lpos
				is_likely: is_likely
			}
		}
		.lcbr {
			// Map `{"age": 20}`
			p.next()
			node = p.map_init()
			p.check(.rcbr)
		}
		.key_fn {
			if p.expecting_type {
				// Anonymous function type
				start_pos := p.tok.pos()
				return ast.TypeNode{
					typ: p.parse_type()
					pos: start_pos.extend(p.prev_tok.pos())
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
					pos := p.tok.pos()
					args := p.call_args()
					p.check(.rpar)
					node = ast.CallExpr{
						name: 'anon'
						left: node
						args: args
						pos: pos
						scope: p.scope
					}
				}
				return node
			}
		}
		else {
			if p.tok.kind == .key_struct && p.peek_tok.kind == .lcbr {
				// Anonymous struct
				p.next()
				return p.struct_init('', .anon)
			}
			if p.tok.kind != .eof && !(p.tok.kind == .rsbr && p.inside_asm) {
				// eof should be handled where it happens
				return none
				// return p.error_with_pos('invalid expression: unexpected $p.tok', p.tok.pos())
			}
		}
	}
	if inside_array_lit {
		if p.tok.kind in [.minus, .mul, .amp, .arrow] && p.tok.pos + 1 == p.peek_tok.pos
			&& p.prev_tok.pos + p.prev_tok.len + 1 != p.peek_tok.pos {
			return node
		}
	}
	if p.inside_if_cond {
		p.if_cond_comments << p.eat_comments()
	}
	return p.expr_with_left(node, precedence, is_stmt_ident)
}

pub fn (mut p Parser) expr_with_left(left ast.Expr, precedence int, is_stmt_ident bool) ast.Expr {
	mut node := left
	if p.inside_asm && p.prev_tok.pos().line_nr < p.tok.pos().line_nr {
		return node
	}
	// Infix
	for precedence < p.tok.precedence() {
		if p.tok.kind == .dot { //&& (p.tok.line_nr == p.prev_tok.line_nr
			// TODO fix a bug with prev_tok.last_line
			//|| p.prev_tok.pos().last_line == p.tok.line_nr) {
			// if p.fileis('vcache.v') {
			// p.warn('tok.line_nr = $p.tok.line_nr; prev_tok.line_nr=$p.prev_tok.line_nr;
			// prev_tok.last_line=$p.prev_tok.pos().last_line')
			//}
			node = p.dot_expr(node)
			if p.name_error {
				return node
			}
			p.is_stmt_ident = is_stmt_ident
		} else if p.tok.kind in [.lsbr, .nilsbr] && (p.tok.line_nr == p.prev_tok.line_nr
			|| (p.prev_tok.kind == .string
			&& p.tok.line_nr == p.prev_tok.line_nr + p.prev_tok.lit.count('\n'))) {
			if p.tok.kind == .nilsbr {
				node = p.index_expr(node, true)
			} else {
				node = p.index_expr(node, false)
			}

			p.is_stmt_ident = is_stmt_ident
			if p.tok.kind == .lpar && p.tok.line_nr == p.prev_tok.line_nr && node is ast.IndexExpr {
				p.next()
				pos := p.tok.pos()
				args := p.call_args()
				p.check(.rpar)
				node = ast.CallExpr{
					left: node
					args: args
					pos: pos
					scope: p.scope
				}
				p.is_stmt_ident = is_stmt_ident
			}
		} else if p.tok.kind == .key_as {
			// sum type as cast `x := SumType as Variant`
			if !p.inside_asm {
				pos := p.tok.pos()
				p.next()
				typ := p.parse_type()
				node = ast.AsCast{
					expr: node
					typ: typ
					pos: pos
				}
			} else {
				return node
			}
		} else if p.tok.kind == .left_shift && p.is_stmt_ident {
			// arr << elem
			tok := p.tok
			mut pos := tok.pos()
			p.next()
			right := p.expr(precedence - 1)
			pos.update_last_line(p.prev_tok.line_nr)
			if mut node is ast.IndexExpr {
				node.recursive_arraymap_set_is_setter()
			}
			node = ast.InfixExpr{
				left: node
				right: right
				op: tok.kind
				pos: pos
				is_stmt: true
			}
		} else if p.tok.kind.is_infix() && !(p.tok.kind in [.minus, .amp, .mul]
			&& p.tok.line_nr != p.prev_tok.line_nr) {
			// continue on infix expr
			node = p.infix_expr(node)
			// return early `if bar is SumType as b {`
			if p.tok.kind == .key_as && p.inside_if {
				return node
			}
		} else if p.tok.kind in [.inc, .dec] || (p.tok.kind == .question && p.inside_ct_if_expr) {
			// Postfix
			// detect `f(x++)`, `a[x++]`
			if p.peek_tok.kind in [.rpar, .rsbr] {
				if !p.inside_ct_if_expr {
					p.warn_with_pos('`$p.tok.kind` operator can only be used as a statement',
						p.tok.pos())
				}
			}
			if p.tok.kind in [.inc, .dec] && p.prev_tok.line_nr != p.tok.line_nr {
				p.error_with_pos('$p.tok must be on the same line as the previous token',
					p.tok.pos())
			}
			if mut node is ast.IndexExpr {
				node.recursive_mapset_is_setter(true)
			}
			is_c2v_prefix := p.peek_tok.kind == .dollar
			node = ast.PostfixExpr{
				op: p.tok.kind
				expr: node
				pos: p.tok.pos()
				is_c2v_prefix: is_c2v_prefix
			}
			if is_c2v_prefix {
				p.next()
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
		p.or_is_handled = true
		p.register_auto_import('sync')
	}
	precedence := p.tok.precedence()
	mut pos := p.tok.pos()
	p.next()
	if p.inside_if_cond {
		p.if_cond_comments << p.eat_comments()
	}
	mut right := ast.empty_expr
	prev_expecting_type := p.expecting_type
	if op in [.key_is, .not_is] {
		p.expecting_type = true
	}
	is_key_in := op in [.key_in, .not_in]
	if is_key_in {
		p.inside_in_array = true
	}

	right = p.expr(precedence)
	if is_key_in {
		p.inside_in_array = false
	}
	p.expecting_type = prev_expecting_type
	if p.pref.is_vet && op in [.key_in, .not_in] && right is ast.ArrayInit
		&& (right as ast.ArrayInit).exprs.len == 1 {
		p.vet_error('Use `var == value` instead of `var in [value]`', pos.line_nr, vet.FixKind.vfmt,
			.default)
	}
	mut or_stmts := []ast.Stmt{}
	mut or_kind := ast.OrKind.absent
	mut or_pos := p.tok.pos()
	// allow `x := <-ch or {...}` to handle closed channel
	if op == .arrow {
		if p.tok.kind == .key_orelse {
			or_kind = .block
			or_stmts, or_pos = p.or_block(.with_err_var)
		}
		if p.tok.kind == .question {
			p.next()
			or_kind = .propagate_option
		}
		p.or_is_handled = false
	}
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.InfixExpr{
		left: left
		right: right
		op: op
		pos: pos
		is_stmt: p.is_stmt_ident
		or_block: ast.OrExpr{
			stmts: or_stmts
			kind: or_kind
			pos: or_pos
		}
	}
}

fn (p &Parser) fileis(s string) bool {
	return p.file_name.contains(s)
}

fn (mut p Parser) prefix_expr() ast.Expr {
	mut pos := p.tok.pos()
	op := p.tok.kind
	if op == .amp {
		p.is_amp = true
	}
	if op == .arrow {
		p.or_is_handled = true
		p.register_auto_import('sync')
	}
	// if op == .mul && !p.inside_unsafe {
	// p.warn('unsafe')
	// }
	p.next()
	mut right := p.expr(int(token.Precedence.prefix))
	p.is_amp = false
	if op == .amp {
		if mut right is ast.CastExpr {
			// Handle &Type(x), as well as &&Type(x) etc:
			p.recast_as_pointer(mut right, pos)
			return right
		}
		if mut right is ast.SelectorExpr {
			// Handle &Type(x).name :
			if mut right.expr is ast.CastExpr {
				p.recast_as_pointer(mut right.expr, pos)
				return right
			}
		}
		if mut right is ast.IndexExpr {
			// Handle &u64(x)[idx] :
			if mut right.left is ast.CastExpr {
				p.recast_as_pointer(mut right.left, pos)
				return right
			}
		}
		if mut right is ast.ParExpr {
			if right.expr is ast.StructInit {
				p.note_with_pos('unnecessary `()`, use `&$right.expr` instead of `&($right.expr)`',
					right.pos)
				right = right.expr
			}
		}
	}
	mut or_stmts := []ast.Stmt{}
	mut or_kind := ast.OrKind.absent
	mut or_pos := p.tok.pos()
	// allow `x := <-ch or {...}` to handle closed channel
	if op == .arrow {
		if p.tok.kind == .key_orelse {
			or_kind = .block
			or_stmts, or_pos = p.or_block(.with_err_var)
		}
		if p.tok.kind == .question {
			p.next()
			or_kind = .propagate_option
		}
		p.or_is_handled = false
	}
	pos.update_last_line(p.prev_tok.line_nr)
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

fn (mut p Parser) recast_as_pointer(mut cast_expr ast.CastExpr, pos token.Pos) {
	cast_expr.typ = cast_expr.typ.ref()
	cast_expr.typname = p.table.sym(cast_expr.typ).name
	cast_expr.pos = pos.extend(cast_expr.pos)
}

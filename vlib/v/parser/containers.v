// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast

fn (mut p Parser) array_init() ast.ArrayInit {
	first_pos := p.tok.pos()
	mut last_pos := p.tok.pos()
	p.check(.lsbr)
	// p.warn('array_init() exp=$p.expected_type')
	mut array_type := ast.void_type
	mut elem_type := ast.void_type
	mut elem_type_pos := first_pos
	mut exprs := []ast.Expr{}
	mut ecmnts := [][]ast.Comment{}
	mut pre_cmnts := []ast.Comment{}
	mut is_fixed := false
	mut has_val := false
	mut has_type := false
	mut has_default := false
	mut has_it := false
	mut default_expr := ast.empty_expr
	if p.tok.kind == .rsbr {
		last_pos = p.tok.pos()
		// []typ => `[]` and `typ` must be on the same line
		line_nr := p.tok.line_nr
		p.next()
		// []string
		if p.tok.kind in [.name, .amp, .lsbr, .key_shared] && p.tok.line_nr == line_nr {
			elem_type_pos = p.tok.pos()
			elem_type = p.parse_type()
			// this is set here because it's a known type, others could be the
			// result of expr so we do those in checker
			idx := p.table.find_or_register_array(elem_type)
			if elem_type.has_flag(.generic) {
				array_type = ast.new_type(idx).set_flag(.generic)
			} else {
				array_type = ast.new_type(idx)
			}
			has_type = true
		}
		last_pos = p.tok.pos()
	} else {
		// [1,2,3] or [const]u8
		old_inside_array_lit := p.inside_array_lit
		p.inside_array_lit = true
		pre_cmnts = p.eat_comments()
		for i := 0; p.tok.kind !in [.rsbr, .eof]; i++ {
			exprs << p.expr(0)
			ecmnts << p.eat_comments()
			if p.tok.kind == .comma {
				p.next()
			}
			ecmnts.last() << p.eat_comments()
		}
		p.inside_array_lit = old_inside_array_lit
		line_nr := p.tok.line_nr
		$if tinyc {
			// Note: do not remove the next line without testing
			// v selfcompilation with tcc first
			tcc_stack_bug := 12345
			_ = tcc_stack_bug
		}
		last_pos = p.tok.pos()
		p.check(.rsbr)
		if exprs.len == 1 && p.tok.line_nr == line_nr
			&& (p.tok.kind in [.name, .amp] || (p.tok.kind == .lsbr && p.is_array_type())) {
			// [100]u8
			elem_type = p.parse_type()
			if p.table.sym(elem_type).name == 'byte' {
				p.error('`byte` has been deprecated in favor of `u8`: use `[10]u8{}` instead of `[10]byte{}`')
			}
			last_pos = p.tok.pos()
			is_fixed = true
			if p.tok.kind == .lcbr {
				p.next()
				if p.tok.kind != .rcbr {
					pos := p.tok.pos()
					n := p.check_name()
					if n != 'init' {
						p.error_with_pos('expected `init:`, not `$n`', pos)
						return ast.ArrayInit{}
					}
					p.check(.colon)
					p.open_scope()
					has_default = true
					p.scope_register_it_as_index()
					default_expr = p.expr(0)
					has_it = if var := p.scope.find_var('it') {
						mut variable := unsafe { var }
						is_used := variable.is_used
						variable.is_used = true
						is_used
					} else {
						false
					}
					p.close_scope()
				}
				last_pos = p.tok.pos()
				p.check(.rcbr)
			} else {
				p.warn_with_pos('use e.g. `x := [1]Type{}` instead of `x := [1]Type`',
					first_pos.extend(last_pos))
			}
		} else {
			if p.tok.kind == .not { // && p.tok.line_nr == p.prev_tok.line_nr {
				last_pos = p.tok.pos()
				is_fixed = true
				has_val = true
				p.next()
			}
			if p.tok.kind == .not && p.tok.line_nr == p.prev_tok.line_nr {
				last_pos = p.tok.pos()
				p.error_with_pos('use e.g. `[1, 2, 3]!` instead of `[1, 2, 3]!!`', last_pos)
				p.next()
			}
		}
	}
	if exprs.len == 0 && p.tok.kind != .lcbr && has_type {
		if !p.pref.is_fmt {
			p.warn_with_pos('use `x := []Type{}` instead of `x := []Type`', first_pos.extend(last_pos))
		}
	}
	mut has_len := false
	mut has_cap := false
	mut len_expr := ast.empty_expr
	mut cap_expr := ast.empty_expr
	if p.tok.kind == .lcbr && exprs.len == 0 && array_type != ast.void_type {
		// `[]int{ len: 10, cap: 100}` syntax
		p.next()
		for p.tok.kind != .rcbr {
			key := p.check_name()
			p.check(.colon)
			match key {
				'len' {
					has_len = true
					len_expr = p.expr(0)
				}
				'cap' {
					has_cap = true
					cap_expr = p.expr(0)
				}
				'init' {
					p.open_scope()
					has_default = true
					p.scope_register_it_as_index()
					default_expr = p.expr(0)
					has_it = if var := p.scope.find_var('it') {
						mut variable := unsafe { var }
						is_used := variable.is_used
						variable.is_used = true
						is_used
					} else {
						false
					}
					p.close_scope()
				}
				else {
					p.error('wrong field `$key`, expecting `len`, `cap`, or `init`')
					return ast.ArrayInit{}
				}
			}
			if p.tok.kind != .rcbr {
				p.check(.comma)
			}
		}
		p.check(.rcbr)
	}
	pos := first_pos.extend_with_last_line(last_pos, p.prev_tok.line_nr)
	return ast.ArrayInit{
		is_fixed: is_fixed
		has_val: has_val
		mod: p.mod
		elem_type: elem_type
		typ: array_type
		exprs: exprs
		ecmnts: ecmnts
		pre_cmnts: pre_cmnts
		pos: pos
		elem_type_pos: elem_type_pos
		has_len: has_len
		len_expr: len_expr
		has_cap: has_cap
		has_default: has_default
		has_it: has_it
		cap_expr: cap_expr
		default_expr: default_expr
	}
}

// parse tokens between braces
fn (mut p Parser) map_init() ast.MapInit {
	first_pos := p.prev_tok.pos()
	mut keys := []ast.Expr{}
	mut vals := []ast.Expr{}
	mut comments := [][]ast.Comment{}
	pre_cmnts := p.eat_comments()
	for p.tok.kind !in [.rcbr, .eof] {
		key := p.expr(0)
		keys << key
		p.check(.colon)
		val := p.expr(0)
		vals << val
		if p.tok.kind == .comma {
			p.next()
		}
		comments << p.eat_comments()
	}
	return ast.MapInit{
		keys: keys
		vals: vals
		pos: first_pos.extend_with_last_line(p.tok.pos(), p.tok.line_nr)
		comments: comments
		pre_cmnts: pre_cmnts
	}
}

fn (mut p Parser) scope_register_it_as_index() {
	p.scope.objects['it'] = ast.Var{ // override it variable if it already exist, else create it variable
		name: 'it'
		pos: p.tok.pos()
		typ: ast.int_type
		is_mut: false
		is_used: false
	}
}

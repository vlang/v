// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table

fn (mut p Parser) array_init() ast.ArrayInit {
	first_pos := p.tok.position()
	mut last_pos := p.tok.position()
	p.check(.lsbr)
	// p.warn('array_init() exp=$p.expected_type')
	mut array_type := table.void_type
	mut elem_type := table.void_type
	mut elem_type_pos := first_pos
	mut exprs := []ast.Expr{}
	mut ecmnts := [][]ast.Comment{}
	mut is_fixed := false
	mut has_val := false
	mut has_type := false
	mut has_default := false
	mut default_expr := ast.Expr{}
	if p.tok.kind == .rsbr {
		last_pos = p.tok.position()
		// []typ => `[]` and `typ` must be on the same line
		line_nr := p.tok.line_nr
		p.next()
		// []string
		if p.tok.kind in [.name, .amp, .lsbr] && p.tok.line_nr == line_nr {
			elem_type_pos = p.tok.position()
			elem_type = p.parse_type()
			// this is set here because it's a known type, others could be the
			// result of expr so we do those in checker
			idx := p.table.find_or_register_array(elem_type)
			array_type = table.new_type(idx)
			has_type = true
		}
		last_pos = p.tok.position()
	} else {
		// [1,2,3] or [const]byte
		for i := 0; p.tok.kind !in [.rsbr, .eof]; i++ {
			exprs << p.expr(0)
			ecmnts << p.eat_comments()
			if p.tok.kind == .comma {
				p.next()
			}
		}
		line_nr := p.tok.line_nr
		$if tinyc {
			// NB: do not remove the next line without testing
			// v selfcompilation with tcc first
			tcc_stack_bug := 12345
			_ = tcc_stack_bug
		}
		last_pos = p.tok.position()
		p.check(.rsbr)
		if exprs.len == 1 && p.tok.kind in [.name, .amp, .lsbr] && p.tok.line_nr == line_nr {
			// [100]byte
			elem_type = p.parse_type()
			last_pos = p.tok.position()
			is_fixed = true
			if p.tok.kind == .lcbr {
				p.next()
				if p.tok.kind != .rcbr {
					pos := p.tok.position()
					n := p.check_name()
					if n != 'init' {
						p.error_with_pos('expected `init:`, not `$n`', pos)
						return ast.ArrayInit{}
					}
					p.check(.colon)
					has_default = true
					default_expr = p.expr(0)
				}
				last_pos = p.tok.position()
				p.check(.rcbr)
			} else {
				p.warn_with_pos('use e.g. `x := [1]Type{}` instead of `x := [1]Type`',
					first_pos.extend(last_pos))
			}
		} else {
			if p.tok.kind == .not && p.tok.line_nr == p.prev_tok.line_nr {
				last_pos = p.tok.position()
				is_fixed = true
				has_val = true
				p.next()
			}
			if p.tok.kind == .not && p.tok.line_nr == p.prev_tok.line_nr {
				last_pos = p.tok.position()
				p.error_with_pos('use e.g. `[1, 2, 3]!` instead of `[1, 2, 3]!!`', last_pos)
				p.next()
			}
		}
	}
	if exprs.len == 0 && p.tok.kind != .lcbr && has_type {
		p.warn_with_pos('use `x := []Type{}` instead of `x := []Type`', first_pos.extend(last_pos))
	}
	mut has_len := false
	mut has_cap := false
	mut len_expr := ast.Expr{}
	mut cap_expr := ast.Expr{}
	if p.tok.kind == .lcbr && exprs.len == 0 {
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
					has_default = true
					default_expr = p.expr(0)
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
	pos := first_pos.extend(last_pos)
	return ast.ArrayInit{
		is_fixed: is_fixed
		has_val: has_val
		mod: p.mod
		elem_type: elem_type
		typ: array_type
		exprs: exprs
		ecmnts: ecmnts
		pos: pos
		elem_type_pos: elem_type_pos
		has_len: has_len
		len_expr: len_expr
		has_cap: has_cap
		has_default: has_default
		cap_expr: cap_expr
		default_expr: default_expr
	}
}

fn (mut p Parser) map_init() ast.MapInit {
	pos := p.tok.position()
	mut keys := []ast.Expr{}
	mut vals := []ast.Expr{}
	for p.tok.kind != .rcbr && p.tok.kind != .eof {
		key := p.expr(0)
		if key is ast.FloatLiteral {
			p.error_with_pos('maps do not support floating point keys yet', key.pos)
		}
		keys << key
		p.check(.colon)
		val := p.expr(0)
		vals << val
		if p.tok.kind == .comma {
			p.next()
		}
	}
	return ast.MapInit{
		keys: keys
		vals: vals
		pos: pos
	}
}

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token

fn (mut p Parser) parse_array_elem_type(is_option bool, is_fixed bool) (ast.Type, ast.Type) {
	mut elem_type := ast.no_type
	mut array_type := ast.void_type

	elem_type_pos := p.tok.pos()
	elem_type = p.parse_type()
	// this is set here because it's a known type, others could be the
	// result of expr so we do those in checker
	if elem_type != 0 && !is_fixed {
		if elem_type.has_flag(.result) {
			p.error_with_pos('arrays do not support storing Result values', elem_type_pos)
		}
		idx := p.table.find_or_register_array(elem_type)
		if elem_type.has_flag(.generic) {
			array_type = ast.new_type(idx).set_flag(.generic)
		} else {
			array_type = ast.new_type(idx)
		}
		if is_option {
			array_type = array_type.set_flag(.option)
		}
	}
	return elem_type, array_type
}

fn (mut p Parser) parse_array_lit(mut exprs []ast.Expr, mut pre_cmnts []ast.Comment, mut ecmnts [][]ast.Comment) (int, token.Pos) {
	old_inside_array_lit := p.inside_array_lit
	old_last_enum_name := p.last_enum_name
	old_last_enum_mod := p.last_enum_mod
	p.inside_array_lit = true
	p.last_enum_name = ''
	p.last_enum_mod = ''
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
	p.last_enum_name = old_last_enum_name
	p.last_enum_mod = old_last_enum_mod
	line_nr := p.tok.line_nr
	last_pos := p.tok.pos()
	p.check(.rsbr)
	return line_nr, last_pos
}

fn (mut p Parser) array_init(is_option bool, alias_array_type ast.Type) ast.ArrayInit {
	// NOTE: for fixed-array, we set the size in `len_expr`
	first_pos := p.tok.pos()
	mut last_pos := p.tok.pos()
	mut array_type := ast.void_type
	mut elem_type := ast.void_type
	mut elem_type_pos := first_pos
	mut exprs := []ast.Expr{}
	mut ecmnts := [][]ast.Comment{}
	mut pre_cmnts := []ast.Comment{}
	mut is_fixed := false
	mut has_val := false
	mut has_type := false
	mut has_init := false
	mut has_len := false
	mut has_cap := false
	mut has_index := false
	mut init_expr := ast.empty_expr
	mut len_expr := ast.empty_expr
	mut cap_expr := ast.empty_expr
	mut auto_length := false
	mut line_nr := p.tok.line_nr
	mut is_deprecated := false // deprecated fixed-array syntax
	if alias_array_type == ast.void_type {
		p.check(.lsbr)
		if p.tok.kind == .rsbr {
			// empty array: []
			last_pos = p.tok.pos()
			p.next()
		} else if p.tok.kind == .dotdot {
			// auto length fixed array: [..]
			is_fixed = true
			auto_length = true
			p.next()
			last_pos = p.tok.pos()
			p.check(.rsbr)
		} else {
			// Parse length expression or first element
			line_nr, last_pos = p.parse_array_lit(mut exprs, mut pre_cmnts, mut ecmnts)
			has_val = true
		}

		// Parse type declaration if present
		// []typ => `[]` and `typ` must be on the same line
		if exprs.len <= 1 && p.tok.line_nr == line_nr
			&& (p.tok.kind in [.name, .amp, .question, .key_shared]
			|| (p.tok.kind == .lsbr && p.is_array_type())
			|| (p.tok.kind == .not && exprs.len == 0)) // only `[]` can has a `!` type: []!int
		  {
			if exprs.len == 1 {
				// [100]u8
				// fixed-array move `exprs[0]` to `len_expr`
				len_expr = exprs[0]
				exprs.clear()
				ecmnts.clear()
				is_fixed = true
				has_val = false
			}
			elem_type_pos = p.tok.pos()
			elem_type, array_type = p.parse_array_elem_type(is_option, is_fixed)
			if elem_type != 0 {
				has_type = true
			}
			last_pos = p.tok.pos()
		}
	} else {
		array_type = (p.table.sym(alias_array_type).info as ast.Alias).parent_type
		elem_type = p.table.sym(array_type).array_info().elem_type
		last_pos = p.tok.pos()
		p.next()
		has_type = true
	}

	if has_type {
		if p.tok.kind == .lsbr {
			// Parse array lit if present
			p.next()
			line_nr, last_pos = p.parse_array_lit(mut exprs, mut pre_cmnts, mut ecmnts)
			if auto_length {
				// auto length fixed-array set `len_expr`
				len_expr = ast.IntegerLiteral{
					val: exprs.len.str()
				}
			}
			has_val = true
		} else if p.tok.kind == .lcbr {
			// Parse array attribute initialization if present
			p.next()
			mut attr_pos := token.Pos{}
			for p.tok.kind != .rcbr {
				attr_pos = p.tok.pos()
				key := p.check_name()
				p.check(.colon)
				if is_option && !is_fixed {
					p.error('Option array cannot have initializers')
				}
				match key {
					'len' {
						if is_fixed {
							p.error_with_pos('`len` and `cap` are invalid attributes for fixed array dimension',
								attr_pos)
						}
						has_len = true
						len_expr = p.expr(0)
					}
					'cap' {
						if is_fixed {
							p.error_with_pos('`len` and `cap` are invalid attributes for fixed array dimension',
								attr_pos)
						}
						has_cap = true
						cap_expr = p.expr(0)
					}
					'init' {
						has_init = true
						has_index = p.handle_index_variable(mut init_expr)
					}
					else {
						if is_fixed {
							p.error_with_pos('wrong field `${key}`, expecting `init`',
								attr_pos)
						} else {
							p.error_with_pos('wrong field `${key}`, expecting `len`, `cap`, or `init`',
								attr_pos)
						}
						return ast.ArrayInit{}
					}
				}
				if p.tok.kind != .rcbr {
					p.check(.comma)
				}
			}
			p.check(.rcbr)
			if has_init && !has_len && !is_fixed {
				p.error_with_pos('cannot use `init` attribute unless `len` attribute is also provided',
					attr_pos)
			}
		} else if !p.pref.is_fmt {
			modifier := if is_option { '?' } else { '' }
			fixed_modifier := if is_fixed { '1' } else { '' }
			p.warn_with_pos('use `x := ${modifier}[${fixed_modifier}]Type{}` instead of `x := ${modifier}[${fixed_modifier}]Type`',
				first_pos.extend(last_pos))
		}
	} else {
		if p.tok.kind == .not {
			last_pos = p.tok.pos()
			is_fixed = true
			has_val = true
			p.next()
			// deprecated fixed-array set `len_expr`
			len_expr = ast.IntegerLiteral{
				val: exprs.len.str()
			}
			is_deprecated = true
		}
		if p.tok.kind == .not && p.tok.line_nr == p.prev_tok.line_nr {
			last_pos = p.tok.pos()
			p.error_with_pos('use e.g. `[1, 2, 3]!` instead of `[1, 2, 3]!!`', last_pos)
			p.next()
		}
	}

	pos := first_pos.extend_with_last_line(last_pos, p.prev_tok.line_nr)
	return ast.ArrayInit{
		is_fixed:      is_fixed
		has_val:       has_val
		mod:           p.mod
		elem_type:     elem_type
		typ:           array_type
		alias_type:    alias_array_type
		exprs:         exprs
		ecmnts:        ecmnts
		pre_cmnts:     pre_cmnts
		pos:           pos
		elem_type_pos: elem_type_pos
		has_len:       has_len
		len_expr:      len_expr
		has_cap:       has_cap
		has_init:      has_init
		has_index:     has_index
		cap_expr:      cap_expr
		init_expr:     init_expr
		is_option:     is_option
		auto_length:   auto_length
		is_deprecated: is_deprecated
	}
}

// parse tokens between braces
fn (mut p Parser) map_init() ast.MapInit {
	old_inside_map_init := p.inside_map_init
	p.inside_map_init = true
	defer {
		p.inside_map_init = old_inside_map_init
	}
	first_pos := p.prev_tok.pos()
	mut keys := []ast.Expr{}
	mut vals := []ast.Expr{}
	mut comments := [][]ast.Comment{}
	mut has_update_expr := false
	mut update_expr := ast.empty_expr
	mut update_expr_comments := []ast.Comment{}
	mut update_expr_pos := token.Pos{}
	pre_cmnts := p.eat_comments()
	if p.tok.kind == .ellipsis {
		// updating init { ...base_map, 'b': 44, 'c': 55 }
		has_update_expr = true
		p.check(.ellipsis)
		update_expr = p.expr(0)
		update_expr_pos = update_expr.pos()
		if p.tok.kind == .comma {
			p.next()
		}
		update_expr_comments << p.eat_comments(same_line: true)
	}
	for p.tok.kind !in [.rcbr, .eof] {
		if p.tok.kind == .name && p.tok.lit in ['r', 'c', 'js'] {
			key := p.string_expr()
			keys << key
		} else {
			key := p.expr(0)
			keys << key
		}
		p.check(.colon)
		val := p.expr(0)
		vals << val
		if p.tok.kind == .comma {
			p.next()
		}
		comments << p.eat_comments()
	}
	return ast.MapInit{
		keys:                 keys
		vals:                 vals
		pos:                  first_pos.extend_with_last_line(p.tok.pos(), p.tok.line_nr)
		comments:             comments
		pre_cmnts:            pre_cmnts
		has_update_expr:      has_update_expr
		update_expr:          update_expr
		update_expr_pos:      update_expr_pos
		update_expr_comments: update_expr_comments
	}
}

fn (mut p Parser) scope_register_index() {
	p.scope.objects['index'] = ast.Var{ // override index variable if it already exist, else create index variable
		name:         'index'
		pos:          p.tok.pos()
		typ:          ast.int_type
		is_mut:       false
		is_used:      false
		is_index_var: true
	}
	p.scope.objects['it'] = ast.Var{ // it is now deprecated, will be removed in future stable release
		name:    'it'
		pos:     p.tok.pos()
		typ:     ast.int_type
		is_mut:  false
		is_used: false
	}
}

fn (mut p Parser) handle_index_variable(mut default_expr ast.Expr) bool {
	mut has_index := false
	p.open_scope()
	defer {
		p.close_scope()
	}
	p.scope_register_index()
	default_expr = p.expr(0)
	if var := p.scope.find_var('index') {
		mut variable := unsafe { var }
		is_used := variable.is_used
		variable.is_used = true
		has_index = is_used
	}
	if var := p.scope.find_var('it') { // FIXME: Remove this block when `it` is forbidden
		mut variable := unsafe { var }
		is_used := variable.is_used
		if is_used {
			p.warn('variable `it` in array initialization will soon be replaced with `index`')
		}
		variable.is_used = true
		if !has_index {
			has_index = is_used
		}
	}
	return has_index
}

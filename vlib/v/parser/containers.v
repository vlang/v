// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token

fn (mut p Parser) array_init(is_option bool, alias_array_type ast.Type) ast.ArrayInit {
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
	mut has_index := false
	mut init_expr := ast.empty_expr
	if alias_array_type == ast.void_type {
		p.check(.lsbr)
		if p.tok.kind == .rsbr {
			last_pos = p.tok.pos()
			// []typ => `[]` and `typ` must be on the same line
			line_nr := p.tok.line_nr
			p.next()
			// []string
			if p.tok.kind in [.name, .amp, .lsbr, .question, .key_shared, .not]
				&& p.tok.line_nr == line_nr {
				elem_type_pos = p.tok.pos()
				elem_type = p.parse_type()
				// this is set here because it's a known type, others could be the
				// result of expr so we do those in checker
				if elem_type != 0 {
					if elem_type.has_flag(.result) {
						p.error_with_pos('arrays do not support storing Result values',
							elem_type_pos)
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
					has_type = true
				}
			}
			last_pos = p.tok.pos()
		} else {
			// [1,2,3] or [const]u8
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
			last_pos = p.tok.pos()
			p.check(.rsbr)
			if exprs.len == 1 && p.tok.line_nr == line_nr
				&& (p.tok.kind in [.name, .amp, .question, .key_shared]
				|| (p.tok.kind == .lsbr && p.is_array_type())) {
				// [100]u8
				elem_type = p.parse_type()
				if elem_type != 0 {
					s := p.table.sym(elem_type)
					if s.name == 'byte' {
						p.error('`byte` has been deprecated in favor of `u8`: use `[10]u8{}` instead of `[10]byte{}`')
					}
				}
				last_pos = p.tok.pos()
				is_fixed = true
				if p.tok.kind == .lcbr {
					p.next()
					if p.tok.kind != .rcbr {
						pos := p.tok.pos()
						n := p.check_name()
						if n != 'init' {
							if is_fixed {
								p.error_with_pos('`len` and `cap` are invalid attributes for fixed array dimension',
									pos)
							} else {
								p.error_with_pos('expected `init:`, not `${n}`', pos)
							}
							return ast.ArrayInit{}
						}
						p.check(.colon)
						has_init = true
						has_index = p.handle_index_variable(mut init_expr)
					}
					last_pos = p.tok.pos()
					p.check(.rcbr)
				} else {
					modifier := if is_option { '?' } else { '' }
					p.warn_with_pos('use e.g. `x := ${modifier}[1]Type{}` instead of `x := ${modifier}[1]Type`',
						first_pos.extend(last_pos))
				}
			} else {
				if p.tok.kind == .not {
					last_pos = p.tok.pos()
					is_fixed = true
					has_val = true
					p.next()
				}
				if p.tok.kind == .not && p.tok.line_nr == p.prev_tok.line_nr {
					last_pos = p.tok.pos()
					p.error_with_pos('use e.g. `[1, 2, 3]!` instead of `[1, 2, 3]!!`',
						last_pos)
					p.next()
				}
			}
		}
		if exprs.len == 0 && p.tok.kind != .lcbr && has_type {
			if !p.pref.is_fmt {
				modifier := if is_option { '?' } else { '' }
				p.warn_with_pos('use `x := ${modifier}[]Type{}` instead of `x := ${modifier}[]Type`',
					first_pos.extend(last_pos))
			}
		}
	} else {
		array_type = (p.table.sym(alias_array_type).info as ast.Alias).parent_type
		elem_type = p.table.sym(array_type).array_info().elem_type
		p.next()
	}
	mut has_len := false
	mut has_cap := false
	mut len_expr := ast.empty_expr
	mut cap_expr := ast.empty_expr
	mut attr_pos := token.Pos{}
	if p.tok.kind == .lcbr && exprs.len == 0 && array_type != ast.void_type {
		// `[]int{ len: 10, cap: 100}` syntax
		p.next()
		for p.tok.kind != .rcbr {
			attr_pos = p.tok.pos()
			key := p.check_name()
			p.check(.colon)
			if is_option {
				p.error('Option array cannot have initializers')
			}
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
					has_init = true
					has_index = p.handle_index_variable(mut init_expr)
				}
				else {
					p.error_with_pos('wrong field `${key}`, expecting `len`, `cap`, or `init`',
						attr_pos)
					return ast.ArrayInit{}
				}
			}
			if p.tok.kind != .rcbr {
				p.check(.comma)
			}
		}
		p.check(.rcbr)
		if has_init && !has_len {
			p.error_with_pos('cannot use `init` attribute unless `len` attribute is also provided',
				attr_pos)
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

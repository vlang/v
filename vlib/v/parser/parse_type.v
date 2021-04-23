// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.util

pub fn (mut p Parser) parse_array_type() ast.Type {
	p.check(.lsbr)
	// fixed array
	if p.tok.kind in [.number, .name] {
		mut fixed_size := 0
		size_expr := p.expr(0)
		match size_expr {
			ast.IntegerLiteral {
				fixed_size = size_expr.val.int()
			}
			ast.Ident {
				if const_field := p.global_scope.find_const('${p.mod}.$size_expr.name') {
					if const_field.expr is ast.IntegerLiteral {
						fixed_size = const_field.expr.val.int()
					} else {
						p.error_with_pos('non-constant array bound `$size_expr.name`',
							size_expr.pos)
					}
				} else {
					p.error_with_pos('non-constant array bound `$size_expr.name`', size_expr.pos)
				}
			}
			else {
				p.error('expecting `int` for fixed size')
			}
		}
		p.check(.rsbr)
		elem_type := p.parse_type()
		if elem_type.idx() == 0 {
			// error is handled by parse_type
			return 0
		}
		if fixed_size <= 0 {
			p.error_with_pos('fixed size cannot be zero or negative', size_expr.position())
		}
		// sym := p.table.get_type_symbol(elem_type)
		idx := p.table.find_or_register_array_fixed(elem_type, fixed_size)
		if elem_type.has_flag(.generic) {
			return ast.new_type(idx).set_flag(.generic)
		}
		return ast.new_type(idx)
	}
	// array
	p.check(.rsbr)
	elem_type := p.parse_type()
	if elem_type.idx() == 0 {
		// error is set in parse_type
		return 0
	}
	mut nr_dims := 1
	// detect attr
	not_attr := p.peek_tok.kind != .name && p.peek_token(2).kind !in [.semicolon, .rsbr]
	for p.tok.kind == .lsbr && not_attr {
		p.next()
		p.check(.rsbr)
		nr_dims++
	}
	idx := p.table.find_or_register_array_with_dims(elem_type, nr_dims)
	if elem_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_map_type() ast.Type {
	p.next()
	if p.tok.kind != .lsbr {
		return ast.map_type
	}
	p.check(.lsbr)
	key_type := p.parse_type()
	key_sym := p.table.get_type_symbol(key_type)
	is_alias := key_sym.kind == .alias
	if key_type.idx() == 0 {
		// error is reported in parse_type
		return 0
	}
	if is_alias && !(key_type in [ast.string_type_idx, ast.voidptr_type_idx]
		|| ((key_type.is_int() || key_type.is_float()) && !key_type.is_ptr())) {
		p.error('cannot use the alias type as the parent type is unsupported')
		return 0
	}
	if !(key_type in [ast.string_type_idx, ast.voidptr_type_idx]
		|| key_sym.kind == .enum_ || ((key_type.is_int() || key_type.is_float()
		|| is_alias) && !key_type.is_ptr())) {
		s := p.table.type_to_str(key_type)
		p.error_with_pos('maps only support string, integer, float, rune, enum or voidptr keys for now (not `$s`)',
			p.tok.position())
		return 0
	}
	p.check(.rsbr)
	value_type := p.parse_type()
	if value_type.idx() == 0 {
		// error is reported in parse_type
		return 0
	}
	if value_type.idx() == ast.void_type_idx {
		p.error_with_pos('map value type cannot be void', p.tok.position())
		return 0
	}
	idx := p.table.find_or_register_map(key_type, value_type)
	if key_type.has_flag(.generic) || value_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_chan_type() ast.Type {
	if p.peek_tok.kind != .name && p.peek_tok.kind != .key_mut && p.peek_tok.kind != .amp
		&& p.peek_tok.kind != .lsbr {
		p.next()
		return ast.chan_type
	}
	p.register_auto_import('sync')
	p.next()
	is_mut := p.tok.kind == .key_mut
	elem_type := p.parse_type()
	idx := p.table.find_or_register_chan(elem_type, is_mut)
	if elem_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_thread_type() ast.Type {
	is_opt := p.peek_tok.kind == .question
	if is_opt {
		p.next()
	}
	if p.peek_tok.kind != .name && p.peek_tok.kind != .key_mut && p.peek_tok.kind != .amp
		&& p.peek_tok.kind != .lsbr {
		p.next()
		if is_opt {
			mut ret_type := ast.void_type
			ret_type = ret_type.set_flag(.optional)
			idx := p.table.find_or_register_thread(ret_type)
			return ast.new_type(idx)
		} else {
			return ast.thread_type
		}
	}
	if !is_opt {
		p.next()
	}
	ret_type := p.parse_type()
	idx := p.table.find_or_register_thread(ret_type)
	if ret_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_multi_return_type() ast.Type {
	p.check(.lpar)
	mut mr_types := []ast.Type{}
	mut has_generic := false
	for p.tok.kind != .eof {
		mr_type := p.parse_type()
		if mr_type.idx() == 0 {
			break
		}
		if mr_type.has_flag(.generic) {
			has_generic = true
		}
		mr_types << mr_type
		if p.tok.kind == .comma {
			p.next()
		} else {
			break
		}
	}
	p.check(.rpar)
	if mr_types.len == 1 {
		// no multi return type needed
		return mr_types[0]
	}
	idx := p.table.find_or_register_multi_return(mr_types)
	if has_generic {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

// given anon name based off signature when `name` is blank
pub fn (mut p Parser) parse_fn_type(name string) ast.Type {
	// p.warn('parse fn')
	p.check(.key_fn)
	line_nr := p.tok.line_nr
	args, _, is_variadic := p.fn_args()
	mut return_type := ast.void_type
	if p.tok.line_nr == line_nr && p.tok.kind.is_start_of_type() {
		return_type = p.parse_type()
	}
	func := ast.Fn{
		name: name
		params: args
		is_variadic: is_variadic
		return_type: return_type
	}
	// MapFooFn typedefs are manually added in cheaders.v
	// because typedefs get generated after the map struct is generated
	has_decl := p.builtin_mod && name.starts_with('Map') && name.ends_with('Fn')
	idx := p.table.find_or_register_fn_type(p.mod, func, false, has_decl)
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_type_with_mut(is_mut bool) ast.Type {
	typ := p.parse_type()
	if is_mut {
		return typ.set_nr_muls(1)
	}
	return typ
}

// Parses any language indicators on a type.
pub fn (mut p Parser) parse_language() ast.Language {
	language := if p.tok.lit == 'C' {
		ast.Language.c
	} else if p.tok.lit == 'JS' {
		ast.Language.js
	} else {
		ast.Language.v
	}
	if language != .v {
		p.next()
		p.check(.dot)
	}
	return language
}

pub fn (mut p Parser) parse_type() ast.Type {
	// optional
	mut is_optional := false
	if p.tok.kind == .question {
		line_nr := p.tok.line_nr
		p.next()
		is_optional = true
		if p.tok.line_nr > line_nr {
			mut typ := ast.void_type
			if is_optional {
				typ = typ.set_flag(.optional)
			}
			return typ
		}
	}
	is_shared := p.tok.kind == .key_shared
	is_atomic := p.tok.kind == .key_atomic
	if is_shared {
		p.register_auto_import('sync')
	}
	mut nr_muls := 0
	if p.tok.kind == .key_mut || is_shared || is_atomic {
		nr_muls++
		p.next()
	}
	if p.tok.kind == .mul {
		p.error('use `&Type` instead of `*Type` when declaring references')
		return 0
	}
	mut nr_amps := 0
	// &Type
	for p.tok.kind == .amp {
		nr_amps++
		nr_muls++
		p.next()
	}
	language := p.parse_language()
	mut typ := ast.void_type
	is_array := p.tok.kind == .lsbr
	if p.tok.kind != .lcbr {
		pos := p.tok.position()
		typ = p.parse_any_type(language, nr_muls > 0, true)
		if typ.idx() == 0 {
			// error is set in parse_type
			return 0
		}
		if typ == ast.void_type {
			p.error_with_pos('use `?` instead of `?void`', pos)
			return 0
		}
	}
	if is_optional {
		typ = typ.set_flag(.optional)
	}
	if is_shared {
		typ = typ.set_flag(.shared_f)
	}
	if is_atomic {
		typ = typ.set_flag(.atomic_f)
	}
	if nr_muls > 0 {
		typ = typ.set_nr_muls(nr_muls)
		if is_array && nr_amps > 0 {
			p.error('V arrays are already references behind the scenes,
there is no need to use a reference to an array (e.g. use `[]string` instead of `&[]string`).
If you need to modify an array in a function, use a mutable argument instead: `fn foo(mut s []string) {}`.')
			return 0
		}
	}
	return typ
}

pub fn (mut p Parser) parse_any_type(language ast.Language, is_ptr bool, check_dot bool) ast.Type {
	mut name := p.tok.lit
	if language == .c {
		name = 'C.$name'
	} else if language == .js {
		name = 'JS.$name'
	} else if p.peek_tok.kind == .dot && check_dot {
		// `module.Type`
		// /if !(p.tok.lit in p.table.imports) {
		if !p.known_import(name) {
			p.error('unknown module `$p.tok.lit`')
			return 0
		}
		if p.tok.lit in p.imports {
			p.register_used_import(p.tok.lit)
		}
		p.next()
		p.check(.dot)
		// prefix with full module
		name = '${p.imports[name]}.$p.tok.lit'
		if p.tok.lit.len > 0 && !p.tok.lit[0].is_capital() {
			p.error('imported types must start with a capital letter')
			return 0
		}
	} else if p.expr_mod != '' && !p.in_generic_params { // p.expr_mod is from the struct and not from the generic parameter
		name = p.expr_mod + '.' + name
	} else if name in p.imported_symbols {
		name = p.imported_symbols[name]
	} else if !p.builtin_mod && name.len > 1 && name !in p.table.type_idxs {
		// `Foo` in module `mod` means `mod.Foo`
		name = p.mod + '.' + name
	}
	// p.warn('get type $name')
	match p.tok.kind {
		.key_fn {
			// func
			return p.parse_fn_type('')
		}
		.lsbr {
			// array
			return p.parse_array_type()
		}
		.lpar {
			// multiple return
			if is_ptr {
				p.error('parse_type: unexpected `&` before multiple returns')
				return 0
			}
			return p.parse_multi_return_type()
		}
		else {
			// no defer
			if name == 'map' {
				return p.parse_map_type()
			}
			if name == 'chan' {
				return p.parse_chan_type()
			}
			if name == 'thread' {
				return p.parse_thread_type()
			}
			defer {
				p.next()
			}
			if name == '' {
				// This means the developer is using some wrong syntax like `x: int` instead of `x int`
				p.error('expecting type declaration')
				return 0
			}
			match name {
				'voidptr' {
					return ast.voidptr_type
				}
				'byteptr' {
					return ast.byteptr_type
				}
				'charptr' {
					return ast.charptr_type
				}
				'i8' {
					return ast.i8_type
				}
				'i16' {
					return ast.i16_type
				}
				'int' {
					return ast.int_type
				}
				'i64' {
					return ast.i64_type
				}
				'byte' {
					return ast.byte_type
				}
				'u16' {
					return ast.u16_type
				}
				'u32' {
					return ast.u32_type
				}
				'u64' {
					return ast.u64_type
				}
				'f32' {
					return ast.f32_type
				}
				'f64' {
					return ast.f64_type
				}
				'string' {
					return ast.string_type
				}
				'char' {
					return ast.char_type
				}
				'bool' {
					return ast.bool_type
				}
				'float_literal' {
					return ast.float_literal_type
				}
				'int_literal' {
					return ast.int_literal_type
				}
				else {
					if name.len == 1 && name[0].is_capital() {
						return p.parse_generic_template_type(name)
					}
					if p.peek_tok.kind == .lt {
						return p.parse_generic_struct_inst_type(name)
					}
					return p.parse_enum_or_struct_type(name, language)
				}
			}
		}
	}
}

pub fn (mut p Parser) parse_enum_or_struct_type(name string, language ast.Language) ast.Type {
	// struct / enum / placeholder
	// struct / enum
	mut idx := p.table.find_type_idx(name)
	if idx > 0 {
		return ast.new_type(idx)
	}
	// not found - add placeholder
	idx = p.table.add_placeholder_type(name, language)
	// println('NOT FOUND: $name - adding placeholder - $idx')
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_generic_template_type(name string) ast.Type {
	mut idx := p.table.find_type_idx(name)
	if idx > 0 {
		return ast.new_type(idx).set_flag(.generic)
	}
	idx = p.table.register_type_symbol(ast.TypeSymbol{
		name: name
		cname: util.no_dots(name)
		mod: p.mod
		kind: .any
		is_public: true
	})
	return ast.new_type(idx).set_flag(.generic)
}

pub fn (mut p Parser) parse_generic_struct_inst_type(name string) ast.Type {
	mut bs_name := name
	mut bs_cname := name
	p.next()
	p.in_generic_params = true
	bs_name += '<'
	bs_cname += '_T_'
	mut concrete_types := []ast.Type{}
	mut is_instance := false
	for p.tok.kind != .eof {
		gt := p.parse_type()
		if !gt.has_flag(.generic) {
			is_instance = true
		}
		gts := p.table.get_type_symbol(gt)
		bs_name += gts.name
		bs_cname += gts.cname
		concrete_types << gt
		if p.tok.kind != .comma {
			break
		}
		p.next()
		bs_name += ','
		bs_cname += '_'
	}
	p.check(.gt)
	p.in_generic_params = false
	bs_name += '>'
	if is_instance && concrete_types.len > 0 {
		mut gt_idx := p.table.find_type_idx(bs_name)
		if gt_idx > 0 {
			return ast.new_type(gt_idx)
		}
		gt_idx = p.table.add_placeholder_type(bs_name, .v)
		mut parent_idx := p.table.type_idxs[name]
		if parent_idx == 0 {
			parent_idx = p.table.add_placeholder_type(name, .v)
		}
		idx := p.table.register_type_symbol(ast.TypeSymbol{
			kind: .generic_struct_inst
			name: bs_name
			cname: util.no_dots(bs_cname)
			mod: p.mod
			info: ast.GenericStructInst{
				parent_idx: parent_idx
				concrete_types: concrete_types
			}
		})
		return ast.new_type(idx)
	}
	return p.parse_enum_or_struct_type(name, .v).set_flag(.generic)
}

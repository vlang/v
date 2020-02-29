module parser
// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import (
	v.table
)

pub fn (p mut Parser) parse_array_type() table.Type {
	p.check(.lsbr)
	// fixed array
	if p.tok.kind == .number {
		size := p.tok.lit.int()
		p.next()
		p.check(.rsbr)
		elem_type := p.parse_type()
		idx := p.table.find_or_register_array_fixed(elem_type, size, 1)
		return table.new_type(idx)
	}
	// array
	p.check(.rsbr)
	elem_type := p.parse_type()
	mut nr_dims := 1
	for p.tok.kind == .lsbr {
		p.check(.lsbr)
		p.check(.rsbr)
		nr_dims++
	}
	idx := p.table.find_or_register_array(elem_type, nr_dims)
	return table.new_type(idx)
}

pub fn (p mut Parser) parse_map_type() table.Type {
	p.next()
	if p.tok.kind != .lsbr {
		return table.map_type
	}
	p.check(.lsbr)
	key_type := p.parse_type()
	// key_type_sym := p.get_type_symbol(key_type)
	// if key_type_sym.kind != .string {
	if table.type_idx(key_type) != table.string_type_idx {
		p.error('maps can only have string keys for now')
	}
	p.check(.rsbr)
	value_type := p.parse_type()
	idx := p.table.find_or_register_map(key_type, value_type)
	return table.new_type(idx)
}

pub fn (p mut Parser) parse_multi_return_type() table.Type {
	p.check(.lpar)
	mut mr_types := []table.Type
	for {
		mr_type := p.parse_type()
		mr_types << mr_type
		if p.tok.kind == .comma {
			p.check(.comma)
		}
		else {
			break
		}
	}
	p.check(.rpar)
	idx := p.table.find_or_register_multi_return(mr_types)
	return table.new_type(idx)
}

pub fn (p mut Parser) parse_fn_type() table.Type {
	// p.warn('parrse fn')
	p.check(.key_fn)
	// p.fn_decl()
	p.fn_args()
	if p.tok.kind.is_start_of_type() {
		p.parse_type()
	}
	return table.int_type
}

pub fn (p mut Parser) parse_type() table.Type {
	// optional
	mut is_optional := false
	if p.tok.kind == .question {
		p.next()
		is_optional = true
	}
	// &Type
	mut nr_muls := 0
	for p.tok.kind == .amp {
		p.check(.amp)
		nr_muls++
	}
	is_c := p.tok.lit == 'C'
	if is_c {
		p.next()
		p.check(.dot)
	}
	mut typ := p.parse_any_type(is_c, nr_muls > 0)
	if is_optional {
		typ = table.type_to_optional(typ)
	}
	if nr_muls > 0 {
		typ = table.type_set_nr_muls(typ, nr_muls)
	}
	return typ
}

pub fn (p mut Parser) parse_any_type(is_c, is_ptr bool) table.Type {
	mut name := p.tok.lit
	if is_c {
		name = 'C.$name'
	}
	// `module.Type`
	else if p.peek_tok.kind == .dot {
		// /if !(p.tok.lit in p.table.imports) {
		if !p.known_import(name) {
			println(p.table.imports)
			p.error('unknown module `$p.tok.lit`')
		}
		p.next()
		p.check(.dot)
		// prefix with full module
		name = '${p.imports[name]}.$p.tok.lit'
	}
	else if p.expr_mod != '' {
		name = p.expr_mod + '.' + name
	}
	// `Foo` in module `mod` means `mod.Foo`
	else if !(p.mod in ['builtin', 'main']) && !(name in table.builtin_type_names) {
		name = p.mod + '.' + name
	}
	// p.warn('get type $name')
	match p.tok.kind {
		// func
		.key_fn {
			return p.parse_fn_type()
		}
		// array
		.lsbr {
			return p.parse_array_type()
		}
		// multiple return
		.lpar {
			if is_ptr {
				p.error('parse_type: unexpected `&` before multiple returns')
			}
			return p.parse_multi_return_type()
		}
		else {
			// no defer
			if name == 'map' {
				return p.parse_map_type()
			}
			defer {
				p.next()
			}
			match name {
				'voidptr' {
					return table.voidptr_type
				}
				'byteptr' {
					return table.byteptr_type
				}
				'charptr' {
					return table.charptr_type
				}
				'i8' {
					return table.i8_type
				}
				'i16' {
					return table.i16_type
				}
				'int' {
					return table.int_type
				}
				'i64' {
					return table.i64_type
				}
				'byte' {
					return table.byte_type
				}
				'u16' {
					return table.u16_type
				}
				'u32' {
					return table.u32_type
				}
				'u64' {
					return table.u64_type
				}
				'f32' {
					return table.f32_type
				}
				'f64' {
					return table.f64_type
				}
				'string' {
					return table.string_type
				}
				'char' {
					return table.char_type
				}
				'bool' {
					return table.bool_type
				}
				// struct / enum / placeholder
				else {
					// struct / enum
					mut idx := p.table.find_type_idx(name)
					if idx > 0 {
						return table.new_type(idx)
					}
					// not found - add placeholder
					idx = p.table.add_placeholder_type(name)
					// println('NOT FOUND: $name - adding placeholder - $idx')
					return table.new_type(idx)
				}
			}
		}
	}
}

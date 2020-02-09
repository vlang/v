module parser
// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import (
	v.table
)

pub fn (p mut Parser) parse_array_ti(nr_muls int) TypeNew/*table.Type*/ {
	p.check(.lsbr)
	// fixed array
	if p.tok.kind == .number {
		size := p.tok.lit.int()
		elem_ti := p.parse_type()
		p.check(.rsbr)
		p.check_name()
		idx := p.table.find_or_register_array_fixed(elem_ti, size, 1)
		return table.new_type_ptr(idx, nr_muls)
	}
	// array
	p.check(.rsbr)
	elem_ti := p.parse_type()
	mut nr_dims := 1
	for p.tok.kind == .lsbr {
		p.check(.lsbr)
		p.check(.rsbr)
		nr_dims++
	}
	idx := p.table.find_or_register_array(elem_ti, nr_dims)
	return table.new_type_ptr(idx, nr_muls)
}

pub fn (p mut Parser) parse_map_type(nr_muls int) TypeNew/*table.Type*/ {
	p.next()
	if p.tok.kind != .lsbr {
		return table.new_type(table.map_type_idx)
	}
	p.check(.lsbr)
	key_type := p.parse_type()
	// if key_type.typ.kind != .string {
	if table.type_idx(key_type) != table.string_type_idx {
		p.error('maps can only have string keys for now')
	}
	p.check(.rsbr)
	value_type := p.parse_type()
	idx := p.table.find_or_register_map(key_type, value_type)
	return table.new_type_ptr(idx, nr_muls)
}

pub fn (p mut Parser) parse_multi_return_ti() TypeNew/*table.Type*/ {
	p.check(.lpar)
	mut mr_types := []TypeNew/*table.Type*/
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

pub fn (p mut Parser) parse_fn_type() TypeNew/*table.Type*/ {
	// p.check(.key_fn)
	p.fn_decl()
	return table.int_type
}

pub fn (p mut Parser) parse_type() TypeNew/*table.Type*/ {
	mut nr_muls := 0
	for p.tok.kind == .amp {
		p.check(.amp)
		nr_muls++
	}
	if p.tok.lit == 'C' {
		p.next()
		p.check(.dot)
	}
	if p.tok.kind == .question {
		p.next()
	}
	name := p.tok.lit
	match p.tok.kind {
		// func
		.key_fn {
			return p.parse_fn_type()
		}
		// array
		.lsbr {
			return p.parse_array_ti(nr_muls)
		}
		// multiple return
		.lpar {
			if nr_muls > 0 {
				p.error('parse_ti: unexpected `&` before multiple returns')
			}
			return p.parse_multi_return_ti()
		}
		else {
			// no defer
			if name == 'map' {
				return p.parse_map_type(nr_muls)
			}
			defer {
				p.next()
			}
			match name {
				'voidptr' {
					return table.new_type_ptr(table.voidptr_type_idx, nr_muls)
				}
				'byteptr' {
					return table.new_type_ptr(table.byteptr_type_idx, nr_muls)
				}
				'charptr' {
					return table.new_type_ptr(table.charptr_type_idx, nr_muls)
				}
				'i8' {
					return table.new_type_ptr(table.i8_type_idx, nr_muls)
				}
				'i16' {
					return table.new_type_ptr(table.i16_type_idx, nr_muls)
				}
				'int' {
					return table.new_type_ptr(table.int_type_idx, nr_muls)
				}
				'i64' {
					return table.new_type_ptr(table.i64_type_idx, nr_muls)
				}
				'byte' {
					return table.new_type_ptr(table.byte_type_idx, nr_muls)
				}
				'u16' {
					return table.new_type_ptr(table.u16_type_idx, nr_muls)
				}
				'u32' {
					return table.new_type_ptr(table.u32_type_idx, nr_muls)
				}
				'u64' {
					return table.new_type_ptr(table.u64_type_idx, nr_muls)
				}
				'f32' {
					return table.new_type_ptr(table.f32_type_idx, nr_muls)
				}
				'f64' {
					return table.new_type_ptr(table.f64_type_idx, nr_muls)
				}
				'string' {
					return table.new_type_ptr(table.string_type_idx, nr_muls)
				}
				'char' {
					return table.new_type_ptr(table.charptr_type_idx, nr_muls)
				}
				'bool' {
					return table.new_type_ptr(table.bool_type_idx, nr_muls)
				}
				// struct / enum / placeholder
				else {
					// struct / enum
					mut idx := p.table.find_type_idx(name)
					if idx > 0 {
						return table.new_type_ptr(idx, nr_muls)
					}
					// not found - add placeholder
					idx = p.table.add_placeholder_type(name)
					println('NOT FOUND: $name - adding placeholder - $idx')
					return table.new_type_ptr(idx, nr_muls)
				}
	}
		}
	}
}

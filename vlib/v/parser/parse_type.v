module parser
// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import (
	v.table
)

pub fn (p mut Parser) parse_array_ti(nr_muls int) table.TypeRef {
	p.check(.lsbr)
	// fixed array
	if p.tok.kind == .number {
		size := p.tok.lit.int()
		elem_ti := p.parse_type()
		p.check(.rsbr)
		p.check_name()
		idx := p.table.find_or_register_array_fixed(elem_ti, size, 1)
		return p.table.type_ref_ptr(idx, nr_muls)
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
	return p.table.type_ref_ptr(idx, nr_muls)
}

pub fn (p mut Parser) parse_map_type(nr_muls int) table.TypeRef {
	if p.tok.kind != .lsbr {
		// check notes in table/atypes.v near map_type_idx
		return p.table.type_ref(p.table.type_idxs['map'])
	}
	p.next()
	p.check(.lsbr)
	key_ti := p.parse_type()
	p.check(.rsbr)
	value_ti := p.parse_type()
	idx := p.table.find_or_register_map(key_ti, value_ti)
	return p.table.type_ref_ptr(idx, nr_muls)
}

pub fn (p mut Parser) parse_multi_return_ti() table.TypeRef {
	p.check(.lpar)
	mut mr_types := []table.TypeRef
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
	return p.table.type_ref(idx)
}

pub fn (p mut Parser) parse_variadic_ti() table.TypeRef {
	p.check(.ellipsis)
	variadic_type := p.parse_type()
	idx := p.table.find_or_register_variadic(variadic_type)
	return p.table.type_ref(idx)
}

pub fn (p mut Parser) parse_fn_type() table.TypeRef {
	// p.check(.key_fn)
	p.fn_decl()
	return p.table.type_ref(table.int_type_idx)
}

pub fn (p mut Parser) parse_type() table.TypeRef {
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
		// variadic
		.ellipsis {
			if nr_muls > 0 {
				p.error('parse_ti: unexpected `&` before variadic')
			}
			return p.parse_variadic_ti()
		}
		else {
			defer {
				p.next()
			}
			match name {
				'map' {
					return p.parse_map_type(nr_muls)
				}
				'voidptr' {
					return p.table.type_ref_ptr(table.voidptr_type_idx, nr_muls)
				}
				'byteptr' {
					return p.table.type_ref_ptr(table.byteptr_type_idx, nr_muls)
				}
				'charptr' {
					return p.table.type_ref_ptr(table.charptr_type_idx, nr_muls)
				}
				'i8' {
					return p.table.type_ref_ptr(table.i8_type_idx, nr_muls)
				}
				'i16' {
					return p.table.type_ref_ptr(table.i16_type_idx, nr_muls)
				}
				'int' {
					return p.table.type_ref_ptr(table.int_type_idx, nr_muls)
				}
				'i64' {
					return p.table.type_ref_ptr(table.i64_type_idx, nr_muls)
				}
				'byte' {
					return p.table.type_ref_ptr(table.byte_type_idx, nr_muls)
				}
				'u16' {
					return p.table.type_ref_ptr(table.u16_type_idx, nr_muls)
				}
				'u32' {
					return p.table.type_ref_ptr(table.u32_type_idx, nr_muls)
				}
				'u64' {
					return p.table.type_ref_ptr(table.u64_type_idx, nr_muls)
				}
				'f32' {
					return p.table.type_ref_ptr(table.f32_type_idx, nr_muls)
				}
				'f64' {
					return p.table.type_ref_ptr(table.f64_type_idx, nr_muls)
				}
				'string' {
					return p.table.type_ref_ptr(table.string_type_idx, nr_muls)
				}
				'char' {
					return p.table.type_ref_ptr(table.charptr_type_idx, nr_muls)
				}
				'bool' {
					return p.table.type_ref_ptr(table.bool_type_idx, nr_muls)
				}
				// struct / enum / placeholder
				else {
					// struct / enum
					mut idx := p.table.find_type_idx(name)
					if idx > 0 {
						return p.table.type_ref_ptr(idx, nr_muls)
					}
					// not found - add placeholder
					idx = p.table.add_placeholder_type(name)
					println('NOT FOUND: $name - adding placeholder - $idx')
					return p.table.type_ref_ptr(idx, nr_muls)
				}
	}
		}
	}
}

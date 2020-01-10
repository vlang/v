module parser
// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import v.types

pub fn (p mut Parser) parse_array_ti(nr_muls int) types.TypeIdent {
	p.check(.lsbr)
	// fixed array
	if p.tok.kind == .number {
		size := p.tok.lit.int()
		p.check(.rsbr)
		elem_ti := p.parse_ti()
		idx,name := p.table.find_or_register_array_fixed(&elem_ti, size, 1)
		return types.new_ti(.array_fixed, name, idx, nr_muls)
	}
	// array
	p.check(.rsbr)
	elem_ti := p.parse_ti()
	mut nr_dims := 1
	for p.tok.kind == .lsbr {
		p.check(.lsbr)
		p.check(.rsbr)
		nr_dims++
	}
	idx,name := p.table.find_or_register_array(&elem_ti, nr_dims)
	return types.new_ti(.array, name, idx, nr_muls)
}

pub fn (p mut Parser) parse_map_ti(nr_muls int) types.TypeIdent {
	p.next()
	p.check(.lsbr)
	key_ti := p.parse_ti()
	p.check(.rsbr)
	value_ti := p.parse_ti()
	idx,name := p.table.find_or_register_map(&key_ti, &value_ti)
	return types.new_ti(.map, name, idx, nr_muls)
}

pub fn (p mut Parser) parse_multi_return_ti() types.TypeIdent {
	p.check(.lpar)
	mut mr_tis := []types.TypeIdent
	for {
		mr_ti := p.parse_ti()
		mr_tis << mr_ti
		if p.tok.kind == .comma {
			p.check(.comma)
		}
		else {
			break
		}
	}
	p.check(.rpar)
	idx,name := p.table.find_or_register_multi_return(mr_tis)
	return types.new_ti(.multi_return, name, idx, 0)
}

pub fn (p mut Parser) parse_variadic_ti() types.TypeIdent {
	p.check(.ellipsis)
	variadic_ti := p.parse_ti()
	idx,name := p.table.find_or_register_variadic(&variadic_ti)
	return types.new_ti(.variadic, name, idx, 0)
}

pub fn (p mut Parser) parse_ti() types.TypeIdent {
	mut nr_muls := 0
	for p.tok.kind == .amp {
		p.check(.amp)
		nr_muls++
	}
	name := p.tok.lit
	match p.tok.kind {
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
				// map
				'map' {
					return p.parse_map_ti(nr_muls)
				}
				'voidptr' {
					return types.new_ti(.voidptr, 'voidptr', types.voidptr_type_idx, nr_muls)
				}
				'byteptr' {
					return types.new_ti(.byteptr, 'byteptr', types.byteptr_type_idx, nr_muls)
				}
				'charptr' {
					return types.new_ti(.charptr, 'charptr', types.charptr_type_idx, nr_muls)
				}
				'i8' {
					return types.new_ti(.i8, 'i8', types.i8_type_idx, nr_muls)
				}
				'i16' {
					return types.new_ti(.i16, 'i16', types.i16_type_idx, nr_muls)
				}
				'int' {
					return types.new_ti(.int, 'int', types.int_type_idx, nr_muls)
				}
				'i64' {
					return types.new_ti(.i64, 'i64', types.i64_type_idx, nr_muls)
				}
				'byte' {
					return types.new_ti(.byte, 'byte', types.byte_type_idx, nr_muls)
				}
				'u16' {
					return types.new_ti(.u16, 'u16', types.u16_type_idx, nr_muls)
				}
				'u32' {
					return types.new_ti(.u32, 'u32', types.u32_type_idx, nr_muls)
				}
				'u64' {
					return types.new_ti(.u64, 'u64', types.u64_type_idx, nr_muls)
				}
				'f32' {
					return types.new_ti(.f32, 'f32', types.f32_type_idx, nr_muls)
				}
				'f64' {
					return types.new_ti(.f64, 'f64', types.f64_type_idx, nr_muls)
				}
				'string' {
					return types.new_ti(.string, 'string', types.string_type_idx, nr_muls)
				}
				'char' {
					return types.new_ti(.char, 'char', types.charptr_type_idx, nr_muls)
				}
				'bool' {
					return types.new_ti(.bool, 'bool', types.bool_type_idx, nr_muls)
				}
				// struct / enum / placeholder
				else {
					// struct / enum
					mut idx := p.table.find_type_idx(name)
					// add placeholder
					if idx == 0 {
						idx = p.table.add_placeholder_type(name)
					}
					return types.new_ti(.placeholder, name, idx, nr_muls)
				}
	}
		}
	}
}

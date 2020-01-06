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
		return types.new_ti(._array_fixed, name, idx, nr_muls)
	}
	// array
	elem_ti := p.parse_ti()
	mut nr_dims := 1
	for p.tok.kind == .lsbr {
		p.check(.lsbr)
		p.next()
		nr_dims++
	}
	p.check(.rsbr)
	idx,name := p.table.find_or_register_array(&elem_ti, nr_dims)
	return types.new_ti(._array, name, idx, nr_muls)
}

pub fn (p mut Parser) parse_map_ti(nr_muls int) types.TypeIdent {
	p.next()
	p.check(.lsbr)
	key_ti := p.parse_ti()
	p.check(.rsbr)
	value_ti := p.parse_ti()
	idx,name := p.table.find_or_register_map(&key_ti, &value_ti)
	return types.new_ti(._map, name, idx, nr_muls)
}

pub fn (p mut Parser) parse_multi_return_ti() types.TypeIdent {
	p.check(.lpar)
	mut mr_tis := []&types.TypeIdent
	for {
		mr_ti := p.parse_ti()
		mr_tis << &mr_ti
		if p.tok.kind == .comma {
			p.check(.comma)
		}
		else {
			break
		}
	}
	p.check(.rpar)
	idx,name := p.table.find_or_register_multi_return(mr_tis)
	return types.new_ti(._multi_return, name, idx, 0)
}

pub fn (p mut Parser) parse_variadic_ti() types.TypeIdent {
	p.check(.ellipsis)
	variadic_ti := p.parse_ti()
	idx,name := p.table.find_or_register_variadic(&variadic_ti)
	return types.new_ti(._variadic, name, idx, 0)
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
					return types.new_base_ti(._voidptr, nr_muls)
				}
				'byteptr' {
					return types.new_base_ti(._byteptr, nr_muls)
				}
				'charptr' {
					return types.new_base_ti(._charptr, nr_muls)
				}
				'i8' {
					return types.new_base_ti(._i8, nr_muls)
				}
				'i16' {
					return types.new_base_ti(._i16, nr_muls)
				}
				'int' {
					return types.new_base_ti(._int, nr_muls)
				}
				'i64' {
					return types.new_base_ti(._i64, nr_muls)
				}
				'byte' {
					return types.new_base_ti(._byte, nr_muls)
				}
				'u16' {
					return types.new_base_ti(._u16, nr_muls)
				}
				'u32' {
					return types.new_base_ti(._u32, nr_muls)
				}
				'u64' {
					return types.new_base_ti(._u64, nr_muls)
				}
				'f32' {
					return types.new_base_ti(._f32, nr_muls)
				}
				'f64' {
					return types.new_base_ti(._f64, nr_muls)
				}
				'string' {
					return types.new_base_ti(._string, nr_muls)
				}
				'char' {
					return types.new_base_ti(._char, nr_muls)
				}
				'bool' {
					return types.new_base_ti(._bool, nr_muls)
				}
				// struct / enum / placeholder
				else {
					// struct / enum
					mut idx := p.table.find_type_idx(name)
					// add placeholder
					if idx == 0 {
						idx = p.table.add_placeholder_type(name)
					}
					return types.new_ti(._placeholder, name, idx, nr_muls)
				}
			}
		}
	}
}

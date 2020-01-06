module parser

import v.types

pub fn (p mut Parser) parse_array_ti(nr_muls int) types.TypeIdent {
	p.check(.lsbr)
	// fixed array
	if p.tok.kind == .number {
		fixed_size := p.tok.lit.int()
		p.check(.rsbr)
		elem_ti := p.parse_ti()
		array_fixed_type := types.ArrayFixed{
			name: 'array_fixed_$elem_ti.type_name'
			size: fixed_size
			elem_type_idx: elem_ti.type_idx
			elem_is_ptr: elem_ti.is_ptr()
		}
		idx := p.table.find_or_register_array_fixed(array_fixed_type)
		return types.new_ti(._array_fixed, array_fixed_type.name, idx, nr_muls)
	}
	p.check(.rsbr)
	// array
	elem_ti := p.parse_ti()
	array_type := types.Array{
		name: 'array_$elem_ti.type_name'
		elem_type_idx: elem_ti.type_idx
		elem_is_ptr: elem_ti.is_ptr()
	}
	idx := p.table.find_or_register_array(array_type)
	return types.new_ti(._array, array_type.name, idx, nr_muls)
}

pub fn (p mut Parser) parse_map_ti(nr_muls int) types.TypeIdent {
	p.next()
	p.check(.lsbr)
	key_ti := p.parse_ti()
	p.check(.rsbr)
	value_ti := p.parse_ti()
	map_type := types.Map{
		name: 'map_${key_ti.type_name}_${value_ti.type_name}'
		key_type_idx: key_ti.type_idx,
		value_type_idx: value_ti.type_idx
	}
	idx := p.table.find_or_register_map(map_type)
	return types.new_ti(._map, map_type.name, idx, nr_muls)
}

pub fn (p mut Parser) parse_multi_return_ti(nr_muls int) types.TypeIdent {
	p.check(.lpar)
	mut mr_type_kinds := []types.Kind
	mut mr_type_idxs := []int
	mut name := 'multi_return'
	for {
		mr_ti := p.parse_ti()
		mr_type_kinds << mr_ti.type_kind
		mr_type_idxs << mr_ti.type_idx
		name += '_$mr_ti.type_name'
		if p.tok.kind == .comma {
			p.check(.comma)
		} else {
			break
		}
	}
	p.check(.rpar)
	mr_type := types.MultiReturn{
		name: name
		type_kinds: mr_type_kinds
		type_idxs: mr_type_idxs
	}
	idx := p.table.find_or_register_multi_return(mr_type)
	return types.new_ti(._multi_return, name, idx, nr_muls)
}

pub fn (p mut Parser) parse_variadic_ti(nr_muls int) types.TypeIdent {
	p.check(.ellipsis)
	variadic_ti := p.parse_ti()
	return types.new_ti(
		._variadic, 'variadic_$variadic_ti.type_name',
		variadic_ti.type_idx, nr_muls)
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
		// multile return
		.lpar {
			return p.parse_multi_return_ti(nr_muls)
		}
		// variadic
		.ellipsis {
			return p.parse_variadic_ti(nr_muls)
		}
		else {
			defer { p.next() }
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

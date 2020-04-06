// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Type layout information (32 bits)
// flag (8 bits) | nr_muls (8 bits) | idx (16 bits)
// pack: (int(flag)<<24) | (nr_muls<<16) | u16(idx)
// unpack:
//     flag: (int(type)>>24) & 0xff
//  nr_muls: (int(type)>>16) & 0xff
//      idx:  u16(type) & 0xffff

module table

import (
	strings
)

pub type Type int

pub type TypeInfo = Array | ArrayFixed | Map | Struct |
MultiReturn | Alias | Enum | SumType | FnType

pub struct TypeSymbol {
pub:
	parent_idx int
mut:
	info       TypeInfo
	kind       Kind
	name       string
	methods    []Fn
}

pub enum TypeFlag {
	unset
	optional
	variadic
}

pub fn (types []Type) contains(typ Type) bool {
	for t in types {
		if int(typ) == int(t) {
			return true
		}
	}
	return false
}

// return TypeSymbol idx for `t`
[inline]
pub fn type_idx(t Type) int {
	return u16(t) & 0xffff
}

// return nr_muls for `t`
[inline]
pub fn type_nr_muls(t Type) int {
	return (int(t)>>16) & 0xff
}

// return true if `t` is a pointer (nr_muls>0)
[inline]
pub fn type_is_ptr(t Type) bool {
	return (int(t)>>16) & 0xff > 0
}
// set nr_muls on `t` and return it
[inline]
pub fn type_set_nr_muls(t Type, nr_muls int) Type {
	if nr_muls < 0 || nr_muls > 255 {
		panic('typ_set_nr_muls: nr_muls must be between 0 & 255')
	}
	return (((int(t)>>24) & 0xff)<<24) | (nr_muls<<16) | (u16(t) & 0xffff)
}

// increments nr_nuls on `t` and return it
[inline]
pub fn type_to_ptr(t Type) Type {
	nr_muls := (int(t)>>16) & 0xff
	if nr_muls == 255 {
		panic('type_to_pre: nr_muls is already at max of 255')
	}
	return (((int(t)>>24) & 0xff)<<24) | ((nr_muls + 1)<<16) | (u16(t) & 0xffff)
}

// decrement nr_muls on `t` and return it
[inline]
pub fn type_deref(t Type) Type {
	nr_muls := (int(t)>>16) & 0xff
	if nr_muls == 0 {
		panic('deref: type `$t` is not a pointer')
	}
	return (((int(t)>>24) & 0xff)<<24) | ((nr_muls - 1)<<16) | (u16(t) & 0xffff)
}

// return the flag that is set on `t`
[inline]
pub fn type_flag(t Type) TypeFlag {
	return (int(t)>>24) & 0xff
}

// set the flag on `t` to `flag` and return it
[inline]
pub fn type_set(t Type, flag TypeFlag) Type {
	return (int(flag)<<24) | (((int(t)>>16) & 0xff)<<16) | (u16(t) & 0xffff)
}

// return true if the flag set on `t` is `flag`
[inline]
pub fn type_is(t Type, flag TypeFlag) bool {
	return (int(t)>>24) & 0xff == flag
}

// return new type with TypeSymbol idx set to `idx`
[inline]
pub fn new_type(idx int) Type {
	if idx < 1 || idx > 65536 {
		panic('new_type_id: idx must be between 1 and 65536')
	}
	return idx
}

// return new type with TypeSymbol idx set to `idx` & nr_muls set to `nr_muls`
[inline]
pub fn new_type_ptr(idx int, nr_muls int) Type {
	if idx < 1 || idx > 65536 {
		panic('new_type_ptr: idx must be between 1 and 65536')
	}
	if nr_muls < 0 || nr_muls > 255 {
		panic('new_type_ptr: nr_muls must be between 0 & 255')
	}
	return (nr_muls<<16) | u16(idx)
}

pub fn is_number(typ Type) bool {
	return type_idx(typ) in number_type_idxs
}

pub const (
	// primitive types
	void_type_idx = 1
	voidptr_type_idx = 2
	byteptr_type_idx = 3
	charptr_type_idx = 4
	i8_type_idx = 5
	i16_type_idx = 6
	int_type_idx = 7
	i64_type_idx = 8
	byte_type_idx = 9
	u16_type_idx = 10
	u32_type_idx = 11
	u64_type_idx = 12
	f32_type_idx = 13
	f64_type_idx = 14
	char_type_idx = 15
	bool_type_idx = 16
	none_type_idx = 17
	// advanced / defined from v structs
	string_type_idx = 18
	array_type_idx = 19
	map_type_idx = 20
)

pub const (
	number_type_idxs = [i8_type_idx, i16_type_idx, int_type_idx, i64_type_idx, byte_type_idx, u16_type_idx, u32_type_idx, u64_type_idx, f32_type_idx, f64_type_idx]
	pointer_type_idxs = [voidptr_type_idx, byteptr_type_idx, charptr_type_idx]
)

pub const (
	void_type = new_type(void_type_idx)
	voidptr_type = new_type(voidptr_type_idx)
	byteptr_type = new_type(byteptr_type_idx)
	charptr_type = new_type(charptr_type_idx)
	i8_type = new_type(i8_type_idx)
	int_type = new_type(int_type_idx)
	i16_type = new_type(i16_type_idx)
	i64_type = new_type(i64_type_idx)
	byte_type = new_type(byte_type_idx)
	u16_type = new_type(u16_type_idx)
	u32_type = new_type(u32_type_idx)
	u64_type = new_type(u64_type_idx)
	f32_type = new_type(f32_type_idx)
	f64_type = new_type(f64_type_idx)
	char_type = new_type(char_type_idx)
	bool_type = new_type(bool_type_idx)
	none_type = new_type(none_type_idx)
	string_type = new_type(string_type_idx)
	array_type = new_type(array_type_idx)
	map_type = new_type(map_type_idx)
)

pub const (
	builtin_type_names = ['void', 'voidptr', 'charptr', 'byteptr', 'i8', 'i16', 'int', 'i64', 'u16', 'u32', 'u64',
	'f32', 'f64', 'string', 'char', 'byte', 'bool', 'none', 'array', 'array_fixed', 'map', 'struct',
	'mapnode', 'ustring', 'size_t']
)

pub struct MultiReturn {
pub:
	name  string
mut:
	types []Type
}

pub struct FnType {
pub:
	is_anon  bool
	has_decl bool
	func     Fn
}

pub enum Kind {
	placeholder
	void
	voidptr
	byteptr
	charptr
	i8
	i16
	int
	i64
	byte
	u16
	u32
	u64
	f32
	f64
	char
	bool
	none_
	string
	array
	array_fixed
	map
	struct_
	multi_return
	sum_type
	alias
	enum_
	function
}

pub fn (t &TypeSymbol) str() string {
	return t.name.replace('array_', '[]')
}

[inline]
pub fn (t &TypeSymbol) enum_info() Enum {
	match t.info {
		Enum {
			return it
		}
		else {
			panic('TypeSymbol.enum_info(): no enum info for type: $t.name')
		}
	}
}

[inline]
pub fn (t &TypeSymbol) mr_info() MultiReturn {
	match t.info {
		MultiReturn {
			return it
		}
		else {
			panic('TypeSymbol.mr_info(): no multi return info for type: $t.name')
		}
	}
}

[inline]
pub fn (t &TypeSymbol) array_info() Array {
	match t.info {
		Array {
			return it
		}
		else {
			panic('TypeSymbol.array_info(): no array info for type: $t.name')
		}
	}
}

[inline]
pub fn (t &TypeSymbol) array_fixed_info() ArrayFixed {
	match t.info {
		ArrayFixed {
			return it
		}
		else {
			panic('TypeSymbol.array_fixed(): no array fixed info for type: $t.name')
		}
	}
}

[inline]
pub fn (t &TypeSymbol) map_info() Map {
	match t.info {
		Map {
			return it
		}
		else {
			panic('TypeSymbol.map_info(): no map info for type: $t.name')
		}
	}
}

/*
pub fn (t TypeSymbol) str() string {
	return t.name
}
*/


pub fn (t mut Table) register_builtin_type_symbols() {
	// reserve index 0 so nothing can go there
	// save index check, 0 will mean not found
	t.register_type_symbol(TypeSymbol{
		kind: .placeholder
		name: 'reserved_0'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .void
		name: 'void'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .voidptr
		name: 'voidptr'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .byteptr
		name: 'byteptr'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .charptr
		name: 'charptr'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .i8
		name: 'i8'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .i16
		name: 'i16'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .int
		name: 'int'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .i64
		name: 'i64'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .byte
		name: 'byte'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .u16
		name: 'u16'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .u32
		name: 'u32'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .u64
		name: 'u64'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .f32
		name: 'f32'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .f64
		name: 'f64'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .char
		name: 'char'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .bool
		name: 'bool'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .none_
		name: 'none'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .string
		name: 'string'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .array
		name: 'array'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .map
		name: 'map'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .placeholder
		name: 'size_t'
	})
	// TODO: remove. for v1 map compatibility
	map_string_string_idx := t.find_or_register_map(string_type, string_type)
	map_string_int_idx := t.find_or_register_map(string_type, int_type)
	t.register_type_symbol(TypeSymbol{
		kind: .alias
		name: 'map_string'
		parent_idx: map_string_string_idx
	})
	t.register_type_symbol(TypeSymbol{
		kind: .alias
		name: 'map_int'
		parent_idx: map_string_int_idx
	})
}

[inline]
pub fn (t &TypeSymbol) is_pointer() bool {
	return t.kind in [.byteptr, .charptr, .voidptr]
}

[inline]
pub fn (t &TypeSymbol) is_int() bool {
	return t.kind in [.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64]
}

[inline]
pub fn (t &TypeSymbol) is_float() bool {
	return t.kind in [.f32, .f64]
}

[inline]
pub fn (t &TypeSymbol) is_number() bool {
	return t.is_int() || t.is_float()
}

pub fn (k Kind) str() string {
	k_str := match k {
		.placeholder{
			'placeholder'
		}
		.void{
			'void'
		}
		.voidptr{
			'voidptr'
		}
		.charptr{
			'charptr'
		}
		.byteptr{
			'byteptr'
		}
		.struct_{
			'struct'
		}
		.int{
			'int'
		}
		.i8{
			'i8'
		}
		.i16{
			'i16'
		}
		.i64{
			'i64'
		}
		.byte{
			'byte'
		}
		.u16{
			'u16'
		}
		.u32{
			'u32'
		}
		.u64{
			'u64'
		}
		.f32{
			'f32'
		}
		.f64{
			'f64'
		}
		.string{
			'string'
		}
		.char{
			'char'
		}
		.bool{
			'bool'
		}
		.none_{
			'none'
		}
		.array{
			'array'
		}
		.array_fixed{
			'array_fixed'
		}
		.map{
			'map'
		}
		.multi_return{
			'multi_return'
		}
		.sum_type{
			'sum_type'
		}
		.alias{
			'alias'
		}
		.enum_{
			'enum'
		}
		else {
			'unknown'}
	}
	return k_str
}

pub fn (kinds []Kind) str() string {
	mut kinds_str := ''
	for i, k in kinds {
		kinds_str += k.str()
		if i < kinds.len - 1 {
			kinds_str += '_'
		}
	}
	return kinds_str
}

pub struct Struct {
pub mut:
	fields []Field
	is_typedef bool // C. [typedef]
}

pub struct Enum {
pub mut:
	vals []string
}

pub struct Alias {
pub:
	foo string
}

pub struct Field {
pub:
	name string
mut:
	typ  Type
	default_val string
}

pub struct Array {
pub:
	nr_dims   int
mut:
	elem_type Type
}

pub struct ArrayFixed {
pub:
	nr_dims   int
	size      int
mut:
	elem_type Type
}

pub struct Map {
pub mut:
	key_type   Type
	value_type Type
}

pub struct SumType {
pub:
	variants []Type
}

pub fn (table &Table) type_to_str(t Type) string {
	sym := table.get_type_symbol(t)
	if sym.kind == .multi_return {
		mut res := '('
		mr_info := sym.info as MultiReturn
		for i, typ in mr_info.types {
			res += table.type_to_str(typ)
			if i < mr_info.types.len - 1 {
				res += ', '
			}
		}
		res += ')'
		return res
	}
	mut res := sym.name
	if sym.kind == .array {
		res = res.replace('array_', '[]')
	}
	else if sym.kind == .map {
		res = res.replace('map_string_', 'map[string]')
	}
	// mod.submod.submod2.Type => submod2.Type
	if res.contains('.') {
		vals := res.split('.')
		if vals.len > 2 {
			res = vals[vals.len - 2] + '.' + vals[vals.len - 1]
		}
		if sym.kind == .array {
			res = '[]' + res
		}
	}
	if type_is(t, .optional) {
		res = '?' + res
	}
	nr_muls := type_nr_muls(t)
	if nr_muls > 0 {
		res = strings.repeat(`&`, nr_muls) + res
	}
	/*
	if res.starts_with(cur_mod +'.') {
	res = res[cur_mod.len+1.. ]
	}
	*/

	return res
}

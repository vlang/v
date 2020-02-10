// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

pub type TypeInfo = Array | ArrayFixed | Map | Struct | MultiReturn

pub struct TypeSymbol {
pub:
	parent     &TypeSymbol
mut:
	info       TypeInfo
	kind       Kind
	name       string
	methods    []Fn
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
	// advanced / defined from v structs
	string_type_idx = 17
	array_type_idx = 18
	map_type_idx = 19
)

pub const (
	builtin_type_names = [
		'void', 'voidptr', 'charptr', 'byteptr', 'i8', 'i16', 'int', 'i64', 'u16', 'u32', 'u64',
		'f32' ,'f64', 'string', 'char', 'byte' ,'bool', 'struct', 'array', 'array_fixed', 'map'
	]
)

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
	string
	struct_
	array
	array_fixed
	map
	multi_return
	unresolved
}

[inline]
pub fn(t &TypeSymbol) mr_info() MultiReturn {
	match t.info {
		MultiReturn {
			return it
		}
		else {
			panic('TypeSymbol.mr_info(): no multi return info')
		}
	}
}

[inline]
pub fn(t &TypeSymbol) array_info() Array {
	match t.info {
		Array {
			return it
		}
		else {
			panic('TypeSymbol.array_info(): no array info')
		}
	}
}

[inline]
pub fn(t &TypeSymbol) array_fixed_info() ArrayFixed {
	match t.info {
		ArrayFixed {
			return it
		}
		else {
			panic('TypeSymbol.array_fixed(): no array fixed info')
		}
	}
}

[inline]
pub fn(t &TypeSymbol) map_info() Map {
	match t.info {
		Map {
			return it
		}
		else {
			panic('TypeSymbol.map_info(): no map info')
		}
	}
}

/*
pub fn (t TypeSymbol) str() string {
	return t.name
}
*/

[inline]
pub fn array_name(elem_type &TypeSymbol, nr_dims int) string {
	return 'array_${elem_type.name}' + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
}

[inline]
pub fn array_fixed_name(elem_type &TypeSymbol, size int, nr_dims int) string {
	return 'array_fixed_${elem_type.name}_${size}' + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
}

[inline]
pub fn map_name(key_type &TypeSymbol, value_type &TypeSymbol) string {
	return 'map_${key_type.name}_${value_type.name}'
}

pub fn (t mut Table) register_builtin_type_symbols() {
	// reserve index 0 so nothing can go there
	// save index check, 0 will mean not found
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .placeholder
		name: 'reserved_0'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .void
		name: 'void'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .voidptr
		name: 'voidptr'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .byteptr
		name: 'byteptr'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .charptr
		name: 'charptr'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .i8
		name: 'i8'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .i16
		name: 'i16'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .int
		name: 'int'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .i64
		name: 'i64'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .byte
		name: 'byte'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .u16
		name: 'u16'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .u32
		name: 'u32'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .u64
		name: 'u64'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .f32
		name: 'f32'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .f64
		name: 'f64'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .char
		name: 'char'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .bool
		name: 'bool'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .string
		name: 'string'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .array
		name: 'array'
	})
	t.register_type_symbol(TypeSymbol{
		parent: 0
		kind: .map
		name: 'map'
	})
	// TODO: remove
	t.register_type_symbol(TypeSymbol{
		parent: &t.types[map_type_idx]
		kind: .struct_
		name: 'map_string'
	})
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
		.unresolved{
			'unresolved'
		}
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
}

pub struct Field {
pub:
	name string
mut:
	typ  Type
}

pub struct Array {
pub:
	nr_dims    int
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

pub struct MultiReturn {
pub:
	name  string
mut:
	types []Type
}

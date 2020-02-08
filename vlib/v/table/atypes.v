// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

pub type TypeInfo = Array | ArrayFixed | Map | Struct | MultiReturn

pub struct Type {
pub:
	parent     &Type
mut:
	info       TypeInfo
	kind       Kind
	name       string
	methods    []Fn
}

pub struct TypeRef {
pub:
	idx     int
	typ     &Type
	nr_muls int
}

pub const (
	// primitive types
	void_type_idx = 1
	voidptr_type_idx = 2
	charptr_type_idx = 3
	byteptr_type_idx = 4
	i8_type_idx = 5
	i16_type_idx = 6
	int_type_idx = 7
	i64_type_idx = 8
	u16_type_idx = 9
	u32_type_idx = 10
	u64_type_idx = 11
	f32_type_idx = 12
	f64_type_idx = 13
	bool_type_idx = 14
	// advanced / defined from v structs
	string_type_idx = 15
	char_type_idx = 16
	byte_type_idx = 17
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
	charptr
	byteptr
	i8
	i16
	int
	i64
	u16
	u32
	u64
	f32
	f64
	string
	char
	byte
	bool
	//const_
	//enum_
	struct_
	array
	array_fixed
	map
	multi_return
	unresolved
}

[inline]
pub fn(t &Type) mr_info() MultiReturn {
	match t.info {
		MultiReturn {
			return it
		}
		else {
			panic('Type.mr_info(): no multi return info')
		}
	}
}

[inline]
pub fn(t &Type) array_info() Array {
	match t.info {
		Array {
			return it
		}
		else {
			panic('Type.array_info(): no array info')
		}
	}
}

[inline]
pub fn(t &Type) array_fixed_info() ArrayFixed {
	match t.info {
		ArrayFixed {
			return it
		}
		else {
			panic('Type.array_fixed(): no array fixed info')
		}
	}
}

[inline]
pub fn(t &Type) map_info() Map {
	match t.info {
		Map {
			return it
		}
		else {
			panic('Type.map_info(): no map info')
		}
	}
}

/*
pub fn (t Type) str() string {
	return t.name
}
*/

pub fn (t &TypeRef) str() string {
	mut muls := ''
	for _ in 0 .. t.nr_muls {
		muls += '&'
	}
	return '$muls$t.typ.name'
}

[inline]
pub fn (t &Table) type_ref(idx int) TypeRef {
	return TypeRef{
		idx: idx
		typ: &t.types[idx]
	}
}

[inline]
pub fn (t &Table) type_ref_ptr(idx int, nr_muls int) TypeRef {
	return TypeRef{
		idx: idx
		nr_muls: nr_muls
		typ: &t.types[idx]
	}
}

[inline]
pub fn array_name(elem_type &TypeRef, nr_dims int) string {
	return 'array_${elem_type.typ.name}' + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
}

[inline]
pub fn array_fixed_name(elem_type &TypeRef, size int, nr_dims int) string {
	return 'array_fixed_${elem_type.typ.name}_${size}' + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
}

[inline]
pub fn map_name(key_type &TypeRef, value_type &TypeRef) string {
	return 'map_${key_type.typ.name}_${value_type.typ.name}'
}

pub fn (t mut Table) register_builtin_types() {
	// reserve index 0 so nothing can go there
	// save index check, 0 will mean not found
	t.register_type(Type{
		parent: 0
		kind: .placeholder
		name: 'reserved_0'
	})
	t.register_type(Type{
		parent: 0
		kind: .void
		name: 'void'
	})
	t.register_type(Type{
		parent: 0
		kind: .voidptr
		name: 'voidptr'
	})
	t.register_type(Type{
		parent: 0
		kind: .charptr
		name: 'charptr'
	})
	t.register_type(Type{
		parent: 0
		kind: .byteptr
		name: 'byteptr'
	})
	t.register_type(Type{
		parent: 0
		kind: .i8
		name: 'i8'
	})
	t.register_type(Type{
		parent: 0
		kind: .i16
		name: 'i16'
	})
	t.register_type(Type{
		parent: 0
		kind: .int
		name: 'int'
	})
	t.register_type(Type{
		parent: 0
		kind: .i64
		name: 'i64'
	})
	t.register_type(Type{
		parent: 0
		kind: .u16
		name: 'u16'
	})
	t.register_type(Type{
		parent: 0
		kind: .u32
		name: 'u32'
	})
	t.register_type(Type{
		parent: 0
		kind: .u64
		name: 'u64'
	})
	t.register_type(Type{
		parent: 0
		kind: .f32
		name: 'f32'
	})
	t.register_type(Type{
		parent: 0
		kind: .f64
		name: 'f64'
	})
	t.register_type(Type{
		parent: 0
		kind: .bool
		name: 'bool'
	})
	t.register_type(Type{
		parent: 0
		kind: .string
		name: 'string'
	})
	t.register_type(Type{
		parent: 0
		kind: .char
		name: 'char'
	})
	t.register_type(Type{
		parent: 0
		kind: .byte
		name: 'byte'
	})
	t.register_type(Type{
		parent: 0
		kind: .array
		name: 'array'
	})
	t.register_type(Type{
		parent: 0
		kind: .map
		name: 'map'
	})
}

[inline]
pub fn (t &TypeRef) is_ptr() bool {
	return t.nr_muls > 0
}

[inline]
pub fn (t &Type) is_int() bool {
	return t.kind in [.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64]
}

[inline]
pub fn (t &Type) is_float() bool {
	return t.kind in [.f32, .f64]
}

[inline]
pub fn (t &Type) is_number() bool {
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
		// .const_{
		// 'const'
		// }
		// .enum_{
		// 'enum'
		// }
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

// pub struct Const {
// pub:
// name string
// }
// pub struct Enum {
// pub:
// name string
// }
pub struct Struct {
pub mut:
	fields []Field
}

pub struct Field {
pub:
	name string
mut:
	typ  TypeRef
	// type_idx int
}
// pub struct Int {
// pub:
// bit_size    u32
// is_unsigned bool
// }
// pub struct Float {
// pub:
// bit_size u32
// }
pub struct Array {
pub:
	nr_dims    int
mut:
	elem_type TypeRef
}

pub struct ArrayFixed {
pub:
	nr_dims   int
	size      int
mut:
	elem_type TypeRef
}

pub struct Map {
pub mut:
	key_type   TypeRef
	value_type TypeRef
}

pub struct MultiReturn {
pub:
	name  string
mut:
	types []TypeRef
}

[inline]
pub fn (t &Table) get_type(idx int) &Type {
	if idx == 0 {
		panic('get_type: idx 0')
	}
	return &t.types[idx]
}

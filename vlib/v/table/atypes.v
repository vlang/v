// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

pub type TypeInfo = Array | ArrayFixed | Map | Struct | MultiReturn | Variadic

pub struct Type {
pub:
	// idx        int
	// parent_idx int
	parent     &Type
mut:
	info       TypeInfo
	kind       Kind
	name       string
	methods    []Fn
	nr_muls    int
}

pub struct TypeRef {
pub:
	idx     int
	typ     &Type
	nr_muls int
}

pub const (
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
	string_type_idx = 14
	char_type_idx = 15
	byte_type_idx = 16
	bool_type_idx = 17
	// map_type_idx = 18
	unresolved_type_idx = 18
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
	const_
	enum_
	struct_
	array
	array_fixed
	map
	multi_return
	variadic
	unresolved
}

/*
pub fn (t Type) str() string {
	return t.name
}
*/

// pub fn (t &Type) str() string {
// 	mut muls := ''
// 	for _ in 0 .. t.nr_muls {
// 		muls += '&'
// 	}
// 	// return '$muls$ti.name'
// 	return '$muls$t.idx'
// }

pub fn (t &TypeRef) str() string {
	mut muls := ''
	for _ in 0 .. t.nr_muls {
		muls += '&'
	}
	return '$muls$t.typ.name'
}

/*
pub fn new_type(kind Kind, name string, idx int, nr_muls int) Type {
	return Type{
		// idx: idx
		kind: kind
		name: name
		nr_muls: nr_muls
	}
}
*/

[inline]
pub fn (t &Table) new_type_ref(idx int, nr_muls int) TypeRef {
	return TypeRef{
		idx: idx
		nr_muls: nr_muls
		typ: &t.types[idx]
	}
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
		kind: .bool
		name: 'bool'
	})
	t.register_type(Type{
		parent: 0
		kind: .unresolved
		name: 'unresolved'
	})
}

[inline]
pub fn (ti &Type) is_ptr() bool {
	return ti.nr_muls > 0
}

[inline]
pub fn (ti &Type) is_int() bool {
	return ti.kind in [.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64]
}

[inline]
pub fn (ti &Type) is_float() bool {
	return ti.kind in [.f32, .f64]
}

[inline]
pub fn (ti &Type) is_number() bool {
	return ti.is_int() || ti.is_float()
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
		.variadic{
			'variadic'
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
pub:
	key_type   TypeRef
	value_type TypeRef
}

pub struct MultiReturn {
pub:
	name  string
	types []TypeRef
}

pub struct Variadic {
pub:
	typ TypeRef
}

[inline]
pub fn (t &Table) get_type(idx int) &Type {
	if idx == 0 {
		panic('get_type: idx 0')
	}
	return &t.types[idx]
}

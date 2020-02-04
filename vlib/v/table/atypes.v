// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

pub type TypeInfo = Array | ArrayFixed | Map | Struct | MultiReturn | Variadic

pub struct Type {
pub:
	idx        int
	parent_idx int
mut:
	info       TypeInfo
	kind       Kind
	name       string
	methods    []Fn
	nr_muls    int
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
	map_type_idx = 18
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

pub const (
	unresolved_type = Type{
		kind: .unresolved
		name: 'unresolved'
	}
	void_type = Type{
		kind: .void
		name: 'void'
		idx: void_type_idx
	}
	int_type = Type{
		kind: .int
		name: 'int'
		idx: int_type_idx
	}
	string_type = Type{
		kind: .string
		name: 'string'
		idx: string_type_idx
	}
	bool_type = Type{
		kind: .bool
		name: 'bool'
		idx: bool_type_idx
	}
	byte_type = Type{
		kind: .byte
		name: 'byte'
		idx: byte_type_idx
	}
	map_type = Type{
		kind: .map
		name: 'map'
		idx: map_type_idx
	}
)
/*
pub fn (t Type) str() string {
	return t.name
}
*/


pub fn (t &Type) str() string {
	mut muls := ''
	for _ in 0 .. t.nr_muls {
		muls += '&'
	}
	// return '$muls$ti.name'
	return '$muls$t.idx'
}

pub fn new_type(kind Kind, name string, idx int, nr_muls int) Type {
	return Type{
		idx: idx
		kind: kind
		name: name
		nr_muls: nr_muls
	}
}

pub fn (t mut Table) register_builtin_types() {
	// reserve index 0 so nothing can go there
	// save index check, 0 will mean not found
	t.register_type(Type{
		kind: .placeholder
		name: 'reserved_0'
	})
	t.register_type(Type{
		kind: .void
		name: 'void'
	})
	t.register_type(Type{
		kind: .voidptr
		name: 'voidptr'
	})
	t.register_type(Type{
		kind: .charptr
		name: 'charptr'
	})
	t.register_type(Type{
		kind: .byteptr
		name: 'byteptr'
	})
	t.register_type(Type{
		kind: .i8
		name: 'i8'
	})
	t.register_type(Type{
		kind: .i16
		name: 'i16'
	})
	t.register_type(Type{
		kind: .int
		name: 'int'
	})
	t.register_type(Type{
		kind: .i64
		name: 'i64'
	})
	t.register_type(Type{
		kind: .u16
		name: 'u16'
	})
	t.register_type(Type{
		kind: .u32
		name: 'u32'
	})
	t.register_type(Type{
		kind: .u64
		name: 'u64'
	})
	t.register_type(Type{
		kind: .f32
		name: 'f32'
	})
	t.register_type(Type{
		kind: .f64
		name: 'f64'
	})
	t.register_type(Type{
		kind: .string
		name: 'string'
	})
	t.register_type(Type{
		kind: .char
		name: 'char'
	})
	t.register_type(Type{
		kind: .byte
		name: 'byte'
	})
	t.register_type(Type{
		kind: .bool
		name: 'bool'
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
			'u18'
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
	ti   Type
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
	elem_type_kind Kind
	elem_type_idx  int
	elem_is_ptr    bool
	nr_dims        int
}

pub struct ArrayFixed {
pub:
	elem_type_kind Kind
	elem_type_idx  int
	elem_is_ptr    bool
	nr_dims        int
	size           int
}

pub struct Map {
pub:
	key_type_kind   Kind
	key_type_idx    int
	value_type_kind Kind
	value_type_idx  int
}

pub struct MultiReturn {
pub:
	name string
	tis  []Type
}

pub struct Variadic {
pub:
	ti Type
}

pub fn (t &Table) refresh_ti(ti Type) Type {
	if ti.idx == 0 {
		return ti
	}
	if ti.kind in [.placeholder, .unresolved] {
		typ := t.types[ti.idx]
		return {
			ti |
			kind:typ.kind,
			name:typ.name
		}
	}
	return ti
}

pub fn (t &Table) get_type(idx int) Type {
	if idx == 0 {
		panic('get_type: idx 0')
	}
	return t.types[idx]
}

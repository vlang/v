// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

import(
	v.table
)

// pub struct Type {
// pub:
// 	name string
// 	idx  int
// }

pub fn check(got, expected &Type) bool {
	// if got.idx != expected.idx {
	// 	return false
	// }
	return true
}

// pub fn check(got, expected &TypeIdent) bool {
// 	if got.type_idx != expected.type_idx {
// 		return false
// 	}
// 	return true
// }


pub struct TypeIdent {
pub:
	type_idx  int
	type_kind Kind
	type_name string
	nr_muls   int
}

// pub fn new_ti(kind Kind, name string, idx int, nr_muls int) TypeIdent {
pub fn new_ti(kind Kind, idx int, nr_muls int) TypeIdent {
    return TypeIdent{
		type_idx: idx
		type_kind: kind
		nr_muls: nr_muls
	}
}

[inline]
pub fn (ti &TypeIdent) is_ptr() bool {
    return ti.nr_muls > 0
}

pub fn (ti &TypeIdent) get_struct(table &table.Table) Struct {
	t1 := table.types[ti.idx] as Struct else {
		panic('ti.get_struct: error casting type')
	}
	return t1
}

// pub fn (t TypeIdent) get_type(name string) &Type {
// 	match t.kind {
// 		._voidptr {
// 			types.voidptr_type
// 		}
// 		._byteptr {
// 			types.byteptr_type
// 		}
// 		._charptr {
// 			types.charptr_type
// 		}
// 		._int {
// 			types.int_type
// 		}
// 		._i64 {
// 			types.i64_type
// 		}
// 		._f32 {
// 			types.f32_type
// 		}
// 		._f64 {
// 			types.f64_type
// 		}
// 		._string {
// 			types.string_type
// 		}
// 		else {
// 			typ := p.table.types[p.tok.lit]
// 			if isnil(typ.name.str) || typ.name == '' {
// 				p.error('undefined type `$p.tok.lit`')
// 			}
// 			println('RET Typ $typ.name')
// 			typ
// 		}
// }


pub enum Kind {
	_placeholder,
	_void,
	_voidptr,
	_charptr,
	_byteptr,
	_const,
	_enum,
	_struct,
	_int,
	_i8,
	_ii6,
	_i64,
	_u16,
	_f32,
	_f64,
	_string,
	_char,
	_byte,
	_bool,
	_array,
	_fixed_array,
	_map,
	_multiReturn,
	_variadic
}

pub type Type = Placeholder | Void | Voidptr | Charptr | Byteptr | Const | Enum | Struct |
	Int | Float | String | Char | Byte | Bool | Array | Map | MultiReturn | Variadic


pub struct Placeholder {
	idx  int
	name string
	kind Kind
}

pub struct Void {}

pub struct Voidptr {}

pub struct Charptr {}

pub struct Byteptr {}

pub struct Const {
	// idx  int
	name string
}

pub struct Enum {
	// idx  int
	name string
}

pub struct Struct {
	idx  int
	name string
}

pub struct Int {
	bit_size    u32
	is_unsigned bool
}

pub struct Float {
	bit_size u32
}

pub struct String {}

pub struct Char {}

pub struct Byte {}

pub struct Bool {}

pub struct Array {
	idx int
	elem_type_kind Kind
	elem_type_idx  int
	nr_dims   int
}

pub struct FixedArray {
	elem_type_kind Kind
	elem_type_idx  int
	size           int
}

pub struct Map {
	idx int
	key_type_kind   Kind
	key_type_idx    int 
	// key_type_name   string
	value_type_kind Kind
	value_type_idx  int
	// value_type_name   string 
}

pub struct MultiReturn {
	elem_type_kinds []Kind
	elem_type_idxs  []int
}

pub struct Variadic {
	elem_type_kind Kind
	elem_type_idx  int
}

pub fn (t Void) str() string { return 'void' }
pub fn (t Voidptr) str() string { return 'voidptr' }
pub fn (t Charptr) str() string { return 'charptr' }
pub fn (t Byteptr) str() string { return 'Byteptr' }
pub fn (t Const) str() string { return t.name }
pub fn (t Enum) str() string { return t.name }
pub fn (t Struct) str() string { return t.name }
pub fn (t Int) str() string { return if t.is_unsigned {'u$t.bit_size' } else { 'i$t.bit_size' } }
pub fn (t Float) str() string { return 'f$t.bit_size' }
pub fn (t String) str() string { return 'string' }
pub fn (t Char) str() string { return 'char' }
pub fn (t Byte) str() string { return 'byte' }
// pub fn (t Array) str() string { return 'array_$t.elem_type_kind.str()_$t.nr_dims' }
pub fn (t Array) str() string {
	elem_type := if t.elem_type_idx == -1 {
		t.elem_type_kind.str()
	} else {
		t.elem_type_idx.str()
	}
	return 'array_$elem_type'
}
// pub fn (t Map) str() string { return 'map_$t.key_type_kind.str()_$t.value_type_kind.str()' }
pub fn (t Map) str() string {
	key_type := if t.key_type_idx == -1 {
		t.key_type_kind.str()
	} else {
		t.key_type_idx.str()
	}
	value_type := if t.key_type_idx == -1 {
		t.key_type_kind.str()
	} else {
		t.key_type_idx.str()
	}
	return 'map_${key_type}_${value_type}'
}
pub fn (t MultiReturn) str() string { return 'multi_return_$t.elem_type_kinds.str()' }
pub fn (t Variadic) str() string { return 'map_$t.elem_type_kind.str()' }

pub const (
	// void_type = Type{
	// 	'void',0}
	// int_type = Type{
	// 	'int',1}
	// string_type = Type{
	// 	'string',2}
	// f64_type = Type{
	// 	'f64',3}
	// bool_type = Type{
	// 	'bool',4}
	void_type    = Void{}
	voidptr_type = Voidptr{}
	charptr_type = Charptr{}
	byteptr_type = Byteptr{}
	int_type     = Int{32, false}
	i64_type     = Int{64, false}
	// byte_type    = Int{8, true}
	u32_type     = Int{32, true}
	u64_type     = Int{64, true}
	f32_type     = Float{32}
	f64_type     = Float{64}
	string_type  = String{}
	char_type    = Char{}
	byte_type    = Byte{}
	bool_type    = Bool{}
)


// pub type TypeIdent int

// [inline]
// pub fn new_ti(idx int, kind Kind, nr_muls int) TypeIdent {
// 	t := (idx << 16) | (int(kind) << 6) | nr_muls
// 	return t
// }

// [inline]
// pub fn (t TypeIdent) is_ptr() bool {
//     return (int(t) & (1 << 6) - 1) > 0
// }

// [inline]
// pub fn (t TypeIdent) nr_muls() int {
//     return int(t) & (1 << 6) - 1
// }

// [inline]
// pub fn (t TypeIdent) kind() int {
// 	return (int(t) >> 6) & (1 << 6) - 1
// }

// [inline]
// pub fn (t TypeIdent) idx() int {
// 	return int(t) >> 16
// }

// pub fn (got TypeIdent) check(expected TypeIdent) bool {
// 	if got.kind() != expected.kind() {
// 		return false
// 	}
// 	return true
// }


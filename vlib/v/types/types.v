// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

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
	_i16,
	_i64,
	_byte,
	_u16,
	_u32,
	_u64,
	_f32,
	_f64,
	_string,
	_char,
	_bool,
	_array,
	_array_fixed,
	_map,
	_multi_return,
	_variadic
}

pub struct TypeIdent {
pub:
	type_idx  int
	type_kind Kind
	type_name string
	nr_muls   int
}

[inline]
pub fn new_ti(type_kind Kind, type_name string, type_idx int, nr_muls int) TypeIdent {
    return TypeIdent{
		type_idx: type_idx
		type_kind: type_kind
		type_name: type_name
		nr_muls: nr_muls
	}
}

[inline]
pub fn new_base_ti(type_kind Kind, nr_muls int) TypeIdent {
    return TypeIdent{
		type_kind: type_kind
		type_name: type_kind.str()
		nr_muls: nr_muls
	}
}

[inline]
pub fn (ti &TypeIdent) is_ptr() bool {
    return ti.nr_muls > 0
}

[inline]
pub fn (ti &TypeIdent) is_int() bool {
    return ti.type_kind in [._i8, ._i16, ._int, ._i64, ._byte, ._u16, ._u32, ._u64]
}

[inline]
pub fn (ti &TypeIdent) is_float() bool {
    return ti.type_kind in [._f32, ._f64]
}

[inline]
pub fn (ti &TypeIdent) is_number() bool {
    return ti.is_int() || ti.is_float()
}

pub fn (ti &TypeIdent) str() string {
	return '$ti.type_kind.str() $ti.type_idx: $ti.type_name ($ti.nr_muls)'
}

pub fn check(got, expected &TypeIdent) bool {
	if got.type_idx != expected.type_idx {
		return false
	}
	return true
}

pub fn (t Kind) str() string {
	t_str := match t {
		._placeholder {
			'placeholder'
		}
		._void  {
			'void'
		} 
		._voidptr  {
			'voidptr'
		}
		._charptr  {
			'charptr'
		}
		._byteptr  {
			'byteptr'
		}
		._const  {
			'const'
		}
		._enum  {
			'enum'
		}
		._struct  {
			'struct'
		}
		._int  {
			'int'
		}
		._i8  {
			'i8'
		}
		._i16  {
			'i16'
		}
		._i64  {
			'i64'
		}
		._byte  {
			'byte'
		}
		._u16  {
			'u18'
		}
		._f32  {
			'f32'
		}
		._f64  {
			'f64'
		}
		._string  {
			'string'
		}
		._char  {
			'char'
		}
		._bool  {
			'bool'
		}
		._array  {
			'array'
		}
		._array_fixed  {
			'array_fixed'
		}
		._map  {
			'map'
		}
		._multi_return  {
			'multi_return'
		}
		._variadic {
			'variadic'
		}
		else {
			'unknown'
		}
	}
	return t_str
}

pub type Type = Placeholder | Void | Voidptr | Charptr | Byteptr | Const | Enum | Struct |
	Int | Float | String | Char | Byte | Bool | Array | ArrayFixed | Map | MultiReturn | Variadic


pub struct Placeholder {
pub:
	idx  int
	name string
	kind Kind
}

pub struct Void {}

pub struct Voidptr {}

pub struct Charptr {}

pub struct Byteptr {}

pub struct Const {
pub:
	idx  int
	name string
}

pub struct Enum {
pub:
	idx  int
	name string
}

pub struct Struct {
pub:
	idx    int
	name   string
	fields []Field
}

pub struct Field {
pub:
	name     string
	type_idx int
}

pub struct Int {
pub:
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
pub:
	idx            int
	name           string
	elem_type_kind Kind
	elem_type_idx  int
	elem_is_ptr    bool
	nr_dims        int
}

pub struct ArrayFixed {
pub:
	idx            int
	name           string
	elem_type_kind Kind
	elem_type_idx  int
	elem_is_ptr    bool
	size           int
}

pub struct Map {
pub:
	idx             int
	name            string
	key_type_kind   Kind
	key_type_idx    int 
	value_type_kind Kind
	value_type_idx  int
}

pub struct MultiReturn {
pub:
	elem_type_kinds []Kind
	elem_type_idxs  []int
}

pub struct Variadic {
pub:
	elem_type_kind Kind
	elem_type_idx  int
}

pub fn (t Void) str() string { return 'void' }
pub fn (t Voidptr) str() string { return 'voidptr' }
pub fn (t Charptr) str() string { return 'charptr' }
pub fn (t Byteptr) str() string { return 'byteptr' }
pub fn (t Const) str() string { return t.name }
pub fn (t Enum) str() string { return t.name }
pub fn (t Struct) str() string { return t.name }
pub fn (t Int) str() string { return if t.is_unsigned {'u$t.bit_size' } else { 'i$t.bit_size' } }
pub fn (t Float) str() string { return 'f$t.bit_size' }
pub fn (t String) str() string { return 'string' }
pub fn (t Char) str() string { return 'char' }
pub fn (t Byte) str() string { return 'byte' }
pub fn (t Array) str() string { return t.name }
pub fn (t ArrayFixed) str() string { return t.name }
pub fn (t Map) str() string { return t.name }
pub fn (t MultiReturn) str() string { return 'multi_return_$t.elem_type_kinds.str()' }
pub fn (t Variadic) str() string { return 'variadic_$t.elem_type_kind.str()' }

pub const (
	void_type    = Void{}
	voidptr_type = Voidptr{}
	charptr_type = Charptr{}
	byteptr_type = Byteptr{}
	i8_type      = Int{8, false}
	i16_type     = Int{16, false}
	int_type     = Int{32, false}
	i64_type     = Int{64, false}
	byte_type    = Int{8, true}
	u16_type     = Int{16, true}
	u32_type     = Int{32, true}
	u64_type     = Int{64, true}
	f32_type     = Float{32}
	f64_type     = Float{64}
	string_type  = String{}
	char_type    = Char{}
	bool_type    = Bool{}
)

// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

pub enum Kind {
	placeholder
	void,
	voidptr,
	charptr,
	byteptr,
	const_,
	enum_,
	struct_,
	int,
	i8,
	i16,
	i64,
	byte,
	u16,
	u32,
	u64,
	f32,
	f64,
	string,
	char,
	bool,
	array,
	array_fixed,
	map,
	multi_return,
	variadic
}

pub type Type = Placeholder | Primitive | Const | Enum | Struct | Int | Float | 
	String | Byte | Bool | Array | ArrayFixed | Map | MultiReturn | Variadic

pub struct TypeIdent {
pub:
	idx     int
	kind    Kind
	name    string
	nr_muls int
}

[inline]
pub fn new_ti(kind Kind, name string, idx int, nr_muls int) TypeIdent {
	return TypeIdent{
		idx: idx
		kind: kind
		name: name
		nr_muls: nr_muls
	}
}

[inline]
pub fn new_builtin_ti(kind Kind, nr_muls int) TypeIdent {
	return TypeIdent{
		idx: -int(kind) - 1
		kind: kind
		name: kind.str()
		nr_muls: nr_muls
	}
}

[inline]
pub fn (ti &TypeIdent) is_ptr() bool {
	return ti.nr_muls > 0
}

[inline]
pub fn (ti &TypeIdent) is_int() bool {
	return ti.kind in [.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64]
}

[inline]
pub fn (ti &TypeIdent) is_float() bool {
	return ti.kind in [.f32, .f64]
}

[inline]
pub fn (ti &TypeIdent) is_number() bool {
	return ti.is_int() || ti.is_float()
}

pub fn (ti &TypeIdent) str() string {
	mut muls := ''
	for _ in 0 .. ti.nr_muls {
		muls += '&'
	}
	return '$muls$ti.name'
}

pub fn check(got, expected &TypeIdent) bool {
	if expected.kind == .voidptr {
		return true
	}
	if expected.name == 'array' {
		return true
	}
	if got.idx != expected.idx {
		return false
	}
	return true
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
		.const_{
			'const'
		}
		.enum_{
			'enum'
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

pub struct Placeholder {
pub:
	idx  int
	name string
	// kind Kind
}

// Void | Voidptr | Charptr | Byteptr
pub struct Primitive {
pub:
	idx  int
	kind Kind
}

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
	idx        int
	parent_idx int
	name       string
pub mut:
	fields     []Field
	methods    []Field // TODO Method
}

pub struct Field {
pub:
	name     string
	type_idx int
}

pub struct Int {
pub:
	idx			int
	bit_size    u32
	is_unsigned bool
}

pub struct Float {
pub:
	idx      int
	bit_size u32
}

pub struct String {
pub:
	idx int
}

pub struct Byte {
pub:
	idx int
}

pub struct Bool {
pub:
	idx int
}

pub struct Array {
pub:
	idx            int
	parent_idx     int
	name           string
	elem_type_kind Kind
	elem_type_idx  int
	elem_is_ptr    bool
	nr_dims        int
}

pub struct ArrayFixed {
pub:
	idx            int
	parent_idx     int
	name           string
	elem_type_kind Kind
	elem_type_idx  int
	elem_is_ptr    bool
	nr_dims        int
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
	idx  int
	name string
	tis  []TypeIdent
}

pub struct Variadic {
pub:
	idx int
	ti  TypeIdent
}

pub fn (t Primitive) str() string {
	s := match t.kind {
		.void {
			'void'
		}
		.voidptr {
			'voidptr'
		}
		.charptr {
			'charptr'
		}
		.byteptr {
			'byteptr'
		}
		.char {
			'char'
		}
		else {
			'unknown'
		}
	}
	return s
}

pub fn (t Const) str() string {
	return t.name
}

pub fn (t Enum) str() string {
	return t.name
}

pub fn (t Struct) str() string {
	return t.name
}

pub fn (t Int) str() string {
	return if t.is_unsigned { 'u$t.bit_size' } else { 'i$t.bit_size' }
}

pub fn (t Float) str() string {
	return 'f$t.bit_size'
}

pub fn (t String) str() string {
	return 'string'
}

pub fn (t Byte) str() string {
	return 'byte'
}

pub fn (t Array) str() string {
	return t.name
}

pub fn (t ArrayFixed) str() string {
	return t.name
}

pub fn (t Map) str() string {
	return t.name
}

pub fn (t MultiReturn) str() string {
	return t.name
}

pub fn (t Variadic) str() string {
	return 'variadic_$t.ti.kind.str()'
}

/*
pub fn (s &Struct) has_field(name string) bool {

}
*/


pub const (
	void_type = Primitive{idx: 1, kind: .void}
	voidptr_type = Primitive{idx:2, kind: .voidptr}
	charptr_type = Primitive{idx:3, kind: .charptr}
	byteptr_type = Primitive{idx:4, kind: .byteptr}
	i8_type = Int{
		5, 8,false}
	i16_type = Int{
		6, 16,false}
	int_type = Int{
		7, 32,false}
	i64_type = Int{
		8, 64,false}
	byte_type = Int{
		9, 8,true}
	u16_type = Int{
		10, 16,true}
	u32_type = Int{
		11, 32,true}
	u64_type = Int{
		12, 64,true}
	f32_type = Float{
		13, 32}
	f64_type = Float{
		14, 64}
	string_type = String{15}
	char_type = Primitive{idx: 16, kind: .char}
	bool_type = Bool{17}
)

pub const (
	void_ti = new_ti(.void, 'void', void_type.idx, 0)
	int_ti = new_ti(.int, 'int', int_type.idx, 0)
	string_ti = new_ti(.string, 'string', string_type.idx, 0)
	bool_ti = new_ti(.bool, 'bool', bool_type.idx, 0)
)

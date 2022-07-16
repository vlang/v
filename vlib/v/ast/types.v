// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Type layout information (32 bits)
// flag (8 bits) | nr_muls (8 bits) | idx (16 bits)
// pack: (int(flag)<<24) | (nr_muls<<16) | u16(idx)
// unpack:
// flag: (int(type)>>24) & 0xff
// nr_muls: (int(type)>>16) & 0xff
// idx:  u16(type) & 0xffff
module ast

import strings
import v.pref

pub type Type = int

pub type TypeInfo = Aggregate
	| Alias
	| Array
	| ArrayFixed
	| Chan
	| Enum
	| FnType
	| GenericInst
	| Interface
	| Map
	| MultiReturn
	| Struct
	| SumType
	| Thread

pub enum Language {
	v
	c
	js
	amd64 // aka x86_64
	i386
	arm64 // 64-bit arm
	arm32 // 32-bit arm
	rv64 // 64-bit risc-v
	rv32 // 32-bit risc-v
}

pub fn pref_arch_to_table_language(pref_arch pref.Arch) Language {
	return match pref_arch {
		.amd64 {
			.amd64
		}
		.arm64 {
			.arm64
		}
		.arm32 {
			.arm32
		}
		.rv64 {
			.rv64
		}
		.rv32 {
			.rv32
		}
		.i386 {
			.i386
		}
		.js_node, .js_browser, .js_freestanding {
			.js
		}
		._auto, ._max {
			.v
		}
	}
}

// Represents a type that only needs an identifier, e.g. int, array_int.
// A pointer type `&T` would have a TypeSymbol `T`.
// Note: For a Type, use:
// * Table.type_to_str(typ) not TypeSymbol.name.
// * Table.type_kind(typ) not TypeSymbol.kind.
// Each TypeSymbol is entered into `Table.types`.
// See also: Table.sym.
[minify]
pub struct TypeSymbol {
pub:
	parent_idx int
pub mut:
	info     TypeInfo
	kind     Kind
	name     string // the internal & source name of the type, i.e. `[5]int`.
	cname    string // the name with no dots for use in the generated C code
	methods  []Fn
	mod      string
	is_pub   bool
	language Language
	idx      int
	size     int = -1
	align    int = -1
}

// max of 8
pub enum TypeFlag {
	optional
	result
	variadic
	generic
	shared_f
	atomic_f
}

/*
To save precious TypeFlag bits the 4 possible ShareTypes are coded in the two
bits `shared` and `atomic_or_rw` (see sharetype_from_flags() below).
*/
pub enum ShareType {
	mut_t
	shared_t
	atomic_t
}

pub fn (t ShareType) str() string {
	match t {
		.mut_t { return 'mut' }
		.shared_t { return 'shared' }
		.atomic_t { return 'atomic' }
	}
}

// <atomic.h> defines special typenames
pub fn (t Type) atomic_typename() string {
	idx := t.idx()
	match idx {
		ast.u32_type_idx { return 'atomic_uint' }
		ast.int_type_idx { return '_Atomic int' }
		ast.u64_type_idx { return 'atomic_ullong' }
		ast.i64_type_idx { return 'atomic_llong' }
		else { return 'unknown_atomic' }
	}
}

pub fn sharetype_from_flags(is_shared bool, is_atomic bool) ShareType {
	return ShareType(int(u32(is_atomic) << 1) | int(is_shared))
}

pub fn (t Type) share() ShareType {
	return sharetype_from_flags(t.has_flag(.shared_f), t.has_flag(.atomic_f))
}

// return TypeSymbol idx for `t`
[inline]
pub fn (t Type) idx() int {
	return u16(t) & 0xffff
}

[inline]
pub fn (t Type) is_void() bool {
	return t == ast.void_type
}

[inline]
pub fn (t Type) is_full() bool {
	return t != 0 && t != ast.void_type
}

// return nr_muls for `t`
[inline]
pub fn (t Type) nr_muls() int {
	return (int(t) >> 16) & 0xff
}

// return true if `t` is a pointer (nr_muls>0)
[inline]
pub fn (t Type) is_ptr() bool {
	// any normal pointer, i.e. &Type, &&Type etc;
	// Note: voidptr, charptr and byteptr are NOT included!
	return (int(t) >> 16) & 0xff > 0
}

[inline]
pub fn (t Type) is_any_kind_of_pointer() bool {
	return (int(t) >> 16) & 0xff > 0 || (u16(t) & 0xffff) in ast.pointer_type_idxs
}

// set nr_muls on `t` and return it
[inline]
pub fn (t Type) set_nr_muls(nr_muls int) Type {
	if nr_muls < 0 || nr_muls > 255 {
		panic('set_nr_muls: nr_muls must be between 0 & 255')
	}
	return int(t) & 0xff00ffff | int(u32(nr_muls) << 16)
}

// increments nr_muls on `t` and return it
[inline]
pub fn (t Type) ref() Type {
	nr_muls := (int(t) >> 16) & 0xff
	if nr_muls == 255 {
		panic('ref: nr_muls is already at max of 255')
	}
	return int(t) & 0xff00ffff | int(u32(nr_muls + 1) << 16)
}

// decrement nr_muls on `t` and return it
[inline]
pub fn (t Type) deref() Type {
	nr_muls := (int(t) >> 16) & 0xff
	if nr_muls == 0 {
		panic('deref: type `$t` is not a pointer')
	}
	return int(t) & 0xff00ffff | int(u32(nr_muls - 1) << 16)
}

// set `flag` on `t` and return `t`
[inline]
pub fn (t Type) set_flag(flag TypeFlag) Type {
	return int(t) | (1 << (int(flag) + 24))
}

// clear `flag` on `t` and return `t`
[inline]
pub fn (t Type) clear_flag(flag TypeFlag) Type {
	return int(t) & ~(1 << (int(flag) + 24))
}

// clear all flags
[inline]
pub fn (t Type) clear_flags() Type {
	return int(t) & 0xffffff
}

// return true if `flag` is set on `t`
[inline]
pub fn (t Type) has_flag(flag TypeFlag) bool {
	return int(t) & (1 << (int(flag) + 24)) > 0
}

// debug returns a verbose representation of the information in ts, useful for tracing/debugging
pub fn (ts TypeSymbol) debug() []string {
	mut res := []string{}
	ts.dbg_common(mut res)
	res << 'info: $ts.info'
	res << 'methods ($ts.methods.len): ' + ts.methods.map(it.str()).join(', ')
	return res
}

// same as .debug(), but without the verbose .info and .methods fields
pub fn (ts TypeSymbol) dbg() []string {
	mut res := []string{}
	ts.dbg_common(mut res)
	return res
}

fn (ts TypeSymbol) dbg_common(mut res []string) {
	res << 'idx: 0x$ts.idx.hex()'
	res << 'parent_idx: 0x$ts.parent_idx.hex()'
	res << 'mod: $ts.mod'
	res << 'name: $ts.name'
	res << 'cname: $ts.cname'
	res << 'kind: $ts.kind'
	res << 'is_pub: $ts.is_pub'
	res << 'language: $ts.language'
}

pub fn (t Type) str() string {
	return 'ast.Type(0x$t.hex() = ${u32(t)})'
}

pub fn (t &Table) type_str(typ Type) string {
	return t.sym(typ).name
}

// debug returns a verbose representation of the information in the type `t`, useful for tracing/debugging
pub fn (t Type) debug() []string {
	mut res := []string{}
	res << 'idx: 0x${t.idx().hex():-8}'
	res << 'type: 0x${t.hex():-8}'
	res << 'nr_muls: $t.nr_muls()'
	if t.has_flag(.optional) {
		res << 'optional'
	}
	if t.has_flag(.variadic) {
		res << 'variadic'
	}
	if t.has_flag(.generic) {
		res << 'generic'
	}
	if t.has_flag(.shared_f) {
		res << 'shared_f'
	}
	if t.has_flag(.atomic_f) {
		res << 'atomic_f'
	}
	return res
}

// copy flags & nr_muls from `t_from` to `t` and return `t`
[inline]
pub fn (t Type) derive(t_from Type) Type {
	return (0xffff0000 & t_from) | u16(t)
}

// copy flags from `t_from` to `t` and return `t`
[inline]
pub fn (t Type) derive_add_muls(t_from Type) Type {
	return Type((0xff000000 & t_from) | u16(t)).set_nr_muls(t.nr_muls() + t_from.nr_muls())
}

// return new type with TypeSymbol idx set to `idx`
[inline]
pub fn new_type(idx int) Type {
	if idx < 1 || idx > 65535 {
		panic('new_type: idx must be between 1 & 65535')
	}
	return idx
}

// return new type with TypeSymbol idx set to `idx` & nr_muls set to `nr_muls`
[inline]
pub fn new_type_ptr(idx int, nr_muls int) Type {
	if idx < 1 || idx > 65535 {
		panic('new_type_ptr: idx must be between 1 & 65535')
	}
	if nr_muls < 0 || nr_muls > 255 {
		panic('new_type_ptr: nr_muls must be between 0 & 255')
	}
	return (u32(nr_muls) << 16) | u16(idx)
}

[inline]
pub fn (typ Type) is_pointer() bool {
	// builtin pointer types (voidptr, byteptr, charptr)
	return typ.idx() in ast.pointer_type_idxs
}

[inline]
pub fn (typ Type) is_voidptr() bool {
	return typ.idx() == ast.voidptr_type_idx
}

[inline]
pub fn (typ Type) is_real_pointer() bool {
	return typ.is_ptr() || typ.is_pointer()
}

[inline]
pub fn (typ Type) is_float() bool {
	return typ.clear_flags() in ast.float_type_idxs
}

[inline]
pub fn (typ Type) is_int() bool {
	return typ.clear_flags() in ast.integer_type_idxs
}

[inline]
pub fn (typ Type) is_int_valptr() bool {
	return typ.idx() in ast.integer_type_idxs
}

[inline]
pub fn (typ Type) is_float_valptr() bool {
	return typ.idx() in ast.float_type_idxs
}

[inline]
pub fn (typ Type) is_pure_int() bool {
	return int(typ) in ast.integer_type_idxs
}

[inline]
pub fn (typ Type) is_pure_float() bool {
	return int(typ) in ast.float_type_idxs
}

[inline]
pub fn (typ Type) is_signed() bool {
	return typ.idx() in ast.signed_integer_type_idxs
}

[inline]
pub fn (typ Type) is_unsigned() bool {
	return typ.idx() in ast.unsigned_integer_type_idxs
}

pub fn (typ Type) flip_signedness() Type {
	return match typ {
		ast.i8_type { ast.u8_type }
		ast.i16_type { ast.u16_type }
		ast.int_type { ast.u32_type }
		ast.isize_type { ast.usize_type }
		ast.i64_type { ast.u64_type }
		ast.u8_type { ast.i8_type }
		ast.u16_type { ast.i16_type }
		ast.u32_type { ast.int_type }
		ast.usize_type { ast.isize_type }
		ast.u64_type { ast.i64_type }
		else { typ }
	}
}

[inline]
pub fn (typ Type) is_int_literal() bool {
	return int(typ) == ast.int_literal_type_idx
}

[inline]
pub fn (typ Type) is_number() bool {
	return typ.clear_flags() in ast.number_type_idxs
}

[inline]
pub fn (typ Type) is_string() bool {
	return typ.idx() in ast.string_type_idxs
}

[inline]
pub fn (typ Type) is_bool() bool {
	return typ.idx() == ast.bool_type_idx
}

pub const (
	void_type_idx          = 1
	voidptr_type_idx       = 2
	byteptr_type_idx       = 3
	charptr_type_idx       = 4
	i8_type_idx            = 5
	i16_type_idx           = 6
	int_type_idx           = 7
	i64_type_idx           = 8
	isize_type_idx         = 9
	u8_type_idx            = 10
	u16_type_idx           = 11
	u32_type_idx           = 12
	u64_type_idx           = 13
	usize_type_idx         = 14
	f32_type_idx           = 15
	f64_type_idx           = 16
	char_type_idx          = 17
	bool_type_idx          = 18
	none_type_idx          = 19
	string_type_idx        = 20
	rune_type_idx          = 21
	array_type_idx         = 22
	map_type_idx           = 23
	chan_type_idx          = 24
	any_type_idx           = 25
	float_literal_type_idx = 26
	int_literal_type_idx   = 27
	thread_type_idx        = 28
	error_type_idx         = 29
	nil_type_idx           = 30
)

// Note: builtin_type_names must be in the same order as the idx consts above
pub const builtin_type_names = ['void', 'voidptr', 'byteptr', 'charptr', 'i8', 'i16', 'int', 'i64',
	'isize', 'u8', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'char', 'bool', 'none', 'string',
	'rune', 'array', 'map', 'chan', 'any', 'float_literal', 'int_literal', 'thread', 'Error', 'nil']

pub const builtin_type_names_matcher = build_builtin_type_names_matcher()

pub const (
	integer_type_idxs          = [i8_type_idx, i16_type_idx, int_type_idx, i64_type_idx, u8_type_idx,
		u16_type_idx, u32_type_idx, u64_type_idx, isize_type_idx, usize_type_idx,
		int_literal_type_idx, rune_type_idx]
	signed_integer_type_idxs   = [char_type_idx, i8_type_idx, i16_type_idx, int_type_idx,
		i64_type_idx, isize_type_idx]
	unsigned_integer_type_idxs = [u8_type_idx, u16_type_idx, u32_type_idx, u64_type_idx,
		usize_type_idx]
	// C will promote any type smaller than int to int in an expression
	int_promoted_type_idxs     = [char_type_idx, i8_type_idx, i16_type_idx, u8_type_idx, u16_type_idx]
	float_type_idxs            = [f32_type_idx, f64_type_idx, float_literal_type_idx]
	number_type_idxs           = [i8_type_idx, i16_type_idx, int_type_idx, i64_type_idx, u8_type_idx,
		char_type_idx, u16_type_idx, u32_type_idx, u64_type_idx, isize_type_idx, usize_type_idx,
		f32_type_idx, f64_type_idx, int_literal_type_idx, float_literal_type_idx, rune_type_idx]
	pointer_type_idxs          = [voidptr_type_idx, byteptr_type_idx, charptr_type_idx]
	string_type_idxs           = [string_type_idx]
)

pub const (
	void_type          = new_type(void_type_idx)
	ovoid_type         = new_type(void_type_idx).set_flag(.optional) // the return type of `fn ()?`
	rvoid_type         = new_type(void_type_idx).set_flag(.result) // the return type of `fn () !`
	voidptr_type       = new_type(voidptr_type_idx)
	byteptr_type       = new_type(byteptr_type_idx)
	charptr_type       = new_type(charptr_type_idx)
	i8_type            = new_type(i8_type_idx)
	int_type           = new_type(int_type_idx)
	i16_type           = new_type(i16_type_idx)
	i64_type           = new_type(i64_type_idx)
	isize_type         = new_type(isize_type_idx)
	u8_type            = new_type(u8_type_idx)
	u16_type           = new_type(u16_type_idx)
	u32_type           = new_type(u32_type_idx)
	u64_type           = new_type(u64_type_idx)
	usize_type         = new_type(usize_type_idx)
	f32_type           = new_type(f32_type_idx)
	f64_type           = new_type(f64_type_idx)
	char_type          = new_type(char_type_idx)
	bool_type          = new_type(bool_type_idx)
	none_type          = new_type(none_type_idx)
	string_type        = new_type(string_type_idx)
	rune_type          = new_type(rune_type_idx)
	array_type         = new_type(array_type_idx)
	map_type           = new_type(map_type_idx)
	chan_type          = new_type(chan_type_idx)
	any_type           = new_type(any_type_idx)
	float_literal_type = new_type(float_literal_type_idx)
	int_literal_type   = new_type(int_literal_type_idx)
	thread_type        = new_type(thread_type_idx)
	error_type         = new_type(error_type_idx)
	charptr_types      = new_charptr_types()
	byteptr_types      = new_byteptr_types()
	voidptr_types      = new_voidptr_types()
	cptr_types         = merge_types(voidptr_types, byteptr_types, charptr_types)
	nil_type           = new_type(nil_type_idx)
)

fn new_charptr_types() []Type {
	return [ast.charptr_type, new_type(ast.char_type_idx).set_nr_muls(1)]
}

fn new_byteptr_types() []Type {
	return [ast.byteptr_type, new_type(ast.u8_type_idx).set_nr_muls(1)]
}

fn new_voidptr_types() []Type {
	return [ast.voidptr_type, new_type(ast.voidptr_type_idx).set_nr_muls(1)]
}

pub fn merge_types(params ...[]Type) []Type {
	mut res := []Type{cap: params.len}
	for types in params {
		for t in types {
			if t !in res {
				res << t
			}
		}
	}
	return res
}

pub fn mktyp(typ Type) Type {
	return match typ {
		ast.float_literal_type { ast.f64_type }
		ast.int_literal_type { ast.int_type }
		else { typ }
	}
}

pub struct MultiReturn {
pub mut:
	types []Type
}

pub struct FnType {
pub mut:
	is_anon  bool
	has_decl bool
	func     Fn
}

// returns TypeSymbol kind only if there are no type modifiers
pub fn (t &Table) type_kind(typ Type) Kind {
	if typ.nr_muls() > 0 || typ.has_flag(.optional) {
		return Kind.placeholder
	}
	return t.sym(typ).kind
}

pub fn (t &Table) type_is_for_pointer_arithmetic(typ Type) bool {
	typ_sym := t.sym(typ)
	if typ_sym.kind == .struct_ {
		return false
	} else {
		return typ.is_any_kind_of_pointer() || typ.is_int_valptr()
	}
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
	isize
	u8
	u16
	u32
	u64
	usize
	f32
	f64
	char
	rune
	bool
	none_
	string
	array
	array_fixed
	map
	chan
	any
	struct_
	generic_inst
	multi_return
	sum_type
	alias
	enum_
	function
	interface_
	float_literal
	int_literal
	aggregate
	thread
}

pub fn (t TypeSymbol) str() string {
	return t.name
}

[noreturn]
fn (t &TypeSymbol) no_info_panic(fname string) {
	panic('$fname: no info for type: $t.name')
}

[inline]
pub fn (t &TypeSymbol) enum_info() Enum {
	if t.info is Enum {
		return t.info
	}
	if t.info is Alias {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is Enum {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

[inline]
pub fn (t &TypeSymbol) mr_info() MultiReturn {
	if t.info is MultiReturn {
		return t.info
	}
	if t.info is Alias {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is MultiReturn {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

[inline]
pub fn (t &TypeSymbol) array_info() Array {
	if t.info is Array {
		return t.info
	}
	if t.info is Alias {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is Array {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

[inline]
pub fn (t &TypeSymbol) array_fixed_info() ArrayFixed {
	if t.info is ArrayFixed {
		return t.info
	}
	if t.info is Alias {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is ArrayFixed {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

[inline]
pub fn (t &TypeSymbol) chan_info() Chan {
	if t.info is Chan {
		return t.info
	}
	if t.info is Alias {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is Chan {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

[inline]
pub fn (t &TypeSymbol) thread_info() Thread {
	if t.info is Thread {
		return t.info
	}
	if t.info is Alias {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is Thread {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

[inline]
pub fn (t &TypeSymbol) map_info() Map {
	if t.info is Map {
		return t.info
	}
	if t.info is Alias {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is Map {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

[inline]
pub fn (t &TypeSymbol) struct_info() Struct {
	if t.info is Struct {
		return t.info
	}
	if t.info is Alias {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is Struct {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

[inline]
pub fn (t &TypeSymbol) sumtype_info() SumType {
	if t.info is SumType {
		return t.info
	}
	if t.info is SumType {
		fsym := global_table.final_sym(t.info.parent_type)
		if fsym.info is SumType {
			return fsym.info
		}
	}
	t.no_info_panic(@METHOD)
}

pub fn (t &TypeSymbol) is_heap() bool {
	if t.kind == .struct_ {
		info := t.info as Struct
		return info.is_heap
	} else {
		return false
	}
}

pub fn (mut t Table) register_builtin_type_symbols() {
	// reserve index 0 so nothing can go there
	// save index check, 0 will mean not found
	t.register_sym(kind: .placeholder, name: 'reserved_0')
	t.register_sym(kind: .void, name: 'void', cname: 'void', mod: 'builtin')
	t.register_sym(kind: .voidptr, name: 'voidptr', cname: 'voidptr', mod: 'builtin')
	t.register_sym(kind: .byteptr, name: 'byteptr', cname: 'byteptr', mod: 'builtin')
	t.register_sym(kind: .charptr, name: 'charptr', cname: 'charptr', mod: 'builtin')
	t.register_sym(kind: .i8, name: 'i8', cname: 'i8', mod: 'builtin')
	t.register_sym(kind: .i16, name: 'i16', cname: 'i16', mod: 'builtin')
	t.register_sym(kind: .int, name: 'int', cname: 'int', mod: 'builtin')
	t.register_sym(kind: .i64, name: 'i64', cname: 'i64', mod: 'builtin')
	t.register_sym(kind: .isize, name: 'isize', cname: 'isize', mod: 'builtin')
	t.register_sym(kind: .u8, name: 'u8', cname: 'u8', mod: 'builtin')
	t.register_sym(kind: .u16, name: 'u16', cname: 'u16', mod: 'builtin')
	t.register_sym(kind: .u32, name: 'u32', cname: 'u32', mod: 'builtin')
	t.register_sym(kind: .u64, name: 'u64', cname: 'u64', mod: 'builtin')
	t.register_sym(kind: .usize, name: 'usize', cname: 'usize', mod: 'builtin')
	t.register_sym(kind: .f32, name: 'f32', cname: 'f32', mod: 'builtin')
	t.register_sym(kind: .f64, name: 'f64', cname: 'f64', mod: 'builtin')
	t.register_sym(kind: .char, name: 'char', cname: 'char', mod: 'builtin')
	t.register_sym(kind: .bool, name: 'bool', cname: 'bool', mod: 'builtin')
	t.register_sym(kind: .none_, name: 'none', cname: 'none', mod: 'builtin')
	t.register_sym(kind: .string, name: 'string', cname: 'string', mod: 'builtin')
	t.register_sym(kind: .rune, name: 'rune', cname: 'rune', mod: 'builtin')
	t.register_sym(kind: .array, name: 'array', cname: 'array', mod: 'builtin')
	t.register_sym(kind: .map, name: 'map', cname: 'map', mod: 'builtin')
	t.register_sym(kind: .chan, name: 'chan', cname: 'chan', mod: 'builtin')
	t.register_sym(kind: .any, name: 'any', cname: 'any', mod: 'builtin')
	t.register_sym(
		kind: .float_literal
		name: 'float literal'
		cname: 'float_literal'
		mod: 'builtin'
	)
	t.register_sym(
		kind: .int_literal
		name: 'int literal'
		cname: 'int_literal'
		mod: 'builtin'
	)
	t.register_sym(
		kind: .thread
		name: 'thread'
		cname: '__v_thread'
		mod: 'builtin'
		info: Thread{
			return_type: ast.void_type
		}
	)
	t.register_sym(kind: .interface_, name: 'IError', cname: 'IError', mod: 'builtin')
	t.register_sym(kind: .voidptr, name: 'nil', cname: 'nil', mod: 'builtin')
}

[inline]
pub fn (t &TypeSymbol) is_pointer() bool {
	return t.kind in [.byteptr, .charptr, .voidptr]
}

[inline]
pub fn (t &TypeSymbol) is_int() bool {
	res := t.kind in [.i8, .i16, .int, .i64, .isize, .u8, .u16, .u32, .u64, .usize, .int_literal,
		.rune]
	if !res && t.kind == .alias {
		return (t.info as Alias).parent_type.is_number()
	}
	return res
}

[inline]
pub fn (t &TypeSymbol) is_float() bool {
	return t.kind in [.f32, .f64, .float_literal]
}

[inline]
pub fn (t &TypeSymbol) is_string() bool {
	return t.kind == .string
}

[inline]
pub fn (t &TypeSymbol) is_number() bool {
	return t.is_int() || t.is_float()
}

[inline]
pub fn (t &TypeSymbol) is_primitive() bool {
	return t.is_number() || t.is_pointer() || t.is_string()
}

[inline]
pub fn (t &TypeSymbol) is_builtin() bool {
	return t.mod == 'builtin'
}

// type_size returns the size and alignment (in bytes) of `typ`, similarly to  C's `sizeof()` and `alignof()`.
pub fn (t &Table) type_size(typ Type) (int, int) {
	if typ.has_flag(.optional) {
		return t.type_size(ast.error_type_idx)
	}
	if typ.nr_muls() > 0 {
		return t.pointer_size, t.pointer_size
	}
	mut sym := t.sym(typ)
	if sym.size != -1 {
		return sym.size, sym.align
	}
	mut size := 0
	mut align := 0
	match sym.kind {
		.placeholder, .void, .none_, .generic_inst {}
		.voidptr, .byteptr, .charptr, .function, .usize, .isize, .any, .thread, .chan {
			size = t.pointer_size
		}
		.i8, .u8, .char, .bool {
			size = 1
			align = 1
		}
		.i16, .u16 {
			size = 2
			align = 2
		}
		.int, .u32, .rune, .f32, .enum_ {
			size = 4
			align = 4
		}
		.i64, .u64, .int_literal, .f64, .float_literal {
			size = 8
			align = 8
		}
		.alias {
			size, align = t.type_size((sym.info as Alias).parent_type)
		}
		.struct_, .string, .multi_return {
			mut max_alignment := 0
			mut total_size := 0
			types := if mut sym.info is Struct {
				sym.info.fields.map(it.typ)
			} else {
				(sym.info as MultiReturn).types
			}
			for ftyp in types {
				field_size, alignment := t.type_size(ftyp)
				if alignment > max_alignment {
					max_alignment = alignment
				}
				total_size = round_up(total_size, alignment) + field_size
			}
			size = round_up(total_size, max_alignment)
			align = max_alignment
		}
		.sum_type, .interface_, .aggregate {
			match mut sym.info {
				SumType, Aggregate {
					size = (sym.info.fields.len + 2) * t.pointer_size
					align = t.pointer_size
				}
				Interface {
					size = (sym.info.fields.len + 2) * t.pointer_size
					align = t.pointer_size
					for etyp in sym.info.embeds {
						esize, _ := t.type_size(etyp)
						size += esize - 2 * t.pointer_size
					}
				}
				else {
					// unreachable
				}
			}
		}
		.array_fixed {
			info := sym.info as ArrayFixed
			elem_size, elem_align := t.type_size(info.elem_type)
			size = info.size * elem_size
			align = elem_align
		}
		// TODO hardcoded:
		.map {
			size = if t.pointer_size == 8 { 120 } else { 80 }
			align = t.pointer_size
		}
		.array {
			size = if t.pointer_size == 8 { 32 } else { 24 }
			align = t.pointer_size
		}
	}
	sym.size = size
	sym.align = align
	return size, align
}

// round_up rounds the number `n` up to the next multiple `multiple`.
// Note: `multiple` must be a power of 2.
[inline]
fn round_up(n int, multiple int) int {
	return (n + multiple - 1) & -multiple
}

// for debugging/errors only, perf is not an issue
pub fn (k Kind) str() string {
	return match k {
		.placeholder { 'placeholder' }
		.void { 'void' }
		.voidptr { 'voidptr' }
		.charptr { 'charptr' }
		.byteptr { 'byteptr' }
		.struct_ { 'struct' }
		.int { 'int' }
		.i8 { 'i8' }
		.i16 { 'i16' }
		.i64 { 'i64' }
		.isize { 'isize' }
		.u8 { 'u8' }
		.u16 { 'u16' }
		.u32 { 'u32' }
		.u64 { 'u64' }
		.usize { 'usize' }
		.int_literal { 'int_literal' }
		.f32 { 'f32' }
		.f64 { 'f64' }
		.float_literal { 'float_literal' }
		.string { 'string' }
		.char { 'char' }
		.bool { 'bool' }
		.none_ { 'none' }
		.array { 'array' }
		.array_fixed { 'array_fixed' }
		.map { 'map' }
		.chan { 'chan' }
		.multi_return { 'multi_return' }
		.sum_type { 'sum_type' }
		.alias { 'alias' }
		.enum_ { 'enum' }
		.any { 'any' }
		.function { 'function' }
		.interface_ { 'interface' }
		.generic_inst { 'generic_inst' }
		.rune { 'rune' }
		.aggregate { 'aggregate' }
		.thread { 'thread' }
	}
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

[minify]
pub struct Struct {
pub:
	attrs []Attr
pub mut:
	embeds         []Type
	fields         []StructField
	is_typedef     bool // C. [typedef]
	is_union       bool
	is_heap        bool
	is_minify      bool
	is_anon        bool
	is_generic     bool
	generic_types  []Type
	concrete_types []Type
	parent_type    Type
}

// instantiation of a generic struct
pub struct GenericInst {
pub mut:
	parent_idx     int    // idx of the base generic struct
	concrete_types []Type // concrete types, e.g. <int, string>
}

[minify]
pub struct Interface {
pub mut:
	types   []Type // all types that implement this interface
	fields  []StructField
	methods []Fn
	embeds  []Type
	// `I1 is I2` conversions
	conversions map[int][]Type
	// generic interface support
	is_generic     bool
	generic_types  []Type
	concrete_types []Type
	parent_type    Type
}

pub struct Enum {
pub:
	vals             []string
	is_flag          bool
	is_multi_allowed bool
	uses_exprs       bool
}

[minify]
pub struct Alias {
pub:
	parent_type Type
	language    Language
	is_import   bool
}

pub struct Aggregate {
mut:
	fields []StructField // used for faster lookup inside the module
pub:
	sum_type Type
	types    []Type
}

pub struct Array {
pub:
	nr_dims int
pub mut:
	elem_type Type
}

[minify]
pub struct ArrayFixed {
pub:
	size      int
	size_expr Expr // used by fmt for e.g. ´[my_const]u8´
pub mut:
	elem_type Type
}

pub struct Chan {
pub mut:
	elem_type Type
	is_mut    bool
}

pub struct Thread {
pub mut:
	return_type Type
}

pub struct Map {
pub mut:
	key_type   Type
	value_type Type
}

[minify]
pub struct SumType {
pub mut:
	fields       []StructField
	found_fields bool
	is_anon      bool
	// generic sumtype support
	is_generic     bool
	variants       []Type
	generic_types  []Type
	concrete_types []Type
	parent_type    Type
}

// human readable type name, also used by vfmt
pub fn (t &Table) type_to_str(typ Type) string {
	return t.type_to_str_using_aliases(typ, map[string]string{})
}

// type name in code (for builtin)
pub fn (mytable &Table) type_to_code(t Type) string {
	match t {
		ast.int_literal_type, ast.float_literal_type { return mytable.sym(t).kind.str() }
		else { return mytable.type_to_str_using_aliases(t, map[string]string{}) }
	}
}

// clean type name from generics form. From Type<int> -> Type
pub fn (t &Table) clean_generics_type_str(typ Type) string {
	result := t.type_to_str(typ)
	return result.all_before('<')
}

// import_aliases is a map of imported symbol aliases 'module.Type' => 'Type'
pub fn (t &Table) type_to_str_using_aliases(typ Type, import_aliases map[string]string) string {
	cache_key := (u64(import_aliases.len) << 32) | u64(typ)
	if cached_res := t.cached_type_to_str[cache_key] {
		return cached_res
	}
	sym := t.sym(typ)
	mut res := sym.name
	mut mt := unsafe { &Table(t) }
	defer {
		// Note, that this relies on `res = value return res` if you want to return early!
		mt.cached_type_to_str[cache_key] = res
	}
	// Note, that the duplication of code in some of the match branches here
	// is VERY deliberate. DO NOT be tempted to use `else {}` instead, because
	// that strongly reduces the usefullness of the exhaustive checking that
	// match does.
	//    Using else{} here led to subtle bugs in vfmt discovered *months*
	// after the original code was written.
	//    It is important that each case here is handled *explicitly* and
	// *clearly*, and that when a new kind is added, it should also be handled
	// explicitly.
	match sym.kind {
		.int_literal, .float_literal {}
		.i8, .i16, .int, .i64, .isize, .u8, .u16, .u32, .u64, .usize, .f32, .f64, .char, .rune,
		.string, .bool, .none_, .voidptr, .byteptr, .charptr {
			// primitive types
			res = sym.kind.str()
		}
		.array {
			if typ == ast.array_type {
				res = 'array'
				return res
			}
			if typ.has_flag(.variadic) {
				res = t.type_to_str_using_aliases(t.value_type(typ), import_aliases)
			} else {
				if sym.info is Array {
					elem_str := t.type_to_str_using_aliases(sym.info.elem_type, import_aliases)
					res = '[]$elem_str'
				} else {
					res = 'array'
				}
			}
		}
		.array_fixed {
			info := sym.info as ArrayFixed
			elem_str := t.type_to_str_using_aliases(info.elem_type, import_aliases)
			if info.size_expr is EmptyExpr {
				res = '[$info.size]$elem_str'
			} else {
				res = '[$info.size_expr]$elem_str'
			}
		}
		.chan {
			// TODO currently the `chan` struct in builtin is not considered a struct but a chan
			if sym.mod != 'builtin' && sym.name != 'chan' {
				info := sym.info as Chan
				mut elem_type := info.elem_type
				mut mut_str := ''
				if info.is_mut {
					mut_str = 'mut '
					elem_type = elem_type.set_nr_muls(elem_type.nr_muls() - 1)
				}
				elem_str := t.type_to_str_using_aliases(elem_type, import_aliases)
				res = 'chan $mut_str$elem_str'
			}
		}
		.function {
			info := sym.info as FnType
			if !t.is_fmt {
				res = t.fn_signature(info.func, type_only: true)
			} else {
				if res.starts_with('fn (') {
					// fn foo ()
					has_names := info.func.params.any(it.name.len > 0)
					res = t.fn_signature_using_aliases(info.func, import_aliases,
						type_only: !has_names
					)
				} else {
					// FnFoo
					res = t.shorten_user_defined_typenames(res, import_aliases)
				}
			}
		}
		.map {
			if int(typ) == ast.map_type_idx {
				res = 'map'
				return res
			}
			info := sym.info as Map
			key_str := t.type_to_str_using_aliases(info.key_type, import_aliases)
			val_str := t.type_to_str_using_aliases(info.value_type, import_aliases)
			res = 'map[$key_str]$val_str'
		}
		.multi_return {
			res = '('
			info := sym.info as MultiReturn
			for i, typ2 in info.types {
				if i > 0 {
					res += ', '
				}
				res += t.type_to_str_using_aliases(typ2, import_aliases)
			}
			res += ')'
		}
		.struct_, .interface_, .sum_type {
			if typ.has_flag(.generic) {
				match sym.info {
					Struct, Interface, SumType {
						res += '<'
						for i, gtyp in sym.info.generic_types {
							res += t.sym(gtyp).name
							if i != sym.info.generic_types.len - 1 {
								res += ', '
							}
						}
						res += '>'
					}
					else {}
				}
			} else if sym.info is SumType && (sym.info as SumType).is_anon {
				variant_names := sym.info.variants.map(t.shorten_user_defined_typenames(t.sym(it).name,
					import_aliases))
				res = '${variant_names.join('|')}'
			} else {
				res = t.shorten_user_defined_typenames(res, import_aliases)
			}
		}
		.generic_inst {
			info := sym.info as GenericInst
			res = t.shorten_user_defined_typenames(sym.name.all_before('<'), import_aliases)
			res += '<'
			for i, ctyp in info.concrete_types {
				res += t.type_to_str_using_aliases(ctyp, import_aliases)
				if i != info.concrete_types.len - 1 {
					res += ', '
				}
			}
			res += '>'
		}
		.void {
			if typ.has_flag(.optional) {
				res = '?'
				return res
			}
			if typ.has_flag(.result) {
				res = '!'
				return res
			}
			res = 'void'
			return res
		}
		.thread {
			rtype := sym.thread_info().return_type
			if rtype != 1 {
				res = 'thread ' + t.type_to_str_using_aliases(rtype, import_aliases)
			}
		}
		.alias, .any, .placeholder, .enum_ {
			res = t.shorten_user_defined_typenames(res, import_aliases)
			/*
			if res.ends_with('.byte') {
				res = 'u8'
			} else {
				res = t.shorten_user_defined_typenames(res, import_aliases)
			}
			*/
		}
		.aggregate {}
	}
	mut nr_muls := typ.nr_muls()
	if typ.has_flag(.shared_f) {
		nr_muls--
		res = 'shared ' + res
	}
	if typ.has_flag(.atomic_f) {
		nr_muls--
		res = 'atomic ' + res
	}
	if nr_muls > 0 && !typ.has_flag(.variadic) {
		res = strings.repeat(`&`, nr_muls) + res
	}
	if typ.has_flag(.optional) {
		res = '?$res'
	}
	if typ.has_flag(.result) {
		res = '!$res'
	}
	return res
}

fn (t Table) shorten_user_defined_typenames(originalname string, import_aliases map[string]string) string {
	mut res := originalname
	if t.cmod_prefix.len > 0 && res.starts_with(t.cmod_prefix) {
		// cur_mod.Type => Type
		res = res.replace_once(t.cmod_prefix, '')
	} else if res in import_aliases {
		res = import_aliases[res]
	} else {
		// FIXME: clean this case and remove the following if
		// because it is an hack to format well the type when
		// there is a []modul.name
		if res.contains('[]') {
			idx := res.index('.') or { -1 }
			return res[idx + 1..]
		}
		// types defined by the user
		// mod.submod.submod2.Type => submod2.Type
		mut parts := res.split('.')
		if parts.len > 1 {
			ind := parts.len - 2
			if t.is_fmt {
				// Rejoin the module parts for correct usage of aliases
				parts[ind] = parts[..ind + 1].join('.')
			}
			if parts[ind] in import_aliases {
				parts[ind] = import_aliases[parts[ind]]
			}

			res = parts[ind..].join('.')
		} else {
			res = parts[0]
		}
	}
	return res
}

[minify]
pub struct FnSignatureOpts {
	skip_receiver bool
	type_only     bool
}

pub fn (t &Table) fn_signature(func &Fn, opts FnSignatureOpts) string {
	return t.fn_signature_using_aliases(func, map[string]string{}, opts)
}

pub fn (t &Table) fn_signature_using_aliases(func &Fn, import_aliases map[string]string, opts FnSignatureOpts) string {
	mut sb := strings.new_builder(20)
	if !opts.skip_receiver {
		sb.write_string('fn ')
		// TODO write receiver
	}
	if !opts.type_only {
		sb.write_string(func.name)
	}
	sb.write_string('(')
	start := int(opts.skip_receiver)
	for i in start .. func.params.len {
		if i != start {
			sb.write_string(', ')
		}
		param := func.params[i]
		mut typ := param.typ
		if param.is_mut {
			if param.typ.is_ptr() {
				typ = typ.deref()
			}
			sb.write_string('mut ')
		}
		if !opts.type_only {
			sb.write_string(param.name)
			sb.write_string(' ')
		}
		styp := t.type_to_str_using_aliases(typ, import_aliases)
		if i == func.params.len - 1 && func.is_variadic {
			sb.write_string('...')
			sb.write_string(styp)
		} else {
			sb.write_string(styp)
		}
	}
	sb.write_string(')')
	if func.return_type != ast.void_type {
		sb.write_string(' ')
		sb.write_string(t.type_to_str_using_aliases(func.return_type, import_aliases))
	}
	return sb.str()
}

// Get the name of the complete quanlified name of the type
// without the generic parts.
pub fn (t &TypeSymbol) symbol_name_except_generic() string {
	// main.Abc<int>
	mut embed_name := t.name
	// remove generic part from name
	// main.Abc<int> => main.Abc
	if embed_name.contains('<') {
		embed_name = embed_name.all_before('<')
	}
	return embed_name
}

pub fn (t &TypeSymbol) embed_name() string {
	// main.Abc<int> => Abc<int>
	mut embed_name := t.name.split('.').last()
	// remove generic part from name
	// Abc<int> => Abc
	if embed_name.contains('<') {
		embed_name = embed_name.split('<')[0]
	}
	return embed_name
}

pub fn (t &TypeSymbol) has_method(name string) bool {
	for mut method in unsafe { t.methods } {
		if method.name == name {
			return true
		}
	}
	return false
}

pub fn (t &TypeSymbol) has_method_with_generic_parent(name string) bool {
	t.find_method_with_generic_parent(name) or { return false }
	return true
}

pub fn (t &TypeSymbol) find_method(name string) ?Fn {
	for mut method in unsafe { t.methods } {
		if method.name == name {
			return method
		}
	}
	return none
}

pub fn (t &TypeSymbol) find_method_with_generic_parent(name string) ?Fn {
	if m := t.find_method(name) {
		return m
	}
	mut table := global_table
	match t.info {
		Struct, Interface, SumType {
			if t.info.parent_type.has_flag(.generic) {
				parent_sym := table.sym(t.info.parent_type)
				if x := parent_sym.find_method(name) {
					match parent_sym.info {
						Struct, Interface, SumType {
							mut method := x
							generic_names := parent_sym.info.generic_types.map(table.sym(it).name)
							return_sym := table.sym(method.return_type)
							if return_sym.kind in [.struct_, .interface_, .sum_type] {
								method.return_type = table.unwrap_generic_type(method.return_type,
									generic_names, t.info.concrete_types)
							} else {
								if rt := table.resolve_generic_to_concrete(method.return_type,
									generic_names, t.info.concrete_types)
								{
									method.return_type = rt
								}
							}
							method.params = method.params.clone()
							for mut param in method.params {
								if pt := table.resolve_generic_to_concrete(param.typ,
									generic_names, t.info.concrete_types)
								{
									param.typ = pt
								}
							}
							return method
						}
						else {}
					}
				}
			}
		}
		else {}
	}
	return none
}

// is_js_compatible returns true if type can be converted to JS type and from JS type back to V type
pub fn (t &TypeSymbol) is_js_compatible() bool {
	mut table := global_table
	if t.kind == .void {
		return true
	}
	if t.kind == .function {
		return true
	}
	if t.language == .js || t.name.starts_with('JS.') {
		return true
	}
	match t.info {
		SumType {
			for variant in t.info.variants {
				sym := table.final_sym(variant)
				if !sym.is_js_compatible() {
					return false
				}
			}
			return true
		}
		else {
			return true
		}
	}
}

pub fn (t &TypeSymbol) str_method_info() (bool, bool, int) {
	mut has_str_method := false
	mut expects_ptr := false
	mut nr_args := 0
	if sym_str_method := t.find_method_with_generic_parent('str') {
		has_str_method = true
		nr_args = sym_str_method.params.len
		if nr_args > 0 {
			expects_ptr = sym_str_method.params[0].typ.is_ptr()
		}
	}
	return has_str_method, expects_ptr, nr_args
}

pub fn (t &TypeSymbol) find_field(name string) ?StructField {
	match t.info {
		Aggregate { return t.info.find_field(name) }
		Struct { return t.info.find_field(name) }
		Interface { return t.info.find_field(name) }
		SumType { return t.info.find_field(name) }
		else { return none }
	}
}

fn (a &Aggregate) find_field(name string) ?StructField {
	for mut field in unsafe { a.fields } {
		if field.name == name {
			return field
		}
	}
	return none
}

pub fn (i &Interface) find_field(name string) ?StructField {
	for mut field in unsafe { i.fields } {
		if field.name == name {
			return field
		}
	}
	return none
}

pub fn (i &Interface) find_method(name string) ?Fn {
	for mut method in unsafe { i.methods } {
		if method.name == name {
			return method
		}
	}
	return none
}

pub fn (i &Interface) has_method(name string) bool {
	for mut method in unsafe { i.methods } {
		if method.name == name {
			return true
		}
	}
	return false
}

pub fn (s Struct) find_field(name string) ?StructField {
	for mut field in unsafe { s.fields } {
		if field.name == name {
			return field
		}
	}
	return none
}

pub fn (s Struct) get_field(name string) StructField {
	if field := s.find_field(name) {
		return field
	}
	panic('unknown field `$name`')
}

pub fn (s &SumType) find_field(name string) ?StructField {
	for mut field in unsafe { s.fields } {
		if field.name == name {
			return field
		}
	}
	return none
}

pub fn (i Interface) defines_method(name string) bool {
	for mut method in unsafe { i.methods } {
		if method.name == name {
			return true
		}
	}
	return false
}

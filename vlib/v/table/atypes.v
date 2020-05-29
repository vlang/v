// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
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
module table

import strings

pub type Type int

pub type TypeInfo = Alias | Array | ArrayFixed | Enum | FnType | Interface | Map | MultiReturn |
	Struct | SumType

pub enum Language {
	v
	c
	js
}

pub struct TypeSymbol {
pub:
	parent_idx int
pub mut:
	info       TypeInfo
	kind       Kind
	name       string
	methods    []Fn
	mod        string
	is_public  bool
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
pub fn (t Type) idx() int {
	return u16(t) & 0xffff
}

// return nr_muls for `t`
[inline]
pub fn (t Type) nr_muls() int {
	return (int(t) >> 16) & 0xff
}

// return true if `t` is a pointer (nr_muls>0)
[inline]
pub fn (t Type) is_ptr() bool {
	return (int(t) >> 16) & 0xff > 0
}

// set nr_muls on `t` and return it
[inline]
pub fn (t Type) set_nr_muls(nr_muls int) Type {
	if nr_muls < 0 || nr_muls > 255 {
		panic('set_nr_muls: nr_muls must be between 0 & 255')
	}
	return (((int(t) >> 24) & 0xff) << 24) | (nr_muls << 16) | (u16(t) & 0xffff)
}

// increments nr_nuls on `t` and return it
[inline]
pub fn (t Type) to_ptr() Type {
	nr_muls := (int(t) >> 16) & 0xff
	if nr_muls == 255 {
		panic('to_ptr: nr_muls is already at max of 255')
	}
	return (((int(t) >> 24) & 0xff) << 24) | ((nr_muls + 1) << 16) | (u16(t) & 0xffff)
}

// decrement nr_muls on `t` and return it
[inline]
pub fn (t Type) deref() Type {
	nr_muls := (int(t) >> 16) & 0xff
	if nr_muls == 0 {
		panic('deref: type `$t` is not a pointer')
	}
	return (((int(t) >> 24) & 0xff) << 24) | ((nr_muls - 1) << 16) | (u16(t) & 0xffff)
}

// return the flag that is set on `t`
[inline]
pub fn (t Type) flag() TypeFlag {
	return (int(t) >> 24) & 0xff
}

// set the flag on `t` to `flag` and return it
[inline]
pub fn (t Type) set_flag(flag TypeFlag) Type {
	return (int(flag) << 24) | (((int(t) >> 16) & 0xff) << 16) | (u16(t) & 0xffff)
}

// return true if the flag set on `t` is `flag`
[inline]
pub fn (t Type) flag_is(flag TypeFlag) bool {
	return (int(t) >> 24) & 0xff == flag
}

// return new type with TypeSymbol idx set to `idx`
[inline]
pub fn new_type(idx int) Type {
	if idx < 1 || idx > 65536 {
		panic('new_type_id: idx must be between 1 & 65536')
	}
	return idx
}

// return new type with TypeSymbol idx set to `idx` & nr_muls set to `nr_muls`
[inline]
pub fn new_type_ptr(idx, nr_muls int) Type {
	if idx < 1 || idx > 65536 {
		panic('new_type_ptr: idx must be between 1 & 65536')
	}
	if nr_muls < 0 || nr_muls > 255 {
		panic('new_type_ptr: nr_muls must be between 0 & 255')
	}
	return (nr_muls << 16) | u16(idx)
}

// built in pointers (voidptr, byteptr, charptr)
[inline]
pub fn (typ Type) is_pointer() bool {
	return typ.idx() in pointer_type_idxs
}

[inline]
pub fn (typ Type) is_float() bool {
	return typ.idx() in float_type_idxs
}

[inline]
pub fn (typ Type) is_int() bool {
	return typ.idx() in integer_type_idxs
}

[inline]
pub fn (typ Type) is_signed() bool {
	return typ.idx() in signed_integer_type_idxs
}

[inline]
pub fn (typ Type) is_unsigned() bool {
	return typ.idx() in unsigned_integer_type_idxs
}

[inline]
pub fn (typ Type) is_any_int() bool {
	return typ.idx() == any_int_type_idx
}

[inline]
pub fn (typ Type) is_number() bool {
	return typ.idx() in number_type_idxs
}

pub const (
	void_type_idx    = 1
	voidptr_type_idx = 2
	byteptr_type_idx = 3
	charptr_type_idx = 4
	i8_type_idx      = 5
	i16_type_idx     = 6
	int_type_idx     = 7
	i64_type_idx     = 8
	byte_type_idx    = 9
	u16_type_idx     = 10
	u32_type_idx     = 11
	u64_type_idx     = 12
	f32_type_idx     = 13
	f64_type_idx     = 14
	char_type_idx    = 15
	bool_type_idx    = 16
	none_type_idx    = 17
	string_type_idx  = 18
	ustring_type_idx = 19
	array_type_idx   = 20
	map_type_idx     = 21
	any_type_idx     = 22
	t_type_idx       = 23
	any_flt_type_idx = 24
	any_int_type_idx = 25
)

pub const (
	integer_type_idxs          = [i8_type_idx, i16_type_idx, int_type_idx, i64_type_idx, byte_type_idx,
		u16_type_idx,
		u32_type_idx,
		u64_type_idx,
		any_int_type_idx
	]
	signed_integer_type_idxs   = [i8_type_idx, i16_type_idx, int_type_idx, i64_type_idx]
	unsigned_integer_type_idxs = [byte_type_idx, u16_type_idx, u32_type_idx, u64_type_idx]
	float_type_idxs            = [f32_type_idx, f64_type_idx, any_flt_type_idx]
	number_type_idxs           = [i8_type_idx, i16_type_idx, int_type_idx,
		i64_type_idx, byte_type_idx,
		u16_type_idx,
		u32_type_idx,
		u64_type_idx,
		f32_type_idx,
		f64_type_idx,
		any_int_type_idx,
		any_flt_type_idx
	]
	pointer_type_idxs          = [voidptr_type_idx, byteptr_type_idx, charptr_type_idx]
	string_type_idxs           = [string_type_idx, ustring_type_idx]
)

pub const (
	void_type    = new_type(void_type_idx)
	voidptr_type = new_type(voidptr_type_idx)
	byteptr_type = new_type(byteptr_type_idx)
	charptr_type = new_type(charptr_type_idx)
	i8_type      = new_type(i8_type_idx)
	int_type     = new_type(int_type_idx)
	i16_type     = new_type(i16_type_idx)
	i64_type     = new_type(i64_type_idx)
	byte_type    = new_type(byte_type_idx)
	u16_type     = new_type(u16_type_idx)
	u32_type     = new_type(u32_type_idx)
	u64_type     = new_type(u64_type_idx)
	f32_type     = new_type(f32_type_idx)
	f64_type     = new_type(f64_type_idx)
	char_type    = new_type(char_type_idx)
	bool_type    = new_type(bool_type_idx)
	none_type    = new_type(none_type_idx)
	string_type  = new_type(string_type_idx)
	ustring_type = new_type(ustring_type_idx)
	array_type   = new_type(array_type_idx)
	map_type     = new_type(map_type_idx)
	any_type     = new_type(any_type_idx)
	t_type       = new_type(t_type_idx)
	any_flt_type = new_type(any_flt_type_idx)
	any_int_type = new_type(any_int_type_idx)
)

pub const (
	builtin_type_names = ['void', 'voidptr', 'charptr', 'byteptr', 'i8', 'i16', 'int', 'i64',
		'u16',
		'u32',
		'u64', 'any_int', 'f32', 'f64', 'any_float', 'string', 'ustring', 'char', 'byte',
		'bool', 'none', 'array', 'array_fixed',
		'map', 'any', 'struct',
		'mapnode', 'size_t']
)

pub struct MultiReturn {
pub:
	name  string
pub mut:
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
	size_t
	bool
	none_
	string
	ustring
	array
	array_fixed
	map
	any
	struct_
	multi_return
	sum_type
	alias
	enum_
	function
	interface_
	any_float
	any_int
}

pub fn (t &TypeSymbol) str() string {
	return t.name.replace('array_', '[]')
}

[inline]
pub fn (t &TypeSymbol) enum_info() Enum {
	match t.info {
		Enum { return it }
		else { panic('TypeSymbol.enum_info(): no enum info for type: $t.name') }
	}
}

[inline]
pub fn (t &TypeSymbol) mr_info() MultiReturn {
	match t.info {
		MultiReturn { return it }
		else { panic('TypeSymbol.mr_info(): no multi return info for type: $t.name') }
	}
}

[inline]
pub fn (t &TypeSymbol) array_info() Array {
	match t.info {
		Array { return it }
		else { panic('TypeSymbol.array_info(): no array info for type: $t.name') }
	}
}

[inline]
pub fn (t &TypeSymbol) array_fixed_info() ArrayFixed {
	match t.info {
		ArrayFixed { return it }
		else { panic('TypeSymbol.array_fixed(): no array fixed info for type: $t.name') }
	}
}

[inline]
pub fn (t &TypeSymbol) map_info() Map {
	match t.info {
		Map { return it }
		else { panic('TypeSymbol.map_info(): no map info for type: $t.name') }
	}
}

[inline]
pub fn (t &TypeSymbol) struct_info() Struct {
	match t.info {
		Struct { return it }
		else { panic('TypeSymbol.struct_info(): no struct info for type: $t.name') }
	}
}

/*
pub fn (t TypeSymbol) str() string {
	return t.name
}
*/
pub fn (mut t Table) register_builtin_type_symbols() {
	// reserve index 0 so nothing can go there
	// save index check, 0 will mean not found
	t.register_type_symbol({
		kind: .placeholder
		name: 'reserved_0'
	})
	t.register_type_symbol(TypeSymbol{
		kind: .void
		name: 'void'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .voidptr
		name: 'voidptr'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .byteptr
		name: 'byteptr'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .charptr
		name: 'charptr'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .i8
		name: 'i8'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .i16
		name: 'i16'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .int
		name: 'int'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .i64
		name: 'i64'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .byte
		name: 'byte'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .u16
		name: 'u16'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .u32
		name: 'u32'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .u64
		name: 'u64'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .f32
		name: 'f32'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .f64
		name: 'f64'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .char
		name: 'char'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .bool
		name: 'bool'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .none_
		name: 'none'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .string
		name: 'string'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .ustring
		name: 'ustring'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .array
		name: 'array'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .map
		name: 'map'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .any
		name: 'any'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .any
		name: 'T'
		mod: 'builtin'
		is_public: true
	})
	t.register_type_symbol({
		kind: .any_float
		name: 'any_float'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .any_int
		name: 'any_int'
		mod: 'builtin'
	})
	t.register_type_symbol({
		kind: .size_t
		name: 'size_t'
		mod: 'builtin'
	})
	// TODO: remove. for v1 map compatibility
	map_string_string_idx := t.find_or_register_map(string_type, string_type)
	map_string_int_idx := t.find_or_register_map(string_type, int_type)
	t.register_type_symbol({
		kind: .alias
		name: 'map_string'
		mod: 'builtin'
		parent_idx: map_string_string_idx
	})
	t.register_type_symbol({
		kind: .alias
		name: 'map_int'
		mod: 'builtin'
		parent_idx: map_string_int_idx
	})
}

[inline]
pub fn (t &TypeSymbol) is_pointer() bool {
	return t.kind in [.byteptr, .charptr, .voidptr]
}

[inline]
pub fn (t &TypeSymbol) is_int() bool {
	return t.kind in [.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64, .any_int]
}

[inline]
pub fn (t &TypeSymbol) is_float() bool {
	return t.kind in [.f32, .f64, .any_float]
}

[inline]
pub fn (t &TypeSymbol) is_number() bool {
	return t.is_int() || t.is_float()
}

// for debugging/errors only, perf is not an issue
pub fn (k Kind) str() string {
	k_str := match k {
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
		.byte { 'byte' }
		.u16 { 'u16' }
		.u32 { 'u32' }
		.u64 { 'u64' }
		.any_int { 'any_int' }
		.f32 { 'f32' }
		.f64 { 'f64' }
		.any_float { 'any_float' }
		.string { 'string' }
		.char { 'char' }
		.bool { 'bool' }
		.size_t { 'size_t' }
		.none_ { 'none' }
		.array { 'array' }
		.array_fixed { 'array_fixed' }
		.map { 'map' }
		.multi_return { 'multi_return' }
		.sum_type { 'sum_type' }
		.alias { 'alias' }
		.enum_ { 'enum' }
		.any { 'any' }
		else { 'unknown' }
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
	fields      []Field
	is_typedef  bool // C. [typedef]
	is_union    bool
	is_ref_only bool
}

pub struct Interface {
pub mut:
	types []Type
}

pub struct Enum {
pub:
	vals    []string
	is_flag bool
}

pub struct Alias {
pub:
	parent_typ Type
	language   Language
}

// NB: FExpr here is a actually an ast.Expr .
// It should always be used by casting to ast.Expr, using ast.fe2ex()/ast.ex2fe()
// That hack is needed to break an import cycle between v.ast and v.table .
pub type FExpr = byteptr | voidptr

pub struct Field {
pub:
	name             string
pub mut:
	typ              Type
	default_expr     FExpr
	has_default_expr bool
	default_val      string
	attrs            []string
	is_pub           bool
	is_mut           bool
	is_global        bool
}

pub struct Array {
pub:
	nr_dims   int
pub mut:
	elem_type Type
}

pub struct ArrayFixed {
pub:
	nr_dims   int
	size      int
pub mut:
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

// TODO simplify this method
pub fn (table &Table) type_to_str(t Type) string {
	sym := table.get_type_symbol(t)
	mut res := sym.name
	if sym.kind == .multi_return {
		res = '('
		if t.flag_is(.optional) {
			res = '?' + res
		}
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
	if sym.kind == .array || 'array_' in res {
		res = res.replace('array_', '[]')
	}
	if sym.kind == .map || 'map_string_' in res {
		res = res.replace('map_string_', 'map[string]')
	}
	// mod.submod.submod2.Type => submod2.Type
	if res.contains('.') {
		vals := res.split('.')
		if vals.len > 2 {
			res = vals[vals.len - 2] + '.' + vals[vals.len - 1]
		}
		if sym.kind == .array && !res.starts_with('[]') {
			res = '[]' + res
		}
	}
	nr_muls := t.nr_muls()
	if nr_muls > 0 {
		res = strings.repeat(`&`, nr_muls) + res
	}
	if t.flag_is(.optional) {
		res = '?' + res
	}
	/*
	if res.starts_with(cur_mod +'.') {
	res = res[cur_mod.len+1.. ]
	}
	*/
	return res
}

pub fn (t &Table) fn_to_str(func &Fn) string {
	mut sb := strings.new_builder(20)
	sb.write('${func.name}(')
	for i in 1 .. func.args.len {
		arg := func.args[i]
		sb.write('$arg.name')
		if i == func.args.len - 1 || func.args[i + 1].typ != arg.typ {
			sb.write(' ${t.type_to_str(arg.typ)}')
		}
		if i != func.args.len - 1 {
			sb.write(', ')
		}
	}
	sb.write(')')
	if func.return_type != void_type {
		sb.write(' ${t.type_to_str(func.return_type)}')
	}
	return sb.str()
}

pub fn (t &TypeSymbol) has_method(name string) bool {
	t.find_method(name) or {
		return false
	}
	return true
}

pub fn (t &TypeSymbol) find_method(name string) ?Fn {
	for method in t.methods {
		if method.name == name {
			return method
		}
	}
	return none
}

pub fn (t &TypeSymbol) str_method_info() (bool, bool, int) {
	mut has_str_method := false
	mut expects_ptr := false
	mut nr_args := 0
	if sym_str_method := t.find_method('str') {
		has_str_method = true
		nr_args = sym_str_method.args.len
		if nr_args > 0 {
			expects_ptr = sym_str_method.args[0].typ.is_ptr()
		}
	}
	return has_str_method, expects_ptr, nr_args
}

pub fn (s Struct) find_field(name string) ?Field {
	for field in s.fields {
		if field.name == name {
			return field
		}
	}
	return none
}

pub fn (s Struct) get_field(name string) Field {
	if field := s.find_field(name) {
		return field
	}
	panic('unknown field `$name`')
}

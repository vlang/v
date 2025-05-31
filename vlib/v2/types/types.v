// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

import v2.ast

// TODO: fix nested sum type in tinyv (like TS)
pub type Type = Alias
	| Array
	| ArrayFixed
	| Channel
	| Char
	| Enum
	| FnType
	| ISize
	| Interface
	| Map
	| NamedType
	| Nil
	| None
	| OptionType
	| Pointer
	| Primitive
	| ResultType
	| Rune
	| String
	| Struct
	| SumType
	| Thread
	| Tuple
	| USize
	| Void

@[flag]
enum Properties {
	boolean
	float
	integer
	unsigned
	untyped
}

// TODO: decide if kind will be used or just properties
// enum PrimitiveKind {
// 	bool_
// 	i8_
// 	i16_
// 	// i32_
// 	int_
// 	i64_
// 	// u8_
// 	byte_
// 	u16_
// 	u32_
// 	u64_
// 	untyped_int
// 	untyped_float
// }

struct Primitive {
	// kind  PrimitiveKind
	props Properties
	size  u8
}

struct Alias {
	name string
mut:
	base_type Type
}

struct Array {
	elem_type Type
}

struct ArrayFixed {
	len       int
	elem_type Type
}

struct Channel {
	elem_type ?Type
}

struct Enum {
	// TODO: store attributes enum or bool?
	is_flag bool
	name    string
	fields  []Field
	// fields	map[string]Type
}

struct OptionType {
	base_type Type
}

struct Parameter {
	name   string
	typ    Type
	is_mut bool
}

struct ResultType {
	base_type Type
}

@[flag]
enum FnTypeAttribute {
	empty
	noreturn
}

fn FnTypeAttribute.from_ast_attributes(ast_attrs []ast.Attribute) FnTypeAttribute {
	mut attrs := FnTypeAttribute.empty
	for attr in ast_attrs {
		match attr.name {
			'noreturn' { attrs.set(.noreturn) }
			else {}
		}
	}
	return attrs
}

struct FnType {
	// generic_params []NamedType // T  ,Y
	generic_params []string // T  ,Y
	// TODO: save in checker.env? or gere?
	// I think I prefer checker env
	// generic_types  []Types  // int,int
	params      []Parameter
	return_type ?Type
	is_variadic bool
	attributes  FnTypeAttribute
mut:
	generic_types []map[string]Type
	// scope was originally used for deferred type checking
	// but its better if we dont need it here, although it may
	// be meeded for soething later im not thinking of??
	// 	scope          &Scope
}

struct Interface {
	name string
mut:
	fields []Field
	// fields map[string]Type
	// TODO:
}

struct Map {
	key_type   Type
	value_type Type
}

struct Pointer {
	base_type Type
}

// struct String {
// }

// Generic `T`, `Y` etc
type NamedType = string

// struct NamedType {
// 	name string
// }

struct Field {
	name string
	typ  Type
}

// struct Method {
// 	name string
// 	typ  FnType
// }

struct Struct {
	name           string
	generic_params []string
mut:
	embedded []Struct
	// embedded       []Type
	fields []Field
	// fields	 map[string]Type
	// methods 	   []Method
}

// TODO:
fn (t Struct) str() string {
	return 'Struct.str (${t.name()})'
}

// TODO: module not included in check
fn (a Struct) == (b Struct) bool {
	if a.name == b.name {
		return true
	}
	return false
}

struct SumType {
	name string
mut:
	variants []Type
}

struct Thread {
	elem_type ?Type
	// return_type Type
}

struct Tuple {
	types []Type
}

type Char = u8
type ISize = u8
type USize = u8
type Rune = u8
type String = u8

// type IntLiteral = u8
// type FloatLiteral = u8
type Void = u8
type Nil = u8
type None = u8

fn (t Type) base_type() Type {
	match t {
		// TODO: add base_type method
		Alias {
			return t.base_type
			// should we fully resolve all aliases, or just one level here?
			// return t.base_type.base_type()
		}
		OptionType, ResultType, Pointer {
			return t.base_type
		}
		// TODO: why as I doing this instead of else? to make it easy to find unhandled cases?
		Primitive, Array, ArrayFixed, Channel, Char, Enum, FnType, Interface, ISize, Map, Rune,
		String, Struct, SumType, Thread, Tuple, USize, Void, Nil, None, NamedType {
			return t
		}
		// else {
		// 	return t
		// }
	}
}

// return the key type used with for in loops
fn (t Type) key_type() Type {
	match t {
		Map { return t.key_type }
		// TODO: struct here is 'struct string', need to fix this.
		// we could use an alias? remove once fixed.
		// Array, ArrayFixed, String, Struct { return int_ }
		// else { panic('TODO: should never be called on ${t.type_name()}') }
		// TODO: see checker ForStmt -> ForInStmt when value is pointer
		else { return int_ }
	}
}

// return the value type used with for in loops
fn (t Type) value_type() Type {
	match t {
		Array, ArrayFixed { return t.elem_type }
		Channel { return t.elem_type or { t } } // TODO: ?
		Map { return t.value_type }
		Pointer { return t.base_type.value_type() }
		String { return u8_ }
		Thread { return t.elem_type or { t } } // TODO: ?
		OptionType, ResultType { return t.base_type.value_type() }
		else { return t.base_type() }
	}
}

// converts untyped constant / literal to its default type
// if is already typed then it returns itself
fn (t Type) typed_default() Type {
	// this handles int & float
	if t is Primitive && t.is_number_literal() {
		mut concrete_props := t.props
		concrete_props.clear(.untyped)
		// TODO: platform dependant size - see universe
		size := u8(if t.props.has(.float) { 32 } else { 0 })
		return Primitive{
			...t
			props: concrete_props
			size:  size
		}
	}
	return t
}

// unwraps option or result
fn (t Type) unwrap() Type {
	match t {
		OptionType, ResultType {
			return t.base_type
		}
		else {
			return t
		}
	}
}

fn (t Type) ref() Pointer {
	return Pointer{
		base_type: t
	}
}

fn (t Type) deref() Type {
	if t is Pointer {
		return t.base_type
	}
	panic('Type.deref(): ${t.name()} is not a pointer')
}

// TODO:
fn (t Type) is_compatible_with(t2 Type) bool {
	if t == t2 {
		return true
	} else if t is Alias {
		return t.base_type.is_compatible_with(t2)
	} else if t2 is Alias {
		return t.is_compatible_with(t2.base_type)
	}
	return false
}

fn (t Type) is_float() bool {
	if t is Primitive {
		return t.is_float()
	}
	return false
}

fn (t Type) is_integer() bool {
	if t is Primitive {
		return t.is_integer()
	}
	return false
}

fn (t Type) is_number() bool {
	if t is ISize {
		return true
	}
	if t is Primitive {
		return t.is_number()
	}
	return false
}

// int_literal || float_literal
fn (t Type) is_number_literal() bool {
	if t is Primitive {
		return t.is_number_literal()
	}
	return false
}

fn (t Type) is_float_literal() bool {
	if t is Primitive {
		return t.is_float_literal()
	}
	return false
}

fn (t Type) is_int_literal() bool {
	if t is Primitive {
		return t.is_int_literal()
	}
	return false
}

fn (t Primitive) is_float() bool {
	return t.props.has(.float)
}

fn (t Primitive) is_integer() bool {
	return t.props.has(.integer)
}

fn (t Primitive) is_number() bool {
	// TODO: should we make sure is not .untyped
	return !t.props.has(.untyped) && t.props.has(.integer | .float)
}

fn (t Primitive) is_number_literal() bool {
	return t.props.has(.untyped) && t.props.has(.integer | .float)
}

fn (t Primitive) is_float_literal() bool {
	return t.props.has(.untyped) && t.props.has(.float)
}

fn (t Primitive) is_int_literal() bool {
	return t.props.has(.untyped) && t.props.has(.integer)
}

fn (t Type) name() string {
	match t {
		Primitive, Alias, Array, ArrayFixed, Channel, Char, Enum, FnType, Interface, ISize, Map,
		OptionType, Pointer, ResultType, Rune, String, Struct, SumType, Thread, Tuple, USize, Void,
		Nil, None, NamedType {
			return t.name()
		}
	}
}

// TODO: clean up :0
fn (t Primitive) name() string {
	if t.props.has(.boolean) {
		return 'bool'
	} else if t.props.has(.untyped) {
		if t.props.has(.integer) {
			return 'int_literal'
		} else if t.props.has(.float) {
			return 'float_literal'
		}
	} else if t.props.has(.integer) {
		if t.props.has(.unsigned) {
			return 'u${t.size}'
		} else {
			// TODO:
			if t.size == 0 {
				return 'int'
			}
			return 'i${t.size}'
		}
	} else if t.props.has(.float) {
		return 'f${t.size}'
	}
	println(t)
	panic(t.props.str())
	return 'malformed primitive' // lol
	// TODO: match seems broke when multuple flags are set
	// actually it was not broken, it matches the bits for flags
	// matches single only, if multiple are set it will not match.
	//
	// match t.props {
	// 	.boolean {
	// 		return 'bool'
	// 	}
	// 	.integer {
	// 		if t.props.has(.unsigned) {
	// 			return 'u$t.size'
	// 		} else {
	// 			if t.size == 32 {
	// 				return 'int'
	// 			}
	// 			return 'i$t.size'
	// 		}
	// 	}
	// 	.float {
	// 		return 'f$t.size'
	// 	}
	// 	else {
	// 		println(t)
	// 		panic(t.props.str())
	// 		return 'malformed primitive' // lol
	// 	}
	// }		
}

fn (t Alias) name() string {
	return t.name
}

fn (t Array) name() string {
	return '[]${t.elem_type.name()}'
}

fn (t ArrayFixed) name() string {
	return '[${t.len}]${t.elem_type.name()}'
}

fn (t Channel) name() string {
	if elem_type := t.elem_type {
		return 'chan ${elem_type.name()}'
	}
	return 'chan'
}

fn (t Char) name() string {
	return 'char'
}

fn (t Enum) name() string {
	return t.name
}

fn (t FnType) name() string {
	mut name := 'fn ('
	for i, param in t.params {
		if param.name != '' {
			name += '${param.name} '
		}
		name += param.typ.name()
		if i < t.params.len - 1 {
			name += ', '
		}
	}
	mut return_type_name := ''
	if rt := t.return_type {
		return_type_name = rt.name()
	}
	name += ') ${return_type_name}'
	return name
}

fn (t Interface) name() string {
	return t.name
}

fn (t Map) name() string {
	return 'map[${t.key_type.name()}]${t.value_type.name()}'
}

fn (t NamedType) name() string {
	return t
}

fn (t OptionType) name() string {
	return '?' + t.base_type.name()
}

fn (t Pointer) name() string {
	return '&' + t.base_type.name()
}

fn (t ResultType) name() string {
	return '!' + t.base_type.name()
}

fn (t Rune) name() string {
	return 'rune'
}

fn (t String) name() string {
	return 'string'
}

fn (t Struct) name() string {
	return t.name
}

fn (t SumType) name() string {
	return t.name
}

fn (t Thread) name() string {
	return 'thread'
}

fn (t Tuple) name() string {
	// TODO: use faster method
	types_str := t.types.map(|typ| typ.name()).join(', ')
	return 'tuple (${types_str})'
}

fn (t ISize) name() string {
	return 'isize'
}

fn (t USize) name() string {
	return 'usize'
}

// fn (t IntLiteral) name() string {
// 	return 'int_literal'
// }

// fn (t FloatLiteral) name() string {
// 	return 'float_literal'
// }

fn (t Void) name() string {
	return 'void'
}

fn (t Nil) name() string {
	return 'nil'
}

fn (t None) name() string {
	return 'none'
}

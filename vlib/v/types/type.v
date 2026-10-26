module types

import v.flat

// Type aliases type values used by types.
pub type Type = Void
	| Unknown
	| Primitive
	| String
	| Char
	| Rune
	| ISize
	| USize
	| Nil
	| None
	| Array
	| ArrayFixed
	| Channel
	| Map
	| Pointer
	| FnType
	| OptionType
	| ResultType
	| Struct
	| Interface
	| Enum
	| SumType
	| Alias
	| MultiReturn

// Void represents void data used by types.
pub struct Void {
	dummy_ u8
}

// Unknown represents unknown data used by types.
pub struct Unknown {
	UnknownDataRef = empty_unknown_data
}

// == compares Unknown payloads by value.
// Cast payload references to compare addresses before inspecting their fields.
@[inline]
pub fn (a Unknown) == (b Unknown) bool {
	return voidptr(a.UnknownDataRef) == voidptr(b.UnknownDataRef) || *a.UnknownDataRef == *b.UnknownDataRef
}

const empty_unknown_data = &UnknownData{}

// UnknownDataRef references the immutable payload of Unknown.
pub type UnknownDataRef = &UnknownData

// UnknownData stores the immutable fields of Unknown.
pub struct UnknownData {
pub:
	reason string
}

// String represents string data used by types.
pub struct String {
	dummy_ u8
}

// Char represents char data used by types.
pub struct Char {
	dummy_ u8
}

// Rune represents rune data used by types.
pub struct Rune {
	dummy_ u8
}

// ISize represents isize data used by types.
pub struct ISize {
	dummy_ u8
}

// USize represents uint data used by types.
pub struct USize {
	dummy_ u8
}

// Nil represents nil data used by types.
pub struct Nil {
	dummy_ u8
}

// None represents none data used by types.
pub struct None {
	dummy_ u8
}

// Properties lists properties values used by types.
@[flag]
pub enum Properties {
	boolean
	float
	integer
	unsigned
	untyped
}

// Primitive represents primitive data used by types.
pub struct Primitive {
pub:
	props Properties
	size  u8
}

// Array represents array data used by types.
pub struct Array {
pub:
	elem_type &Type = empty_type
}

// ArrayFixed represents array fixed data used by types.
pub struct ArrayFixed {
	ArrayFixedDataRef = empty_array_fixed_data
}

// == compares ArrayFixed payloads by value.
@[inline]
pub fn (a ArrayFixed) == (b ArrayFixed) bool {
	return voidptr(a.ArrayFixedDataRef) == voidptr(b.ArrayFixedDataRef) || *a.ArrayFixedDataRef == *b.ArrayFixedDataRef
}

const empty_array_fixed_data = &ArrayFixedData{}

// ArrayFixedDataRef references the immutable payload of ArrayFixed.
pub type ArrayFixedDataRef = &ArrayFixedData

// ArrayFixedData stores the immutable fields of ArrayFixed.
pub struct ArrayFixedData {
pub:
	elem_type &Type = empty_type
	len       int
	len_expr  string
}

// Channel represents channel data used by types.
pub struct Channel {
	ChannelDataRef = empty_channel_data
}

// == compares Channel payloads by value.
@[inline]
pub fn (a Channel) == (b Channel) bool {
	return voidptr(a.ChannelDataRef) == voidptr(b.ChannelDataRef) || *a.ChannelDataRef == *b.ChannelDataRef
}

const empty_channel_data = &ChannelData{}

// ChannelDataRef references the immutable payload of Channel.
pub type ChannelDataRef = &ChannelData

// ChannelData stores the immutable fields of Channel.
pub struct ChannelData {
pub:
	elem_type &Type = empty_type
	is_mut    bool
}

// Map represents map data used by types.
pub struct Map {
	MapDataRef = empty_map_data
}

// == compares Map payloads by value.
@[inline]
pub fn (a Map) == (b Map) bool {
	return voidptr(a.MapDataRef) == voidptr(b.MapDataRef) || *a.MapDataRef == *b.MapDataRef
}

const empty_map_data = &MapData{}

// MapDataRef references the immutable payload of Map.
pub type MapDataRef = &MapData

// MapData stores the immutable fields of Map.
pub struct MapData {
pub:
	key_type   &Type = empty_type
	value_type &Type = empty_type
}

// Pointer represents pointer data used by types.
pub struct Pointer {
pub:
	base_type &Type = empty_type
}

// FnType represents fn type data used by types.
pub struct FnType {
	FnTypeDataRef = empty_fn_type_data
}

// == compares FnType payloads by value.
@[inline]
pub fn (a FnType) == (b FnType) bool {
	return voidptr(a.FnTypeDataRef) == voidptr(b.FnTypeDataRef) || *a.FnTypeDataRef == *b.FnTypeDataRef
}

const empty_fn_type_data = &FnTypeData{}

// FnTypeDataRef references the immutable payload of FnType.
pub type FnTypeDataRef = &FnTypeData

// FnTypeData stores the immutable fields of FnType.
pub struct FnTypeData {
pub:
	params      []FnParam
	return_type &Type = empty_type
	is_variadic bool // The final array parameter accepts zero or more values.
}

pub struct FnParam {
pub:
	typ    &Type = empty_type
	is_mut bool
}

// OptionType represents option type data used by types.
pub struct OptionType {
pub:
	base_type &Type = empty_type
}

// ResultType represents result type data used by types.
pub struct ResultType {
pub:
	base_type &Type = empty_type
}

// Struct represents struct data used by types.
pub struct Struct {
pub:
	name_id u32
}

// Interface represents interface data used by types.
pub struct Interface {
pub:
	name_id u32
}

// Enum represents enum data used by types.
pub struct Enum {
pub:
	name_id u32
	is_flag bool
}

// SumType represents sum type data used by types.
pub struct SumType {
pub:
	name_id u32
}

// Alias represents alias data used by types.
pub struct Alias {
	AliasDataRef = empty_alias_data
}

// == compares Alias payloads by value.
@[inline]
pub fn (a Alias) == (b Alias) bool {
	return voidptr(a.AliasDataRef) == voidptr(b.AliasDataRef) || *a.AliasDataRef == *b.AliasDataRef
}

const empty_alias_data = &AliasData{}

// AliasDataRef references the immutable payload of Alias.
pub type AliasDataRef = &AliasData

// AliasData stores the immutable fields of Alias.
pub struct AliasData {
pub:
	name_id   u32
	base_type &Type = empty_type
}

// MultiReturn represents multi return data used by types.
pub struct MultiReturn {
	MultiReturnDataRef = empty_multi_return_data
}

// == compares MultiReturn payloads by value.
@[inline]
pub fn (a MultiReturn) == (b MultiReturn) bool {
	return voidptr(a.MultiReturnDataRef) == voidptr(b.MultiReturnDataRef) || *a.MultiReturnDataRef == *b.MultiReturnDataRef
}

const empty_multi_return_data = &MultiReturnData{}

// MultiReturnDataRef references the immutable payload of MultiReturn.
pub type MultiReturnDataRef = &MultiReturnData

// MultiReturnData stores the immutable fields of MultiReturn.
pub struct MultiReturnData {
pub:
	types []types.Type
}

// new_type_reference allocates a shallow copy of value with stable reference storage.
// Nested metadata remains borrowed; clone_owned_type copies it when its arena will end.
pub fn new_type_reference(value Type) &Type {
	return &value
}

// clone_owned_type clones a type and all nested owned metadata.
pub fn clone_owned_type(value &Type) Type {
	return match value {
		Void {
			Type(void_)
		}
		Unknown {
			Type(Unknown{
				reason: value.reason.clone()
			})
		}
		Primitive {
			Type(Primitive{
				props: value.props
				size:  value.size
			})
		}
		String {
			Type(string_)
		}
		Char {
			Type(char_)
		}
		Rune {
			Type(rune_)
		}
		ISize {
			Type(isize_)
		}
		USize {
			Type(usize_)
		}
		Nil {
			Type(nil_)
		}
		None {
			Type(none_)
		}
		Array {
			Type(Array{
				elem_type: new_type_reference(Type(clone_owned_type(value.elem_type)))
			})
		}
		ArrayFixed {
			Type(ArrayFixed{
				elem_type: new_type_reference(Type(clone_owned_type(value.elem_type)))
				len:       value.len
				len_expr:  value.len_expr.clone()
			})
		}
		Channel {
			Type(Channel{
				elem_type: new_type_reference(Type(clone_owned_type(value.elem_type)))
				is_mut:    value.is_mut
			})
		}
		Map {
			Type(Map{
				key_type:   new_type_reference(Type(clone_owned_type(value.key_type)))
				value_type: new_type_reference(Type(clone_owned_type(value.value_type)))
			})
		}
		Pointer {
			Type(Pointer{
				base_type: new_type_reference(Type(clone_owned_type(value.base_type)))
			})
		}
		FnType {
			mut params := []FnParam{cap: value.params.len}
			for param in value.params {
				params << FnParam{
					typ:    new_type_reference(Type(clone_owned_type(param.typ)))
					is_mut: param.is_mut
				}
			}
			Type(FnType{
				params:      params
				is_variadic: value.is_variadic
				return_type: new_type_reference(Type(clone_owned_type(value.return_type)))
			})
		}
		OptionType {
			Type(OptionType{
				base_type: new_type_reference(Type(clone_owned_type(value.base_type)))
			})
		}
		ResultType {
			Type(ResultType{
				base_type: new_type_reference(Type(clone_owned_type(value.base_type)))
			})
		}
		Struct {
			Type(Struct{
				name_id: value.name_id
			})
		}
		Interface {
			Type(Interface{
				name_id: value.name_id
			})
		}
		Enum {
			Type(Enum{
				name_id: value.name_id
				is_flag: value.is_flag
			})
		}
		SumType {
			Type(SumType{
				name_id: value.name_id
			})
		}
		Alias {
			Type(Alias{
				name_id:   value.name_id
				base_type: new_type_reference(Type(clone_owned_type(value.base_type)))
			})
		}
		MultiReturn {
			Type(MultiReturn{
				types: clone_owned_types(value.types)
			})
		}
	}
}

// clone_owned_types clones a list of types and all nested owned metadata.
pub fn clone_owned_types(values []Type) []Type {
	mut cloned := []Type{cap: values.len}
	for i in 0 .. values.len {
		cloned << clone_owned_type(&values[i])
	}
	return cloned
}

// StructField represents struct field data used by types.
pub struct StructField {
pub:
	name        string
	typ         Type
	has_default bool
	is_embed    bool
	is_mut      bool
	is_volatile bool
}

// unwrap_pointer transforms unwrap pointer data for types.
pub fn unwrap_pointer(t &Type) Type {
	if t is Pointer {
		return *t.base_type
	}
	return *t
}

// unwrap_all_pointers removes every pointer layer from t.
pub fn unwrap_all_pointers(t &Type) Type {
	mut clean := t
	for clean is Pointer {
		clean = clean.base_type
	}
	return clean
}

// generic_base_name returns the declaration part of a concrete generic type name.
pub fn generic_base_name(name string) string {
	if name.starts_with('[') {
		return name
	}
	idx := name.index_u8(`[`)
	if idx > 0 {
		return name[..idx]
	}
	return name
}

// generic_base_type_id returns the declaration spelling of a concrete generic type.
// Array spellings and names without generic arguments retain their original ID.
pub fn generic_base_type_id(name_id u32) u32 {
	shape := flat.source_type_shape(name_id)
	if !shape.has(.bracketed) || shape.has(.leading_bracket) {
		return name_id
	}
	return flat.source_type_syntax(name_id).generic_base
}

// is_pointer reports whether is pointer applies in types.
@[inline]
pub fn (t &Type) is_pointer() bool {
	return t is Pointer
}

// is_string reports whether is string applies in types.
@[inline]
pub fn (t &Type) is_string() bool {
	return t is String
}

// is_integer reports whether is integer applies in types.
@[inline]
pub fn (t &Type) is_integer() bool {
	if t is Primitive {
		return t.props.has(.integer)
	}
	return t is Rune || t is ISize || t is USize
}

// unsigned_shift_result_type returns the unsigned counterpart used as the result of `>>>`.
pub fn unsigned_shift_result_type(t Type) Type {
	if t is Alias {
		return unsigned_shift_result_type(t.base_type)
	}
	if t is Primitive {
		if !t.props.has(.integer) || t.props.has(.unsigned) {
			return t
		}
		return match t.size {
			8 { Type(u8_) }
			16 { Type(u16_) }
			0 {
				if platform_int_bits() == 64 { Type(u64_) } else { Type(u32_) }
			}
			64 { Type(u64_) }
			128 { Type(u128_) }
			else { Type(u32_) }
		}
	}
	if t is Rune {
		return Type(u32_)
	}
	if t is ISize {
		return Type(usize_)
	}
	return t
}

// is_float reports whether is float applies in types.
@[inline]
pub fn (t &Type) is_float() bool {
	if t is Primitive {
		return t.props.has(.float)
	}
	return false
}

// name returns name data for Type.
@[inline]
pub fn (t &Type) name() string {
	return match t {
		Void { 'void' }
		Struct, Alias, Interface, Enum, SumType { flat.source_type_name(t.name_id) }
		Primitive { prim_name_from(t.props, t.size) }
		String { 'string' }
		Unknown { 'unknown' }
		Nil { 'nil' }
		None { 'none' }
		Char { 'char' }
		Rune { 'rune' }
		ISize { 'isize' }
		USize { 'uint' }
		else { flat.source_type_name(t.composite_source_type_id()) }
	}
}

@[direct_array_access]
fn (t &Type) composite_source_type_id() u32 {
	if t is Pointer {
		if t.base_type is Void {
			return flat.source_type_voidptr
		}
		return flat.source_type_composition_one(.pointer, t.base_type.source_type_id())
	}
	if t is Array {
		return flat.source_type_composition_one(.array, t.elem_type.source_type_id())
	}
	if t is OptionType {
		return flat.source_type_composition_one(.option, t.base_type.source_type_id())
	}
	if t is ResultType {
		return flat.source_type_composition_one(.result, t.base_type.source_type_id())
	}
	if t is FnType {
		count := t.params.len + if t.return_type is Void { 0 } else { 1 }
		mut small := [16]u32{}
		// This borrowed stack view cannot escape: the composition cache owns its keys.
		mut parts := if count <= small.len {
			unsafe { []u32(array{ data: &small[0], len: count, cap: small.len, element_size: sizeof(u32), flags: .nogrow | .nofree }) }
		} else {
			[]u32{len: count}
		}
		for index, param in t.params {
			mut part := u32(0)
			if param.is_mut && param.typ is Pointer {
				part = param.typ.base_type.source_type_id()
			} else if t.is_variadic && index == t.params.len - 1 && param.typ is Array {
				part = flat.source_type_composition_one(.variadic_parameter, param.typ.elem_type.source_type_id())
			} else {
				part = param.typ.source_type_id()
			}
			if param.is_mut {
				part = flat.source_type_composition_one(.mutable_parameter, part)
			}
			parts[index] = part
		}
		if t.return_type is Void {
			return flat.source_type_composition(.void_function_type, parts)
		}
		parts[t.params.len] = t.return_type.source_type_id()
		return flat.source_type_composition(.function_type, parts)
	}
	if t is Map {
		return flat.source_type_composition_two(.map, t.key_type.source_type_id(), t.value_type.source_type_id())
	}
	if t is ArrayFixed {
		// Keep the canonical prefix form: ?[3]u8 and [3]?u8 have different nesting.
		return flat.source_type_fixed_array(t.len, t.len_expr, t.elem_type.source_type_id(), false)
	}
	if t is Channel {
		if t.is_mut && t.elem_type is Pointer {
			return flat.source_type_composition_one(.mutable_channel, t.elem_type.base_type.source_type_id())
		}
		return flat.source_type_composition_one(.channel, t.elem_type.source_type_id())
	}
	if t is MultiReturn {
		mut small := [16]u32{}
		mut parts := if t.types.len <= small.len {
			// The cache consumes this view before the stack buffer expires.
			unsafe { []u32(array{ data: &small[0], len: t.types.len, cap: small.len, element_size: sizeof(u32), flags: .nogrow | .nofree }) }
		} else {
			[]u32{len: t.types.len}
		}
		for index, typ in t.types {
			parts[index] = typ.source_type_id()
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	return 0
}

// prim_name_from supports prim name from handling for types.
fn prim_name_from(props Properties, size u8) string {
	if props.has(.boolean) {
		return 'bool'
	}
	if props.has(.integer) {
		if props.has(.unsigned) {
			return match size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${size}' }
			}
		}
		return match size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${size}' }
		}
	}
	if props.has(.float) {
		return match size {
			32 { 'f32' }
			64 { 'f64' }
			else { 'f${size}' }
		}
	}
	return 'int'
}

fn primitive_source_type_id(props Properties, size u8) u32 {
	if props.has(.boolean) {
		return flat.source_type_bool
	}
	if props.has(.integer) {
		if props.has(.unsigned) {
			return match size {
				8 { flat.source_type_u8 }
				16 { flat.source_type_u16 }
				32 { flat.source_type_u32 }
				64 { flat.source_type_u64 }
				else { flat.source_type_id('u${size}') }
			}
		}
		return match size {
			0 { flat.source_type_int }
			8 { flat.source_type_i8 }
			16 { flat.source_type_i16 }
			32 { flat.source_type_i32 }
			64 { flat.source_type_i64 }
			else { flat.source_type_id('i${size}') }
		}
	}
	if props.has(.float) {
		return match size {
			32 { flat.source_type_f32 }
			64 { flat.source_type_f64 }
			else { flat.source_type_id('f${size}') }
		}
	}
	return flat.source_type_int
}

// prim_name supports prim name handling for types.
fn prim_name(t Primitive) string {
	if t.props.has(.boolean) {
		return 'bool'
	}
	if t.props.has(.integer) {
		if t.props.has(.unsigned) {
			return match t.size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${t.size}' }
			}
		}
		return match t.size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${t.size}' }
		}
	}
	if t.props.has(.float) {
		return match t.size {
			32 { 'f32' }
			64 { 'f64' }
			else { 'f${t.size}' }
		}
	}
	return 'int'
}

pub const empty_type = &builtin_void_type

pub fn (a Type) == (b Type) bool {
	return semantic_types_equal(a, b)
}

// name returns the canonical spelling of this nominal type.
@[inline]
pub fn (value Alias) name() string {
	return flat.source_type_name(value.name_id)
}

// name returns the canonical spelling of this nominal type.
@[inline]
pub fn (value Enum) name() string {
	return flat.source_type_name(value.name_id)
}

// name returns the canonical spelling of this nominal type.
@[inline]
pub fn (value Interface) name() string {
	return flat.source_type_name(value.name_id)
}

// name returns the canonical spelling of this nominal type.
@[inline]
pub fn (value Struct) name() string {
	return flat.source_type_name(value.name_id)
}

// name returns the canonical spelling of this nominal type.
@[inline]
pub fn (value SumType) name() string {
	return flat.source_type_name(value.name_id)
}

// source_type_id returns an interned spelling, reusing nominal type identities directly.
@[inline]
pub fn (t &Type) source_type_id() u32 {
	return match t {
		Struct, Alias, Enum, Interface, SumType { t.name_id }
		Void { flat.source_type_void }
		Unknown { flat.source_type_unknown }
		Nil { flat.source_type_nil }
		None { flat.source_type_none }
		String { flat.source_type_string }
		Char { flat.source_type_char }
		Rune { flat.source_type_rune }
		ISize { flat.source_type_isize }
		USize { flat.source_type_uint }
		Primitive { primitive_source_type_id(t.props, t.size) }
		else { t.composite_source_type_id() }
	}
}

// source_type_id returns this nominal type's interned spelling.
@[inline]
pub fn (value Struct) source_type_id() u32 {
	return value.name_id
}

// source_type_id returns this nominal type's interned spelling.
@[inline]
pub fn (value Alias) source_type_id() u32 {
	return value.name_id
}

// source_type_id returns this nominal type's interned spelling.
@[inline]
pub fn (value Enum) source_type_id() u32 {
	return value.name_id
}

// source_type_id returns this nominal type's interned spelling.
@[inline]
pub fn (value Interface) source_type_id() u32 {
	return value.name_id
}

// source_type_id returns this nominal type's interned spelling.
@[inline]
pub fn (value SumType) source_type_id() u32 {
	return value.name_id
}

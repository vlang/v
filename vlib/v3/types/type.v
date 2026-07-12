module types

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

// USize represents usize data used by types.
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
	elem_type Type
}

// ArrayFixed represents array fixed data used by types.
pub struct ArrayFixed {
pub:
	elem_type Type
	len       int
	len_expr  string
}

// Channel represents channel data used by types.
pub struct Channel {
pub:
	elem_type Type
}

// Map represents map data used by types.
pub struct Map {
pub:
	key_type   Type
	value_type Type
}

// Pointer represents pointer data used by types.
pub struct Pointer {
pub:
	base_type Type
}

// FnType represents fn type data used by types.
pub struct FnType {
pub:
	params      []Type
	return_type Type
}

// OptionType represents option type data used by types.
pub struct OptionType {
pub:
	base_type Type
}

// ResultType represents result type data used by types.
pub struct ResultType {
pub:
	base_type Type
}

// Struct represents struct data used by types.
pub struct Struct {
pub:
	name string
}

// Interface represents interface data used by types.
pub struct Interface {
pub:
	name string
}

// Enum represents enum data used by types.
pub struct Enum {
pub:
	name    string
	is_flag bool
}

// SumType represents sum type data used by types.
pub struct SumType {
pub:
	name string
}

// Alias represents alias data used by types.
pub struct Alias {
pub:
	name      string
	base_type Type
}

// MultiReturn represents multi return data used by types.
pub struct MultiReturn {
pub:
	types []Type
}

// StructField represents struct field data used by types.
pub struct StructField {
pub:
	name string
	typ  Type
}

// unwrap_pointer transforms unwrap pointer data for types.
pub fn unwrap_pointer(t Type) Type {
	if t is Pointer {
		return t.base_type
	}
	return t
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

// is_pointer reports whether is pointer applies in types.
pub fn (t Type) is_pointer() bool {
	return t is Pointer
}

// is_string reports whether is string applies in types.
pub fn (t Type) is_string() bool {
	return t is String
}

// is_integer reports whether is integer applies in types.
pub fn (t Type) is_integer() bool {
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
			64 { Type(u64_) }
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
pub fn (t Type) is_float() bool {
	if t is Primitive {
		return t.props.has(.float)
	}
	return false
}

// name returns name data for Type.
pub fn (t Type) name() string {
	if t is Void {
		return 'void'
	}
	if t is Unknown {
		return 'unknown'
	}
	if t is Nil {
		return 'nil'
	}
	if t is None {
		return 'none'
	}
	if t is String {
		return 'string'
	}
	if t is Char {
		return 'char'
	}
	if t is Rune {
		return 'rune'
	}
	if t is ISize {
		return 'isize'
	}
	if t is USize {
		return 'usize'
	}
	if t is Primitive {
		return prim_name_from(t.props, t.size)
	}
	if t is Array {
		return '[]${nested_type_name(t.elem_type)}'
	}
	if t is ArrayFixed {
		mut len_text := t.len.str()
		if t.len_expr.len > 0 {
			len_text = t.len_expr
		}
		return '${nested_type_name(t.elem_type)}[${len_text}]'
	}
	if t is Channel {
		return 'chan ${nested_type_name(t.elem_type)}'
	}
	if t is Map {
		return 'map[${nested_type_name(t.key_type)}]${nested_type_name(t.value_type)}'
	}
	if t is Pointer {
		return '&${nested_type_name(t.base_type)}'
	}
	if t is FnType {
		mut s := 'fn('
		for i in 0 .. t.params.len {
			if i > 0 {
				s += ', '
			}
			s += nested_type_name(fn_type_param_type(t, i))
		}
		s += ')'
		if t.return_type !is Void {
			s += ' ${nested_type_name(t.return_type)}'
		}
		return s
	}
	if t is OptionType {
		return '?${nested_type_name(t.base_type)}'
	}
	if t is ResultType {
		return '!${nested_type_name(t.base_type)}'
	}
	if t is Struct {
		return t.name
	}
	if t is Interface {
		return t.name
	}
	if t is Enum {
		return t.name
	}
	if t is SumType {
		return t.name
	}
	if t is Alias {
		return t.name
	}
	if t is MultiReturn {
		mut parts := []string{}
		for i in 0 .. t.types.len {
			parts << nested_type_name(t.types[i])
		}
		return '(${parts.join(', ')})'
	}
	return ''
}

// nested_type_name supports nested type name handling for types.
fn nested_type_name(t Type) string {
	return t.name()
}

// fn_type_param_type supports fn type param type handling for types.
fn fn_type_param_type(f FnType, idx int) Type {
	return f.params[idx]
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

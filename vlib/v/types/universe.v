@[has_globals]
module types

// platform_int_c_type is the C spelling emitted for V's platform-width `int`
// (the `Primitive` with size 0). `int` is 64-bit on 64-bit targets and 32-bit
// on 32-bit targets, so it lowers to `i64` or `i32` respectively. It defaults to
// the 64-bit spelling so self-host and the C-backend unit tests work before a
// target is configured; `set_platform_int_bits` updates it for cross builds.
__global platform_int_c_type = 'i64'

// set_platform_int_bits selects the C spelling for the platform `int` from the
// target pointer width. The driver calls this once, before checking or code
// generation, so every `c_type` lowering agrees on the width.
pub fn set_platform_int_bits(bits int) {
	platform_int_c_type = if bits == 32 { 'i32' } else { 'i64' }
}

// platform_int_bits returns the bit width of V's platform `int` (64 or 32),
// used for literal-overflow and range checks so they match the emitted width.
pub fn platform_int_bits() int {
	return if platform_int_c_type == 'i64' { 64 } else { 32 }
}

pub const bool_ = Primitive{
	props: .boolean
}
pub const int_ = Primitive{
	props: .integer
}
pub const i8_ = Primitive{
	props: .integer
	size:  8
}
pub const i16_ = Primitive{
	props: .integer
	size:  16
}
pub const i32_ = Primitive{
	props: .integer
	size:  32
}
pub const i64_ = Primitive{
	props: .integer
	size:  64
}
pub const u8_ = Primitive{
	props: .integer | .unsigned
	size:  8
}
pub const u16_ = Primitive{
	props: .integer | .unsigned
	size:  16
}
pub const u32_ = Primitive{
	props: .integer | .unsigned
	size:  32
}
pub const u64_ = Primitive{
	props: .integer | .unsigned
	size:  64
}
pub const i128_ = Primitive{
	props: .integer
	size:  128
}
pub const u128_ = Primitive{
	props: .integer | .unsigned
	size:  128
}
pub const f32_ = Primitive{
	props: .float
	size:  32
}
pub const f64_ = Primitive{
	props: .float
	size:  64
}
pub const string_ = String{}
pub const char_ = Char{}
pub const rune_ = Rune{}
pub const isize_ = ISize{}
pub const uint_ = USize{}
pub const usize_ = USize{}
pub const void_ = Void{}
pub const nil_ = Nil{}
pub const none_ = None{}
pub const voidptr_ = Pointer{
	base_type: Type(Void{})
}
pub const charptr_ = Pointer{
	base_type: Type(Char{})
}
pub const byteptr_ = Pointer{
	base_type: Type(Primitive{
		props: .integer | .unsigned
		size:  8
	})
}

// Reuse immutable builtin payloads instead of boxing a new sum value for every lookup.
const builtin_bool_type = Type(bool_)
const builtin_int_type = Type(int_)
const builtin_i8_type = Type(i8_)
const builtin_i16_type = Type(i16_)
const builtin_i32_type = Type(i32_)
const builtin_i64_type = Type(i64_)
const builtin_u8_type = Type(u8_)
const builtin_u16_type = Type(u16_)
const builtin_u32_type = Type(u32_)
const builtin_u64_type = Type(u64_)
const builtin_i128_type = Type(i128_)
const builtin_u128_type = Type(u128_)
const builtin_f32_type = Type(f32_)
const builtin_f64_type = Type(f64_)
const builtin_string_type = Type(string_)
const builtin_char_type = Type(char_)
const builtin_rune_type = Type(rune_)
const builtin_isize_type = Type(isize_)
const builtin_usize_type = Type(usize_)
const builtin_void_type = Type(void_)
const builtin_nil_type = Type(nil_)
const builtin_none_type = Type(none_)
const builtin_voidptr_type = Type(voidptr_)
const builtin_charptr_type = Type(charptr_)
const builtin_byteptr_type = Type(byteptr_)

// is_builtin_type_name reports whether name is one of V's builtin type names.
pub fn is_builtin_type_name(name string) bool {
	return match name.len {
		2 { name in ['i8', 'u8'] }
		3 { name in ['int', 'i16', 'i32', 'i64', 'u16', 'u32', 'u64', 'f32', 'f64', 'map', 'nil'] }
		4 { name in ['bool', 'char', 'i128', 'rune', 'u128', 'uint', 'void', 'none'] }
		5 { name in ['isize', 'usize', 'array'] }
		6 { name == 'string' }
		7 { name in ['voidptr', 'charptr', 'byteptr'] }
		else { false }
	}
}

// builtin_type_value returns the Type for a known builtin type name.
pub fn builtin_type_value(name string) Type {
	if name == 'bool' {
		return builtin_bool_type
	}
	if name == 'int' {
		return builtin_int_type
	}
	if name == 'i8' {
		return builtin_i8_type
	}
	if name == 'i16' {
		return builtin_i16_type
	}
	if name == 'i32' {
		return builtin_i32_type
	}
	if name == 'i64' {
		return builtin_i64_type
	}
	if name == 'u8' {
		return builtin_u8_type
	}
	if name == 'u16' {
		return builtin_u16_type
	}
	if name == 'u32' {
		return builtin_u32_type
	}
	if name == 'u64' {
		return builtin_u64_type
	}
	if name == 'i128' {
		return builtin_i128_type
	}
	if name == 'u128' {
		return builtin_u128_type
	}
	if name == 'f32' {
		return builtin_f32_type
	}
	if name == 'f64' {
		return builtin_f64_type
	}
	if name == 'string' {
		return builtin_string_type
	}
	if name == 'char' {
		return builtin_char_type
	}
	if name == 'rune' {
		return builtin_rune_type
	}
	if name == 'isize' {
		return builtin_isize_type
	}
	if name == 'uint' || name == 'usize' {
		return builtin_usize_type
	}
	if name == 'void' {
		return builtin_void_type
	}
	if name == 'voidptr' {
		return builtin_voidptr_type
	}
	if name == 'array' {
		return Type(Array{
			elem_type: Type(Void{})
		})
	}
	if name == 'charptr' {
		return builtin_charptr_type
	}
	if name == 'byteptr' {
		return builtin_byteptr_type
	}
	if name == 'nil' {
		return builtin_nil_type
	}
	if name == 'none' {
		return builtin_none_type
	}
	return Type(Unknown{
		reason: 'unknown builtin type'
	})
}

// builtin_type returns the Type for a builtin type name, or none otherwise.
pub fn builtin_type(name string) ?Type {
	if is_builtin_type_name(name) {
		return builtin_type_value(name)
	}
	return none
}

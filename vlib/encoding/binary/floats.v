module binary

// big_endian_f32_at reads an IEEE 754 f32 from four bytes at offset o in big endian order.
// Panics if o is negative or the value extends past the end of b.
@[inline]
pub fn big_endian_f32_at(b []u8, o int) f32 {
	// The union reinterprets the integer bits without a numeric conversion.
	return unsafe { U32_F32{ u: big_endian_u32_at(b, o) }.f }
}

// little_endian_f64_at reads an IEEE 754 f64 from eight bytes at offset o in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[inline]
pub fn little_endian_f64_at(b []u8, o int) f64 {
	// The union reinterprets the integer bits without a numeric conversion.
	return unsafe { U64_F64{ u: little_endian_u64_at(b, o) }.f }
}

// big_endian_f64_at reads an IEEE 754 f64 from eight bytes at offset o in big endian order.
// Panics if o is negative or the value extends past the end of b.
@[inline]
pub fn big_endian_f64_at(b []u8, o int) f64 {
	// The union reinterprets the integer bits without a numeric conversion.
	return unsafe { U64_F64{ u: big_endian_u64_at(b, o) }.f }
}

// little_endian_put_f32_at writes an IEEE 754 f32 to four bytes at offset o in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[inline]
pub fn little_endian_put_f32_at(mut b []u8, v f32, o int) {
	// The union preserves the float's exact representation, including its NaN payload.
	little_endian_put_u32_at(mut b, unsafe { U32_F32{ f: v }.u }, o)
}

// big_endian_put_f32_at writes an IEEE 754 f32 to four bytes at offset o in big endian order.
// Panics if o is negative or the value extends past the end of b.
@[inline]
pub fn big_endian_put_f32_at(mut b []u8, v f32, o int) {
	// The union preserves the float's exact representation, including its NaN payload.
	big_endian_put_u32_at(mut b, unsafe { U32_F32{ f: v }.u }, o)
}

// little_endian_put_f64_at writes an IEEE 754 f64 to eight bytes at offset o in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[inline]
pub fn little_endian_put_f64_at(mut b []u8, v f64, o int) {
	// The union preserves the float's exact representation, including its NaN payload.
	little_endian_put_u64_at(mut b, unsafe { U64_F64{ f: v }.u }, o)
}

// big_endian_put_f64_at writes an IEEE 754 f64 to eight bytes at offset o in big endian order.
// Panics if o is negative or the value extends past the end of b.
@[inline]
pub fn big_endian_put_f64_at(mut b []u8, v f64, o int) {
	// The union preserves the float's exact representation, including its NaN payload.
	big_endian_put_u64_at(mut b, unsafe { U64_F64{ f: v }.u }, o)
}

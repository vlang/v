module protobuf

// help unions to reinterpret a float as its integer bit pattern, so the fixed
// width writers can be reused for `float` and `double` without duplicating the
// byte shuffling. They mirror the ones in `encoding.binary`.
union U32F32 {
mut:
	u u32
	f f32
}

union U64F64 {
mut:
	u u64
	f f64
}

// put_fixed32 appends `value` as four little-endian bytes, the encoding shared
// by `fixed32`, `sfixed32`, and `float`.
pub fn put_fixed32(mut buf []u8, value u32) {
	buf << u8(value)
	buf << u8(value >> 8)
	buf << u8(value >> 16)
	buf << u8(value >> 24)
}

// put_fixed64 appends `value` as eight little-endian bytes, the encoding shared
// by `fixed64`, `sfixed64`, and `double`.
pub fn put_fixed64(mut buf []u8, value u64) {
	buf << u8(value)
	buf << u8(value >> 8)
	buf << u8(value >> 16)
	buf << u8(value >> 24)
	buf << u8(value >> 32)
	buf << u8(value >> 40)
	buf << u8(value >> 48)
	buf << u8(value >> 56)
}

// get_fixed32 reads the four little-endian bytes at `pos`, returning the value
// and the position just past them.
pub fn get_fixed32(data []u8, pos int) !(u32, int) {
	if pos < 0 || pos + 4 > data.len {
		return unexpected_eof_at(pos, 4, data.len - pos)
	}
	value := u32(data[pos]) | u32(data[pos + 1]) << 8 | u32(data[pos + 2]) << 16 |
		u32(data[pos + 3]) << 24
	return value, pos + 4
}

// get_fixed64 reads the eight little-endian bytes at `pos`, returning the value
// and the position just past them.
pub fn get_fixed64(data []u8, pos int) !(u64, int) {
	if pos < 0 || pos + 8 > data.len {
		return unexpected_eof_at(pos, 8, data.len - pos)
	}
	mut value := u64(0)
	for i in 0 .. 8 {
		value |= u64(data[pos + i]) << (u32(i) * 8)
	}
	return value, pos + 8
}

// put_float appends `value` as the four little-endian bytes of its IEEE-754
// binary32 bit pattern.
pub fn put_float(mut buf []u8, value f32) {
	// Reading a union field is an unsafe reinterpret, so it is wrapped and
	// kept to the one expression that needs it.
	put_fixed32(mut buf, unsafe { U32F32{ f: value }.u })
}

// put_double appends `value` as the eight little-endian bytes of its IEEE-754
// binary64 bit pattern.
pub fn put_double(mut buf []u8, value f64) {
	put_fixed64(mut buf, unsafe { U64F64{ f: value }.u })
}

// get_float reads a `float` from the four little-endian bytes at `pos`,
// returning the value and the position just past them.
pub fn get_float(data []u8, pos int) !(f32, int) {
	bits, next := get_fixed32(data, pos)!
	return unsafe { U32F32{ u: bits }.f }, next
}

// get_double reads a `double` from the eight little-endian bytes at `pos`,
// returning the value and the position just past them.
pub fn get_double(data []u8, pos int) !(f64, int) {
	bits, next := get_fixed64(data, pos)!
	return unsafe { U64F64{ u: bits }.f }, next
}

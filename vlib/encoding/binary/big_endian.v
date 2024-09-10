// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module binary

// big_endian_u16 creates a u16 from the first two bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u16(b []u8) u16 {
	_ = b[1] // bounds check
	return u16(b[1]) | (u16(b[0]) << u16(8))
}

// big_endian_u16_at creates a u16 from two bytes in the array b at the specified offset in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u16_at(b []u8, o int) u16 {
	_ = b[o] // bounds check
	_ = b[o + 1] // bounds check
	return u16(b[o + 1]) | (u16(b[o]) << u16(8))
}

// big_endian_u16_end creates a u16 from two bytes in the array b at the specified offset in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u16_end(b []u8) u16 {
	return big_endian_u16_at(b, b.len - 2)
}

// big_endian_put_u16 writes a u16 to the first two bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u16(mut b []u8, v u16) {
	_ = b[1] // bounds check
	b[0] = u8(v >> u16(8))
	b[1] = u8(v)
}

// big_endian_put_u16_at writes a u16 to the two bytes in the array b at the specified offset in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u16_at(mut b []u8, v u16, o int) {
	_ = b[o] // bounds check
	_ = b[o + 1] // bounds check
	b[o] = u8(v >> u16(8))
	b[o + 1] = u8(v)
}

// big_endian_put_u16_end writes a u16 to the last two bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u16_end(mut b []u8, v u16) {
	big_endian_put_u16_at(mut b, v, b.len - 2)
}

// big_endian_get_u16 creates u8 array from the unsigned 16-bit integer v in big endian order.
pub fn big_endian_get_u16(v u16) []u8 {
	mut b := []u8{cap: 2}
	b << u8(v >> u16(8))
	b << u8(v)
	return b
}

// big_endian_u32 creates a u32 from four bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u32(b []u8) u32 {
	_ = b[3] // bounds check
	return u32(b[3]) | (u32(b[2]) << u32(8)) | (u32(b[1]) << u32(16)) | (u32(b[0]) << u32(24))
}

// big_endian_u32_at creates a u32 from four bytes in the array b at the specified offset in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u32_at(b []u8, o int) u32 {
	_ = b[o] // bounds check
	_ = b[o + 3] // bounds check
	return u32(b[o + 3]) | (u32(b[o + 2]) << u32(8)) | (u32(b[o + 1]) << u32(16)) | (u32(b[o]) << u32(24))
}

// big_endian_u32_end creates a u32 from the last four bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u32_end(b []u8) u32 {
	return big_endian_u32_at(b, b.len - 4)
}

// big_endian_put_u32 writes a u32 to the first four bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u32(mut b []u8, v u32) {
	_ = b[3] // bounds check
	b[0] = u8(v >> u32(24))
	b[1] = u8(v >> u32(16))
	b[2] = u8(v >> u32(8))
	b[3] = u8(v)
}

// big_endian_put_u32_at writes a u32 to four bytes in the array b at the specified offset in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u32_at(mut b []u8, v u32, o int) {
	_ = b[o] // bounds check
	_ = b[o + 3] // bounds check
	b[o] = u8(v >> u32(24))
	b[o + 1] = u8(v >> u32(16))
	b[o + 2] = u8(v >> u32(8))
	b[o + 3] = u8(v)
}

// big_endian_put_u32_end writes a u32 to the last four bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u32_end(mut b []u8, v u32) {
	big_endian_put_u32_at(mut b, v, b.len - 4)
}

// big_endian_get_u32 creates u8 array from the unsigned 32-bit integer v in big endian order.
pub fn big_endian_get_u32(v u32) []u8 {
	mut b := []u8{cap: 4}
	b << u8(v >> u32(24))
	b << u8(v >> u32(16))
	b << u8(v >> u32(8))
	b << u8(v)
	return b
}

// big_endian_u64 creates a u64 from the first eight bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u64(b []u8) u64 {
	_ = b[7] // bounds check
	return u64(b[7]) | (u64(b[6]) << u64(8)) | (u64(b[5]) << u64(16)) | (u64(b[4]) << u64(24)) | (u64(b[3]) << u64(32)) | (u64(b[2]) << u64(40)) | (u64(b[1]) << u64(48)) | (u64(b[0]) << u64(56))
}

// big_endian_u64_at creates a u64 from eight bytes in the array b at the specified offset in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u64_at(b []u8, o int) u64 {
	_ = b[o] // bounds check
	_ = b[o + 7] // bounds check
	return u64(b[o + 7]) | (u64(b[o + 6]) << u64(8)) | (u64(b[o + 5]) << u64(16)) | (u64(b[o + 4]) << u64(24)) | (u64(b[
		o + 3]) << u64(32)) | (u64(b[o + 2]) << u64(40)) | (u64(b[o + 1]) << u64(48)) | (u64(b[o]) << u64(56))
}

// big_endian_u64_end creates a u64 from the last eight bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_u64_end(b []u8) u64 {
	return big_endian_u64_at(b, b.len - 8)
}

// big_endian_put_u64 writes a u64 to the first eight bytes in the array b in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u64(mut b []u8, v u64) {
	_ = b[7] // bounds check
	b[0] = u8(v >> u64(56))
	b[1] = u8(v >> u64(48))
	b[2] = u8(v >> u64(40))
	b[3] = u8(v >> u64(32))
	b[4] = u8(v >> u64(24))
	b[5] = u8(v >> u64(16))
	b[6] = u8(v >> u64(8))
	b[7] = u8(v)
}

// big_endian_put_u64_at writes a u64 to eight bytes in the array b at the specified offset in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u64_at(mut b []u8, v u64, o int) {
	_ = b[o] // bounds check
	_ = b[o + 7] // bounds check
	b[o] = u8(v >> u64(56))
	b[o + 1] = u8(v >> u64(48))
	b[o + 2] = u8(v >> u64(40))
	b[o + 3] = u8(v >> u64(32))
	b[o + 4] = u8(v >> u64(24))
	b[o + 5] = u8(v >> u64(16))
	b[o + 6] = u8(v >> u64(8))
	b[o + 7] = u8(v)
}

// big_endian_put_u64_end writes a u64 to the last eight bytes in the array b at the specified offset in big endian order.
@[direct_array_access; inline]
pub fn big_endian_put_u64_end(mut b []u8, v u64) {
	big_endian_put_u64_at(mut b, v, b.len - 8)
}

// big_endian_get_u64 creates u8 array from the unsigned 64-bit integer v in big endian order.
pub fn big_endian_get_u64(v u64) []u8 {
	mut b := []u8{}
	b << u8(v >> u64(56))
	b << u8(v >> u64(48))
	b << u8(v >> u64(40))
	b << u8(v >> u64(32))
	b << u8(v >> u64(24))
	b << u8(v >> u64(16))
	b << u8(v >> u64(8))
	b << u8(v)
	return b
}

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module binary

// little_endian_u16_fixed creates a u16 from the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u16_fixed(b [2]u8) u16 {
	return u16(b[0]) | (u16(b[1]) << u16(8))
}

// little_endian_put_u16_fixed writes a u16 to the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u16_fixed(mut b [2]u8, v u16) {
	b[0] = u8(v)
	b[1] = u8(v >> u16(8))
}

// little_endian_u32_fixed creates a u32 from the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u32_fixed(b [4]u8) u32 {
	return u32(b[0]) | (u32(b[1]) << u32(8)) | (u32(b[2]) << u32(16)) | (u32(b[3]) << u32(24))
}

// little_endian_put_u32_fixed writes a u32 to the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u32_fixed(mut b [4]u8, v u32) {
	b[0] = u8(v)
	b[1] = u8(v >> u32(8))
	b[2] = u8(v >> u32(16))
	b[3] = u8(v >> u32(24))
}

// little_endian_u64_fixed creates a u64 from the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u64_fixed(b [8]u8) u64 {
	return u64(b[0]) | (u64(b[1]) << u64(8)) | (u64(b[2]) << u64(16)) | (u64(b[3]) << u64(24)) | (u64(b[4]) << u64(32)) | (u64(b[5]) << u64(40)) | (u64(b[6]) << u64(48)) | (u64(b[7]) << u64(56))
}

// little_endian_put_u64_fixed writes a u64 to the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u64_fixed(mut b [8]u8, v u64) {
	b[0] = u8(v)
	b[1] = u8(v >> u64(8))
	b[2] = u8(v >> u64(16))
	b[3] = u8(v >> u64(24))
	b[4] = u8(v >> u64(32))
	b[5] = u8(v >> u64(40))
	b[6] = u8(v >> u64(48))
	b[7] = u8(v >> u64(56))
}

// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module binary

// Little Endian
[direct_array_access; inline]
pub fn little_endian_u16(b []u8) u16 {
	_ = b[1] // bounds check
	return u16(b[0]) | (u16(b[1]) << u16(8))
}

[direct_array_access; inline]
pub fn little_endian_put_u16(mut b []u8, v u16) {
	_ = b[1] // bounds check
	b[0] = u8(v)
	b[1] = u8(v >> u16(8))
}

[direct_array_access; inline]
pub fn little_endian_u32(b []u8) u32 {
	_ = b[3] // bounds check
	return u32(b[0]) | (u32(b[1]) << u32(8)) | (u32(b[2]) << u32(16)) | (u32(b[3]) << u32(24))
}

[direct_array_access; inline]
pub fn little_endian_put_u32(mut b []u8, v u32) {
	_ = b[3] // bounds check
	b[0] = u8(v)
	b[1] = u8(v >> u32(8))
	b[2] = u8(v >> u32(16))
	b[3] = u8(v >> u32(24))
}

[direct_array_access; inline]
pub fn little_endian_u64(b []u8) u64 {
	_ = b[7] // bounds check
	return u64(b[0]) | (u64(b[1]) << u64(8)) | (u64(b[2]) << u64(16)) | (u64(b[3]) << u64(24)) | (u64(b[4]) << u64(32)) | (u64(b[5]) << u64(40)) | (u64(b[6]) << u64(48)) | (u64(b[7]) << u64(56))
}

[direct_array_access; inline]
pub fn little_endian_put_u64(mut b []u8, v u64) {
	_ = b[7] // bounds check
	b[0] = u8(v)
	b[1] = u8(v >> u64(8))
	b[2] = u8(v >> u64(16))
	b[3] = u8(v >> u64(24))
	b[4] = u8(v >> u64(32))
	b[5] = u8(v >> u64(40))
	b[6] = u8(v >> u64(48))
	b[7] = u8(v >> u64(56))
}

// Big Endian
[direct_array_access; inline]
pub fn big_endian_u16(b []u8) u16 {
	_ = b[1] // bounds check
	return u16(b[1]) | (u16(b[0]) << u16(8))
}

[direct_array_access; inline]
pub fn big_endian_put_u16(mut b []u8, v u16) {
	_ = b[1] // bounds check
	b[0] = u8(v >> u16(8))
	b[1] = u8(v)
}

[direct_array_access; inline]
pub fn big_endian_u32(b []u8) u32 {
	_ = b[3] // bounds check
	return u32(b[3]) | (u32(b[2]) << u32(8)) | (u32(b[1]) << u32(16)) | (u32(b[0]) << u32(24))
}

[direct_array_access; inline]
pub fn big_endian_put_u32(mut b []u8, v u32) {
	_ = b[3] // bounds check
	b[0] = u8(v >> u32(24))
	b[1] = u8(v >> u32(16))
	b[2] = u8(v >> u32(8))
	b[3] = u8(v)
}

[direct_array_access; inline]
pub fn big_endian_u64(b []u8) u64 {
	_ = b[7] // bounds check
	return u64(b[7]) | (u64(b[6]) << u64(8)) | (u64(b[5]) << u64(16)) | (u64(b[4]) << u64(24)) | (u64(b[3]) << u64(32)) | (u64(b[2]) << u64(40)) | (u64(b[1]) << u64(48)) | (u64(b[0]) << u64(56))
}

[direct_array_access; inline]
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

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module binary

// little_endian_u16_fixed creates a u16 from the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u16_fixed(b [2]u8) u16 {
	unsafe {
		mut u := U16{}
		$if little_endian {
			u.b[0], u.b[1] = b[0], b[1]
		} $else {
			u.b[0], u.b[1] = b[1], b[0]
		}
		return u.u
	}
}

// little_endian_put_u16_fixed writes a u16 to the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u16_fixed(mut b [2]u8, v u16) {
	unsafe {
		mut u := U16{
			u: v
		}
		$if little_endian {
			b[0], b[1] = u.b[0], u.b[1]
		} $else {
			b[0], b[1] = u.b[1], u.b[0]
		}
	}
}

// little_endian_u32_fixed creates a u32 from the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u32_fixed(b [4]u8) u32 {
	unsafe {
		mut u := U32{}
		$if little_endian {
			u.b[0], u.b[1], u.b[2], u.b[3] = b[0], b[1], b[2], b[3]
		} $else {
			u.b[0], u.b[1], u.b[2], u.b[3] = b[3], b[2], b[1], b[0]
		}
		return u.u
	}
}

// little_endian_put_u32_fixed writes a u32 to the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u32_fixed(mut b [4]u8, v u32) {
	unsafe {
		mut u := U32{
			u: v
		}
		$if little_endian {
			b[0], b[1], b[2], b[3] = u.b[0], u.b[1], u.b[2], u.b[3]
		} $else {
			b[0], b[1], b[2], b[3] = u.b[3], u.b[2], u.b[1], u.b[0]
		}
	}
}

// little_endian_u64_fixed creates a u64 from the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u64_fixed(b [8]u8) u64 {
	unsafe {
		mut u := U64{}
		$if little_endian {
			u.b[0], u.b[1], u.b[2], u.b[3], u.b[4], u.b[5], u.b[6], u.b[7] = b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]
		} $else {
			u.b[0], u.b[1], u.b[2], u.b[3], u.b[4], u.b[5], u.b[6], u.b[7] = b[7], b[6], b[5], b[4], b[3], b[2], b[1], b[0]
		}
		return u.u
	}
}

// little_endian_put_u64_fixed writes a u64 to the fixed array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u64_fixed(mut b [8]u8, v u64) {
	unsafe {
		mut u := U64{
			u: v
		}
		$if little_endian {
			b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7] = u.b[0], u.b[1], u.b[2], u.b[3], u.b[4], u.b[5], u.b[6], u.b[7]
		} $else {
			b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7] = u.b[7], u.b[6], u.b[5], u.b[4], u.b[3], u.b[2], u.b[1], u.b[0]
		}
	}
}

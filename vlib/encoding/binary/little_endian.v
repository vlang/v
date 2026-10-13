// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module binary

// little_endian_u16 creates a u16 from the first two bytes in the array b in little endian order.
// Panics if b is shorter than the value width.
@[direct_array_access; inline]
pub fn little_endian_u16(b []u8) u16 {
	check_bounds(b, 0, 2)
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

// little_endian_u16_at creates a u16 from two bytes in the array b at the specified offset in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[direct_array_access; inline]
pub fn little_endian_u16_at(b []u8, o int) u16 {
	check_bounds(b, o, 2)
	unsafe {
		mut u := U16{}
		$if little_endian {
			u.b[0], u.b[1] = b[o], b[o + 1]
		} $else {
			u.b[0], u.b[1] = b[o + 1], b[o]
		}
		return u.u
	}
}

// little_endian_u16_end creates a u16 from the last two bytes of the array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u16_end(b []u8) u16 {
	return little_endian_u16_at(b, b.len - 2)
}

// little_endian_put_u16 writes a u16 to the first two bytes in the array b in little endian order.
// Panics if b is shorter than the value width.
@[direct_array_access; inline]
pub fn little_endian_put_u16(mut b []u8, v u16) {
	check_bounds(b, 0, 2)
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

// little_endian_put_u16_at writes a u16 to the two bytes in the array b at the specified offset in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[direct_array_access; inline]
pub fn little_endian_put_u16_at(mut b []u8, v u16, o int) {
	check_bounds(b, o, 2)
	unsafe {
		mut u := U16{
			u: v
		}
		$if little_endian {
			b[o], b[o + 1] = u.b[0], u.b[1]
		} $else {
			b[o], b[o + 1] = u.b[1], u.b[0]
		}
	}
}

// little_endian_put_u16_end writes a u16 to the last two bytes of the array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u16_end(mut b []u8, v u16) {
	little_endian_put_u16_at(mut b, v, b.len - 2)
}

// little_endian_get_u16 creates u8 array from the unsigned 16-bit integer v in little endian order.
pub fn little_endian_get_u16(v u16) []u8 {
	unsafe {
		mut u := U16{
			u: v
		}
		$if big_endian {
			u.b[0], u.b[1] = u.b[1], u.b[0]
		}
		return u.b[..]
	}
}

// little_endian_u32 creates a u32 from the first four bytes in the array b in little endian order.
// Panics if b is shorter than the value width.
@[direct_array_access; inline]
pub fn little_endian_u32(b []u8) u32 {
	check_bounds(b, 0, 4)
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

// little_endian_u32_at creates a u32 from four bytes in the array b at the specified offset in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[direct_array_access; inline]
pub fn little_endian_u32_at(b []u8, o int) u32 {
	check_bounds(b, o, 4)
	unsafe {
		mut u := U32{}
		$if little_endian {
			u.b[0], u.b[1], u.b[2], u.b[3] = b[o], b[o + 1], b[o + 2], b[o + 3]
		} $else {
			u.b[0], u.b[1], u.b[2], u.b[3] = b[o + 3], b[o + 2], b[o + 1], b[o]
		}
		return u.u
	}
}

// little_endian_u32_end creates a u32 from the last four bytes in the array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u32_end(b []u8) u32 {
	return little_endian_u32_at(b, b.len - 4)
}

// little_endian_put_u32 writes a u32 to the first four bytes in the array b in little endian order.
// Panics if b is shorter than the value width.
@[direct_array_access; inline]
pub fn little_endian_put_u32(mut b []u8, v u32) {
	check_bounds(b, 0, 4)
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

// little_endian_put_u32_at writes a u32 to the four bytes in the array b at the specified offset in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[direct_array_access; inline]
pub fn little_endian_put_u32_at(mut b []u8, v u32, o int) {
	check_bounds(b, o, 4)
	unsafe {
		mut u := U32{
			u: v
		}
		$if little_endian {
			b[o], b[o + 1], b[o + 2], b[o + 3] = u.b[0], u.b[1], u.b[2], u.b[3]
		} $else {
			b[o], b[o + 1], b[o + 2], b[o + 3] = u.b[3], u.b[2], u.b[1], u.b[0]
		}
	}
}

// little_endian_put_u32_end writes a u32 to the last four bytes in the array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u32_end(mut b []u8, v u32) {
	little_endian_put_u32_at(mut b, v, b.len - 4)
}

// little_endian_get_u32 creates u8 array from the unsigned 32-bit integer v in little endian order.
pub fn little_endian_get_u32(v u32) []u8 {
	unsafe {
		mut u := U32{
			u: v
		}
		$if big_endian {
			u.b[0], u.b[1], u.b[2], u.b[3] = u.b[3], u.b[2], u.b[1], u.b[0]
		}
		return u.b[..]
	}
}

// little_endian_u64 creates a u64 from the first eight bytes in the array b in little endian order.
// Panics if b is shorter than the value width.
@[direct_array_access; inline]
pub fn little_endian_u64(b []u8) u64 {
	check_bounds(b, 0, 8)
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

// little_endian_u64_at creates a u64 from eight bytes in the array b at the specified offset in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[direct_array_access; inline]
pub fn little_endian_u64_at(b []u8, o int) u64 {
	check_bounds(b, o, 8)
	unsafe {
		mut u := U64{}
		$if little_endian {
			u.b[0], u.b[1], u.b[2], u.b[3], u.b[4], u.b[5], u.b[6], u.b[7] = b[o], b[o + 1], b[o + 2], b[o + 3], b[o + 4], b[o + 5], b[o + 6], b[o + 7]
		} $else {
			u.b[0], u.b[1], u.b[2], u.b[3], u.b[4], u.b[5], u.b[6], u.b[7] = b[o + 7], b[o + 6], b[o + 5], b[o + 4], b[o + 3], b[o + 2], b[o + 1], b[o]
		}
		return u.u
	}
}

// little_endian_u64_end creates a u64 from the last eight bytes in the array b in little endian order.
@[direct_array_access; inline]
pub fn little_endian_u64_end(b []u8) u64 {
	return little_endian_u64_at(b, b.len - 8)
}

// little_endian_put_u64 writes a u64 to the first eight bytes in the array b in little endian order.
// Panics if b is shorter than the value width.
@[direct_array_access; inline]
pub fn little_endian_put_u64(mut b []u8, v u64) {
	check_bounds(b, 0, 8)
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

// little_endian_put_u64_at writes a u64 to the eight bytes in the array b at the specified offset in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[direct_array_access; inline]
pub fn little_endian_put_u64_at(mut b []u8, v u64, o int) {
	check_bounds(b, o, 8)
	unsafe {
		mut u := U64{
			u: v
		}
		$if little_endian {
			b[o], b[o + 1], b[o + 2], b[o + 3], b[o + 4], b[o + 5], b[o + 6], b[o + 7] = u.b[0], u.b[1], u.b[2], u.b[3], u.b[4], u.b[5], u.b[6], u.b[7]
		} $else {
			b[o], b[o + 1], b[o + 2], b[o + 3], b[o + 4], b[o + 5], b[o + 6], b[o + 7] = u.b[7], u.b[6], u.b[5], u.b[4], u.b[3], u.b[2], u.b[1], u.b[0]
		}
	}
}

// little_endian_put_u64_end writes a u64 to the last eight bytes in the array b at in little endian order.
@[direct_array_access; inline]
pub fn little_endian_put_u64_end(mut b []u8, v u64) {
	little_endian_put_u64_at(mut b, v, b.len - 8)
}

// little_endian_f32_at creates an f32 from four bytes at offset o in little endian order.
// Panics if o is negative or the value extends past the end of b.
@[direct_array_access; inline]
pub fn little_endian_f32_at(b []u8, o int) f32 {
	check_bounds(b, o, 4)
	unsafe {
		mut u := F32{}
		$if little_endian {
			u.b[0], u.b[1], u.b[2], u.b[3] = b[o], b[o + 1], b[o + 2], b[o + 3]
		} $else {
			u.b[0], u.b[1], u.b[2], u.b[3] = b[o + 3], b[o + 2], b[o + 1], b[o]
		}
		return u.u
	}
}

// little_endian_get_u64 creates u8 array from the unsigned 64-bit integer v in little endian order.
pub fn little_endian_get_u64(v u64) []u8 {
	unsafe {
		mut u := U64{
			u: v
		}
		$if big_endian {
			u.b[0], u.b[1], u.b[2], u.b[3], u.b[4], u.b[5], u.b[6], u.b[7] = u.b[7], u.b[6], u.b[5], u.b[4], u.b[3], u.b[2], u.b[1], u.b[0]
		}
		return u.b[..]
	}
}

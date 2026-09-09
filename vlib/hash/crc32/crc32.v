// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This is a fairly basic crc32 implementation, with 4 variants of the crc32 algorithm, and a way
// to create custom crc32 tables from user-provided polynomials.
module crc32

// polynomials
pub const ieee = u32(0xedb88320)
pub const castagnoli = u32(0x82f63b78)
pub const koopman = u32(0xeb31d82e)
// q is the standard CRC-32Q polynomial (MSB-first).
pub const q = u32(0x814141ab)
// q_reflected is the reflected (LSB-first) form of CRC-32Q polynomial.
pub const q_reflected = u32(0xd5828281)

// Named aliases for common CRC-32 variants.
pub const crc32c = castagnoli
pub const crc32k = koopman
pub const crc32q = q
pub const crc32q_reflected = q_reflected

struct Crc32 {
mut:
	table []u32
}

// generate_table populates a 256-word table from the specified polynomial `poly`
// to represent the polynomial for efficient processing.
@[direct_array_access]
fn (mut c Crc32) generate_table(poly u32) {
	c.table = []u32{len: 256}
	for i in 0 .. 256 {
		mut crc := u32(i)
		for _ in 0 .. 8 {
			if crc & u32(1) == u32(1) {
				crc = (crc >> 1) ^ poly
			} else {
				crc >>= u32(1)
			}
		}
		c.table[i] = crc
	}
}

@[direct_array_access]
fn (c &Crc32) update32(crc u32, b []u8) u32 {
	mut next := crc
	for i in 0 .. b.len {
		next = c.table[u8(next) ^ b[i]] ^ (next >> 8)
	}
	return next
}

// update_state updates an internal CRC state with the bytes in `b`.
// Start from `~u32(0)` and finalize with `~state`.
pub fn (c &Crc32) update_state(state u32, b []u8) u32 {
	return c.update32(state, b)
}

// checksum returns the CRC-32 checksum of data `b` by using the polynomial represented by `c`'s table.
pub fn (c &Crc32) checksum(b []u8) u32 {
	return ~c.update_state(~u32(0), b)
}

// update returns the updated CRC-32 checksum for `b`, starting from `crc`.
// Use `crc = 0` for a fresh checksum, or pass a previous result to continue streaming.
pub fn (c &Crc32) update(crc u32, b []u8) u32 {
	state := c.update_state(~crc, b)
	return ~state
}

// new creates a `Crc32` polynomial.
pub fn new(poly u32) &Crc32 {
	mut c := &Crc32{}
	c.generate_table(poly)
	return c
}

// sum_with_poly calculates the CRC-32 checksum of `b` for the provided polynomial.
// Built-in constants use their canonical parameter sets.
pub fn sum_with_poly(poly u32, b []u8) u32 {
	return match poly {
		ieee { ieee_poly.checksum(b) }
		crc32c { crc32c_poly.checksum(b) }
		crc32k { crc32k_poly.checksum(b) }
		crc32q { crc32q_sum_internal(b) }
		crc32q_reflected { crc32q_reflected_poly.checksum(b) }
		else { new(poly).checksum(b) }
	}
}

const ieee_poly = new(ieee)
const crc32c_poly = new(crc32c)
const crc32k_poly = new(crc32k)
const crc32q_reflected_poly = new(crc32q_reflected)
const crc32q_table = crc32q_generate_table(q)

@[direct_array_access]
fn crc32q_generate_table(poly u32) []u32 {
	mut table := []u32{len: 256}
	for i in 0 .. 256 {
		mut crc := u32(i) << 24
		for _ in 0 .. 8 {
			if crc & u32(0x80000000) != 0 {
				crc = (crc << 1) ^ poly
			} else {
				crc <<= 1
			}
		}
		table[i] = crc
	}
	return table
}

@[direct_array_access]
fn crc32q_sum_internal(b []u8) u32 {
	mut crc := u32(0)
	for byte in b {
		idx := u8((crc >> 24) ^ byte)
		crc = crc32q_table[idx] ^ (crc << 8)
	}
	return crc
}

// sum calculates the CRC-32 checksum of `b` by using the IEEE polynomial.
pub fn sum(b []u8) u32 {
	return ieee_poly.checksum(b)
}

// sum_crc32c calculates the CRC-32C checksum of `b`.
pub fn sum_crc32c(b []u8) u32 {
	return crc32c_poly.checksum(b)
}

// sum_crc32k calculates the CRC-32K checksum of `b`.
pub fn sum_crc32k(b []u8) u32 {
	return crc32k_poly.checksum(b)
}

// sum_crc32q calculates the CRC-32Q checksum of `b`.
pub fn sum_crc32q(b []u8) u32 {
	return crc32q_sum_internal(b)
}

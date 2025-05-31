// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Package scrypt implements the key derivation functions as
// described in https://datatracker.ietf.org/doc/html/rfc7914
module scrypt

import crypto.pbkdf2
import crypto.sha256
import encoding.binary
import math.bits

pub const max_buffer_length = ((u64(1) << 32) - 1) * 32
pub const max_blocksize_parallal_product = u64(1 << 30)

// salsa20_8 applies the salsa20/8 core transformation to a block
// of 64 u8 bytes.  The block is modified in place.
fn salsa20_8(mut block []u8) {
	mut block_words := []u32{len: 16}
	mut scratch := [16]u32{}

	for i in 0 .. 16 {
		block_words[i] = binary.little_endian_u32_at(block, i * 4)
		scratch[i] = block_words[i]
	}

	for i := 8; i > 0; i -= 2 {
		// processing columns
		scratch[4] ^= bits.rotate_left_32(scratch[0] + scratch[12], 7)
		scratch[8] ^= bits.rotate_left_32(scratch[4] + scratch[0], 9)
		scratch[12] ^= bits.rotate_left_32(scratch[8] + scratch[4], 13)
		scratch[0] ^= bits.rotate_left_32(scratch[12] + scratch[8], 18)

		scratch[9] ^= bits.rotate_left_32(scratch[5] + scratch[1], 7)
		scratch[13] ^= bits.rotate_left_32(scratch[9] + scratch[5], 9)
		scratch[1] ^= bits.rotate_left_32(scratch[13] + scratch[9], 13)
		scratch[5] ^= bits.rotate_left_32(scratch[1] + scratch[13], 18)

		scratch[14] ^= bits.rotate_left_32(scratch[10] + scratch[6], 7)
		scratch[2] ^= bits.rotate_left_32(scratch[14] + scratch[10], 9)
		scratch[6] ^= bits.rotate_left_32(scratch[2] + scratch[14], 13)
		scratch[10] ^= bits.rotate_left_32(scratch[6] + scratch[2], 18)

		scratch[3] ^= bits.rotate_left_32(scratch[15] + scratch[11], 7)
		scratch[7] ^= bits.rotate_left_32(scratch[3] + scratch[15], 9)
		scratch[11] ^= bits.rotate_left_32(scratch[7] + scratch[3], 13)
		scratch[15] ^= bits.rotate_left_32(scratch[11] + scratch[7], 18)

		// processing rows
		scratch[1] ^= bits.rotate_left_32(scratch[0] + scratch[3], 7)
		scratch[2] ^= bits.rotate_left_32(scratch[1] + scratch[0], 9)
		scratch[3] ^= bits.rotate_left_32(scratch[2] + scratch[1], 13)
		scratch[0] ^= bits.rotate_left_32(scratch[3] + scratch[2], 18)

		scratch[6] ^= bits.rotate_left_32(scratch[5] + scratch[4], 7)
		scratch[7] ^= bits.rotate_left_32(scratch[6] + scratch[5], 9)
		scratch[4] ^= bits.rotate_left_32(scratch[7] + scratch[6], 13)
		scratch[5] ^= bits.rotate_left_32(scratch[4] + scratch[7], 18)

		scratch[11] ^= bits.rotate_left_32(scratch[10] + scratch[9], 7)
		scratch[8] ^= bits.rotate_left_32(scratch[11] + scratch[10], 9)
		scratch[9] ^= bits.rotate_left_32(scratch[8] + scratch[11], 13)
		scratch[10] ^= bits.rotate_left_32(scratch[9] + scratch[8], 18)

		scratch[12] ^= bits.rotate_left_32(scratch[15] + scratch[14], 7)
		scratch[13] ^= bits.rotate_left_32(scratch[12] + scratch[15], 9)
		scratch[14] ^= bits.rotate_left_32(scratch[13] + scratch[12], 13)
		scratch[15] ^= bits.rotate_left_32(scratch[14] + scratch[13], 18)
	}

	for i in 0 .. 16 {
		scratch[i] += block_words[i]
		binary.little_endian_put_u32_at(mut block, scratch[i], i * 4)
	}
}

@[inline]
fn blkcpy(mut dest []u8, src []u8, len u32) {
	for i in 0 .. len {
		dest[i] = src[i]
	}
}

@[inline]
fn blkxor(mut dest []u8, src []u8, len u32) {
	for i in 0 .. len {
		dest[i] ^= src[i]
	}
}

// block_mix performs the block_mix operation using salsa20_8
//
// The block input must be 128 * r in length.  The temp array
// has to be the same size, 128 * r.  r is a positive integer
// value > 0.  The block is modified in place.
fn block_mix(mut block []u8, mut temp []u8, r u32) {
	mut scratch := []u8{len: 64, cap: 64}

	blkcpy(mut scratch, block[(((2 * r) - 1) * 64)..], 64)

	for i in 0 .. 2 * r {
		start := i * 64
		stop := start + 64

		blkxor(mut scratch, block[start..stop], 64)
		salsa20_8(mut scratch)

		blkcpy(mut temp[start..stop], scratch, 64)
	}

	for i in 0 .. r {
		start := i * 64
		stop := start + 64

		temp_start := (i * 2) * 64
		temp_stop := temp_start + 64

		blkcpy(mut block[start..stop], temp[temp_start..temp_stop], 64)
	}

	for i in 0 .. r {
		start := (i + r) * 64
		stop := start + 64

		temp_start := ((i * 2) + 1) * 64
		temp_stop := temp_start + 64

		blkcpy(mut block[start..stop], temp[temp_start..temp_stop], 64)
	}
}

fn smix(mut block []u8, r u32, n u64, mut v_block []u8, mut temp_block []u8) {
	blkcpy(mut temp_block, block, 128 * r)

	y_start := 128 * r

	for i in 0 .. n {
		v_start := i * (128 * r)
		v_stop := v_start + (128 * r)

		blkcpy(mut v_block[v_start..v_stop], temp_block, 128 * r)
		block_mix(mut temp_block, mut temp_block[y_start..], r)
	}

	for _ in 0 .. n {
		j := binary.little_endian_u64_at(temp_block, int(((2 * r) - 1) * 64)) & (n - 1)

		v_start := j * (128 * r)
		v_stop := v_start + (128 * r)

		blkxor(mut temp_block, v_block[v_start..v_stop], 128 * r)
		block_mix(mut temp_block, mut temp_block[y_start..], r)
	}

	blkcpy(mut block, temp_block, 128 * r)
}

struct OutputBufferLengthError {
	Error
	length u64
}

fn (err OutputBufferLengthError) msg() string {
	return 'the output buffer length, ${err.length}, is greater than ${max_buffer_length}'
}

struct BlocksizeParallelProductError {
	Error
	blocksize u32
	parallel  u32
	product   u64
}

fn (err BlocksizeParallelProductError) msg() string {
	return 'the product of blocksize ${err.blocksize} * parallel ${err.parallel} = ${err.product}, is greater than ${max_blocksize_parallal_product}'
}

struct CpuMemoryCostError {
	Error
	cost u64
}

fn (err CpuMemoryCostError) msg() string {
	return 'the CPU/memory cost ${err.cost} must be greater than 0 and also a power of 2'
}

// scrypt performs password based key derivation using the scrypt algorithm.
//
// The input parameters are:
//
//     password - a slice of bytes which is the password being used to
//         derive the key.  Don't leak this value to anybody.
//     salt - a slice of bytes used to make it harder to crack the key.
//     n - CPU/Memory cost parameter, must be larger than 0, a power of 2,
//         and less than 2^(128 * r / 8).
//     r - block size parameter.
//     p - parallelization parameter, a positive integer less than or
//         equal to ((2^32-1) * hLen) / MFLen where hLen is 32 and
//         MFlen is 128 * r.
//     dk_len - intended output length in octets of the derived key;
//         a positive integer less than or equal to (2^32 - 1) * hLen
//         where hLen is 32.
//
// Reasonable values for n, r, and p are n = 1024, r = 8, p = 16.
pub fn scrypt(password []u8, salt []u8, n u64, r u32, p u32, dk_len u64) ![]u8 {
	if dk_len > max_buffer_length {
		return OutputBufferLengthError{
			length: dk_len
		}
	}

	if u64(r) * u64(p) >= max_blocksize_parallal_product {
		return BlocksizeParallelProductError{
			blocksize: r
			parallel:  p
			product:   u64(r) * u64(p)
		}
	}

	// the following is a sneaky way to determine if a number is a
	// power of 2.  Also, a value of 0 is not allowed.
	if (n & (n - 1)) != 0 || n == 0 {
		return CpuMemoryCostError{
			cost: n
		}
	}

	mut b := pbkdf2.key(password, salt, 1, int(128 * r * p), sha256.new())!

	mut xy := []u8{len: int(256 * r), cap: int(256 * r), init: 0}
	mut v := []u8{len: int(128 * r * n), cap: int(128 * r * n), init: 0}

	for i in u32(0) .. p {
		smix(mut b[i * 128 * r..], r, n, mut v, mut xy)
	}

	result := pbkdf2.key(password, b, 1, int(128 * r * p), sha256.new())!

	return result[..dk_len]
}

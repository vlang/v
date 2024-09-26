// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package blake2s implements the Blake2s 256, 224, 160, and
// 128 bit hash algorithms
// as defined in IETF RFC 7693.
// Based off:   https://datatracker.ietf.org/doc/html/rfc7693
// Last updated: November 2015
module blake2s

import encoding.binary

// size128 is the size, in bytes, of a Blake2s 128 checksum.
pub const size128 = 16
// size160 is the size, in bytes, of a Blake2s 160 checksum.
pub const size160 = 20
// size224 is the size, in bytes, of a Blake2s 224 checksum.
pub const size224 = 28
// size256 is the size, in bytes, of a Blake2s 256 checksum.
pub const size256 = 32

// block_size is the block size, in bytes, of the Blake2s hash functions.
pub const block_size = 64

// G rotation constants
const r1 = 16
const r2 = 12
const r3 = 8
const r4 = 7

// negative G rotation constants so we can rotate right.
const nr1 = -1 * r1
const nr2 = -1 * r2
const nr3 = -1 * r3
const nr4 = -1 * r4

// initialization vector
const iv = [
	u32(0x6a09e667),
	u32(0xbb67ae85),
	u32(0x3c6ef372),
	u32(0xa54ff53a),
	u32(0x510e527f),
	u32(0x9b05688c),
	u32(0x1f83d9ab),
	u32(0x5be0cd19),
]

// message word schedule permutations
const sigma = [
	[u8(0), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
	[u8(14), 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
	[u8(11), 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4],
	[u8(7), 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8],
	[u8(9), 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13],
	[u8(2), 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9],
	[u8(12), 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11],
	[u8(13), 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10],
	[u8(6), 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5],
	[u8(10), 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0],
]

struct Digest {
	hash_size u8
mut:
	input_buffer []u8
	h            []u32
	m            [16]u32
	t            u64
}

// string makes a formatted string representation of a Digest structure
pub fn (d Digest) str() string {
	return 'blake2s.Digest{\n    hash_size: ${d.hash_size}\n    input_buffer: ${d.input_buffer}\n    input_buffer.len: ${d.input_buffer.len}\n    h: [0x${d.h[0]:016x}, 0x${d.h[1]:016x}, 0x${d.h[2]:016x}, 0x${d.h[3]:016x},\n        0x${d.h[4]:016x}, 0x${d.h[5]:016x}, 0x${d.h[6]:016x}, 0x${d.h[7]:016x}]\n    m: [0x${d.m[0]:016x}, 0x${d.m[1]:016x}, 0x${d.m[2]:016x}, 0x${d.m[3]:016x},\n        0x${d.m[4]:016x}, 0x${d.m[5]:016x}, 0x${d.m[6]:016x}, 0x${d.m[7]:016x},\n        0x${d.m[8]:016x}, 0x${d.m[9]:016x}, 0x${d.m[10]:016x}, 0x${d.m[11]:016x},\n        0x${d.m[12]:016x}, 0x${d.m[13]:016x}, 0x${d.m[14]:016x}, 0x${d.m[15]:016x}]\n    t: ${d.t}\n}'
}

// new256 initializes the digest structure for a Blake2s 256 bit hash
pub fn new256() !&Digest {
	return new_digest(size256, []u8{})!
}

// new_pmac256 initializes the digest structure for a Blake2s 256 bit prefix MAC
pub fn new_pmac256(key []u8) !&Digest {
	return new_digest(size256, key)!
}

// new224 initializes the digest structure for a Blake2s 224 bit hash
pub fn new224() !&Digest {
	return new_digest(size224, []u8{})!
}

// new_pmac224 initializes the digest structure for a Blake2s 224 bit prefix MAC
pub fn new_pmac224(key []u8) !&Digest {
	return new_digest(size224, key)!
}

// new160 initializes the digest structure for a Blake2s 160 bit hash
pub fn new160() !&Digest {
	return new_digest(size160, []u8{})!
}

// new_pmac160 initializes the digest structure for a Blake2s 160 bit prefix MAC
pub fn new_pmac160(key []u8) !&Digest {
	return new_digest(size160, key)!
}

// new126 initializes the digest structure for a Blake2s 128 bit hash
pub fn new128() !&Digest {
	return new_digest(size128, []u8{})!
}

// new_pmac128 initializes the digest structure for a Blake2s 128 bit prefix MAC
pub fn new_pmac128(key []u8) !&Digest {
	return new_digest(size128, key)!
}

struct HashSizeError {
	Error
	size u8
}

fn (err HashSizeError) msg() string {
	return 'Hash size ${err.size} must be between 1 and ${size256}'
}

struct KeySizeError {
	Error
	size i32
}

fn (err KeySizeError) msg() string {
	return 'Key size ${err.size} must be between 0 and ${size256}'
}

struct InputBufferSizeError {
	Error
	size i32
}

fn (err InputBufferSizeError) msg() string {
	return 'The input buffer size ${err.size} .must be between 0 and ${block_size}'
}

// new_digest creates an initialized digest structure based on
// the hash size and whether or not you specify a MAC key.
//
// hash_size - the number if bytes in the generated hash.
//     Legal values are between 1 and 32.
//
// key - key used for generating a prefix MAC.  A zero length
//     key is used for just generating a hash.  A key of 1 to
//     32 bytes can be used for generating a prefix MAC.
pub fn new_digest(hash_size u8, key []u8) !&Digest {
	if hash_size < 1 || hash_size > size256 {
		return HashSizeError{
			size: hash_size
		}
	}

	if key.len < 0 || key.len > size256 {
		return KeySizeError{
			size: i32(key.len)
		}
	}

	mut d := Digest{
		h:         iv.clone()
		t:         0
		hash_size: hash_size
	}

	if key.len > 0 {
		p0 := 0x01010000 ^ u32(key.len) << 8 ^ u32(hash_size)

		d.h[0] ^= p0

		d.input_buffer.clear()
		d.input_buffer << key

		pad_length := block_size - key.len
		for _ in 0 .. pad_length {
			d.input_buffer << 0
		}
	} else {
		p0 := 0x01010000 ^ u32(hash_size)

		d.h[0] ^= p0
	}

	return &d
}

// The intent is to only use this method
// when the input buffer is full or when
// the last data to be hashed is in the
// input buffer.
//
// If the input buffer does not contain enough
// data to fill all the message blocks, the
// input buffer is padded with 0 bytes until
// it is full prior to moving the data into
// the message blocks.
//
// The input buffer is emptied.
//
// The total byte count is incremented by the
// number of bytes in the input buffer.
fn (mut d Digest) move_input_to_message_blocks() ! {
	// the number of bytes in the input buffer
	// should never exceed the block size.
	if d.input_buffer.len < 0 || d.input_buffer.len > block_size {
		return InputBufferSizeError{
			size: i32(d.input_buffer.len)
		}
	}

	// keep the hashed data length up to date
	d.t += u64(d.input_buffer.len)

	// pad the input buffer if necessary
	if d.input_buffer.len < block_size {
		pad_length := block_size - d.input_buffer.len

		for _ in 0 .. pad_length {
			d.input_buffer << 0
		}
	}

	// treat the input bytes as little endian u32 values
	for i in 0 .. 16 {
		d.m[i] = binary.little_endian_u32_at(d.input_buffer, i * 4)
	}

	// empty the input buffer
	d.input_buffer.clear()

	return
}

// write adds bytes to the hash
pub fn (mut d Digest) write(data []u8) ! {
	// if no data is being added to the hash,
	// just return
	if data.len == 0 {
		return
	}

	// if the input buffer is already full,
	// process the existing input bytes first.
	// this is not the final input.
	if d.input_buffer.len >= block_size {
		d.move_input_to_message_blocks()!
		d.f(false)
	}

	// add data to the input buffer until you
	// run out of space in the input buffer or
	// run out of data, whichever comes first.
	empty_space := block_size - d.input_buffer.len
	mut remaining_data := unsafe { data[..] }

	if empty_space >= data.len {
		// ran out of data first
		// just add it to the input buffer and return
		d.input_buffer << data
		return
	} else {
		// ran out of input buffer space first
		// fill it up and process it.
		d.input_buffer << data[..empty_space]
		remaining_data = unsafe { remaining_data[empty_space..] }

		d.move_input_to_message_blocks()!
		d.f(false)
	}

	// process the data in block size amounts until
	// all the data has been processed
	for remaining_data.len > 0 {
		if block_size >= remaining_data.len {
			// running out of data
			// just add it to the input buffer and return
			d.input_buffer << remaining_data
			return
		}

		// add block size bytes to the input buffer
		// and process it
		d.input_buffer << remaining_data[..block_size]
		remaining_data = unsafe { remaining_data[block_size..] }

		d.move_input_to_message_blocks()!
		d.f(false)
	}

	return
}

fn (mut d Digest) checksum_internal() ![]u8 {
	// process the last input bytes
	d.move_input_to_message_blocks()!
	d.f(true)

	// get the hash into the proper byte order
	mut hash_bytes := []u8{}

	for hash in d.h {
		mut h_bytes := []u8{len: 4, cap: 4}
		binary.little_endian_put_u32(mut h_bytes, hash)
		hash_bytes << h_bytes
	}

	// return the appropriate number of hash bytes
	return hash_bytes[..d.hash_size]
}

// checksum finalizes the hash and returns the generated bytes.
pub fn (mut d Digest) checksum() []u8 {
	return d.checksum_internal() or { panic(err) }
}

// sum256 returns the Blake2s 256 bit checksum of the data.
pub fn sum256(data []u8) []u8 {
	mut d := new256() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// sum224 returns the Blake2s 224 bit checksum of the data.
pub fn sum224(data []u8) []u8 {
	mut d := new224() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// sum160 returns the Blake2s 160 bit checksum of the data.
pub fn sum160(data []u8) []u8 {
	mut d := new160() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// sum128 returns the Blake2s 128 bit checksum of the data.
pub fn sum128(data []u8) []u8 {
	mut d := new128() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// pmac256 returns the Blake2s 256 bit prefix MAC of the data.
pub fn pmac256(data []u8, key []u8) []u8 {
	mut d := new_pmac256(key) or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// pmac224 returns the Blake2s 224 bit prefix MAC of the data.
pub fn pmac224(data []u8, key []u8) []u8 {
	mut d := new_pmac224(key) or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// pmac160 returns the Blake2s 160 bit prefix MAC of the data.
pub fn pmac160(data []u8, key []u8) []u8 {
	mut d := new_pmac160(key) or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// pmac128 returns the Blake2s 128 bit prefix MAC of the data.
pub fn pmac128(data []u8, key []u8) []u8 {
	mut d := new_pmac128(key) or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

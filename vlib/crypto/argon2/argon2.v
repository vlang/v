// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Based off:   https://github.com/golang/crypto/tree/master/argon2
// Package argon2 implements the Argon2 password hashing and key derivation
// functions described in RFC 9106.
module argon2

import crypto.blake2b
import crypto.hmac
import crypto.rand
import encoding.base64
import encoding.binary
import math.bits

pub const version = u32(0x13)

pub const default_time = u32(3)
pub const default_memory = u32(64 * 1024)
pub const default_threads = u8(4)
pub const default_key_len = u32(32)
pub const default_salt_len = 16

const argon2_d = 0
const argon2_i = 1
const argon2_id = 2
const block_length = 128
const block_bytes = 1024
const sync_points = u32(4)

// Params controls Argon2 password hashing output.
pub struct Params {
pub:
	time     u32
	memory   u32
	threads  u8
	key_len  u32
	salt_len int
}

struct InvalidTimeError {
	Error
	time u32
}

fn (err InvalidTimeError) msg() string {
	return 'the number of rounds ${err.time} must be greater than 0'
}

struct InvalidThreadsError {
	Error
	threads u8
}

fn (err InvalidThreadsError) msg() string {
	return 'the parallelism degree ${err.threads} must be greater than 0'
}

struct InvalidKeyLengthError {
	Error
	length u32
}

fn (err InvalidKeyLengthError) msg() string {
	return 'the output length ${err.length} must be greater than 0'
}

struct InvalidSaltLengthError {
	Error
	length int
}

fn (err InvalidSaltLengthError) msg() string {
	return 'the salt length ${err.length} must be greater than 0'
}

struct InvalidHashError {
	Error
	reason string
}

fn (err InvalidHashError) msg() string {
	return err.reason
}

// default_params returns the recommended default Argon2id password hashing parameters.
pub fn default_params() Params {
	return Params{
		time:     default_time
		memory:   default_memory
		threads:  default_threads
		key_len:  default_key_len
		salt_len: default_salt_len
	}
}

// key derives a key from `password` and `salt` using Argon2i.
pub fn key(password []u8, salt []u8, time u32, memory u32, threads u8, key_len u32) ![]u8 {
	return derive_key(argon2_i, password, salt, []u8{}, []u8{}, time, memory, threads, key_len)
}

// d_key derives a key from `password` and `salt` using Argon2d.
pub fn d_key(password []u8, salt []u8, time u32, memory u32, threads u8, key_len u32) ![]u8 {
	return derive_key(argon2_d, password, salt, []u8{}, []u8{}, time, memory, threads, key_len)
}

// id_key derives a key from `password` and `salt` using Argon2id.
pub fn id_key(password []u8, salt []u8, time u32, memory u32, threads u8, key_len u32) ![]u8 {
	return derive_key(argon2_id, password, salt, []u8{}, []u8{}, time, memory, threads, key_len)
}

// generate_from_password hashes `password` with default Argon2id parameters
// and returns a PHC-formatted encoded string.
pub fn generate_from_password(password []u8) !string {
	return generate_from_password_with_params(password, default_params())
}

// generate_from_password_with_params hashes `password` with Argon2id using `params`
// and returns a PHC-formatted encoded string.
pub fn generate_from_password_with_params(password []u8, params Params) !string {
	normalized := normalize_params(params)
	if normalized.salt_len <= 0 {
		return InvalidSaltLengthError{
			length: normalized.salt_len
		}
	}
	salt := rand.bytes(normalized.salt_len)!
	hash := derive_key(argon2_id, password, salt, []u8{}, []u8{}, normalized.time,
		normalized.memory, normalized.threads, normalized.key_len)!
	return encode_hash(argon2_id, salt, hash, normalized.time, normalized.memory,
		normalized.threads)
}

// compare_hash_and_password verifies a PHC-formatted Argon2 hash against `password`.
pub fn compare_hash_and_password(password []u8, encoded_hash []u8) ! {
	decoded := decode_hash(encoded_hash.bytestr())!
	other_hash := derive_key(decoded.mode, password, decoded.salt, []u8{}, []u8{}, decoded.time,
		decoded.memory, decoded.threads, u32(decoded.hash.len))!
	if !hmac.equal(other_hash, decoded.hash) {
		return error('mismatched hash and password')
	}
}

struct DecodedHash {
	mode    int
	time    u32
	memory  u32
	threads u8
	salt    []u8
	hash    []u8
}

fn derive_key(mode int, password []u8, salt []u8, secret []u8, data []u8, time u32, memory u32, threads u8, key_len u32) ![]u8 {
	if time < 1 {
		return InvalidTimeError{
			time: time
		}
	}
	if threads < 1 {
		return InvalidThreadsError{
			threads: threads
		}
	}
	if key_len < 1 {
		return InvalidKeyLengthError{
			length: key_len
		}
	}
	threads_u32 := u32(threads)
	effective_memory := normalize_memory(memory, threads_u32)
	h0 :=
		init_hash(password, salt, secret, data, time, effective_memory, threads_u32, key_len, mode)!
	mut blocks := init_blocks(h0, effective_memory, threads_u32)!
	process_blocks(mut blocks, time, effective_memory, threads_u32, mode)
	return extract_key(mut blocks, effective_memory, threads_u32, key_len)
}

fn normalize_params(params Params) Params {
	threads := if params.threads == 0 { default_threads } else { params.threads }
	memory := if params.memory == 0 { default_memory } else { params.memory }
	return Params{
		time:     if params.time == 0 { default_time } else { params.time }
		memory:   normalize_memory(memory, u32(threads))
		threads:  threads
		key_len:  if params.key_len == 0 { default_key_len } else { params.key_len }
		salt_len: if params.salt_len == 0 { default_salt_len } else { params.salt_len }
	}
}

@[inline]
fn normalize_memory(memory u32, threads u32) u32 {
	mut effective_memory := memory / (sync_points * threads) * (sync_points * threads)
	if effective_memory < 2 * sync_points * threads {
		effective_memory = 2 * sync_points * threads
	}
	return effective_memory
}

fn init_hash(password []u8, salt []u8, secret []u8, data []u8, time u32, memory u32, threads u32, key_len u32, mode int) ![]u8 {
	mut params := []u8{len: 24, init: 0}
	mut tmp := []u8{len: 4, init: 0}
	mut d := blake2b.new512()!
	binary.little_endian_put_u32_at(mut params, threads, 0)
	binary.little_endian_put_u32_at(mut params, key_len, 4)
	binary.little_endian_put_u32_at(mut params, memory, 8)
	binary.little_endian_put_u32_at(mut params, time, 12)
	binary.little_endian_put_u32_at(mut params, version, 16)
	binary.little_endian_put_u32_at(mut params, u32(mode), 20)
	d.write(params)!
	binary.little_endian_put_u32(mut tmp, u32(password.len))
	d.write(tmp)!
	d.write(password)!
	binary.little_endian_put_u32(mut tmp, u32(salt.len))
	d.write(tmp)!
	d.write(salt)!
	binary.little_endian_put_u32(mut tmp, u32(secret.len))
	d.write(tmp)!
	d.write(secret)!
	binary.little_endian_put_u32(mut tmp, u32(data.len))
	d.write(tmp)!
	d.write(data)!
	mut h0 := []u8{len: blake2b.size512 + 8, init: 0}
	sum := d.checksum()
	copy(mut h0[..blake2b.size512], sum)
	return h0
}

fn init_blocks(h0 []u8, memory u32, threads u32) ![]u64 {
	mut block := []u8{len: block_bytes, init: 0}
	mut blocks := []u64{len: int(memory) * block_length, init: u64(0)}
	mut h0_block := h0.clone()
	for lane := u32(0); lane < threads; lane++ {
		start := lane * (memory / threads)
		binary.little_endian_put_u32_at(mut h0_block, lane, blake2b.size512 + 4)
		binary.little_endian_put_u32_at(mut h0_block, 0, blake2b.size512)
		block = blake2b_hash(block_bytes, h0_block)!
		set_block(mut blocks, start, block)
		binary.little_endian_put_u32_at(mut h0_block, 1, blake2b.size512)
		block = blake2b_hash(block_bytes, h0_block)!
		set_block(mut blocks, start + 1, block)
	}
	return blocks
}

fn process_blocks(mut blocks []u64, time u32, memory u32, threads u32, mode int) {
	lanes := memory / threads
	segments := lanes / sync_points
	for pass := u32(0); pass < time; pass++ {
		for slice := u32(0); slice < sync_points; slice++ {
			for lane := u32(0); lane < threads; lane++ {
				process_segment(mut blocks, pass, slice, lane, time, memory, lanes, segments,
					threads, mode)
			}
		}
	}
}

fn process_segment(mut blocks []u64, pass u32, slice u32, lane u32, time u32, memory u32, lanes u32, segments u32, threads u32, mode int) {
	mut addresses := [block_length]u64{}
	mut input := [block_length]u64{}
	zero := [block_length]u64{}
	if mode == argon2_i || (mode == argon2_id && pass == 0 && slice < sync_points / 2) {
		input[0] = u64(pass)
		input[1] = u64(lane)
		input[2] = u64(slice)
		input[3] = u64(memory)
		input[4] = u64(time)
		input[5] = u64(mode)
	}
	mut index := u32(0)
	if pass == 0 && slice == 0 {
		index = 2
		if mode == argon2_i || mode == argon2_id {
			input[6]++
			process_block(mut addresses, input, zero)
			process_block(mut addresses, addresses, zero)
		}
	}
	mut offset := lane * lanes + slice * segments + index
	for index < segments {
		mut prev := offset - 1
		if index == 0 && slice == 0 {
			prev += lanes
		}
		mut random := u64(0)
		if mode == argon2_i || (mode == argon2_id && pass == 0 && slice < sync_points / 2) {
			if index % u32(block_length) == 0 {
				input[6]++
				process_block(mut addresses, input, zero)
				process_block(mut addresses, addresses, zero)
			}
			random = addresses[int(index % u32(block_length))]
		} else {
			random = blocks[block_offset(prev)]
		}
		new_offset := index_alpha(random, lanes, segments, threads, pass, slice, lane, index)
		process_block_xor_in_place(mut blocks, offset, prev, new_offset)
		index++
		offset++
	}
}

fn extract_key(mut blocks []u64, memory u32, threads u32, key_len u32) ![]u8 {
	lanes := memory / threads
	last_block := memory - 1
	last_base := block_offset(last_block)
	for lane := u32(0); lane < threads - 1; lane++ {
		base := block_offset(lane * lanes + lanes - 1)
		for i := 0; i < block_length; i++ {
			blocks[last_base + i] ^= blocks[base + i]
		}
	}
	mut block := []u8{len: block_bytes, init: 0}
	for i := 0; i < block_length; i++ {
		binary.little_endian_put_u64_at(mut block, blocks[last_base + i], i * 8)
	}
	return blake2b_hash(int(key_len), block)
}

@[inline]
fn block_offset(index u32) int {
	return int(index) * block_length
}

fn set_block(mut blocks []u64, index u32, data []u8) {
	base := block_offset(index)
	for i := 0; i < block_length; i++ {
		blocks[base + i] = binary.little_endian_u64_at(data, i * 8)
	}
}

fn blake2b_hash(out_len int, input []u8) ![]u8 {
	mut prefix := []u8{len: 4, init: 0}
	binary.little_endian_put_u32(mut prefix, u32(out_len))
	mut initial := prefix.clone()
	initial << input
	if out_len <= blake2b.size512 {
		mut d := blake2b.new_digest(u8(out_len), []u8{})!
		d.write(initial)!
		return d.checksum()
	}
	mut out := []u8{len: out_len, init: 0}
	mut block := blake2b.sum512(initial)
	copy(mut out[..32], block[..32])
	mut pos := 32
	for out_len - pos > blake2b.size512 {
		block = blake2b.sum512(block)
		copy(mut out[pos..pos + 32], block[..32])
		pos += 32
	}
	remaining := out_len - pos
	mut d := blake2b.new_digest(u8(remaining), []u8{})!
	d.write(block)!
	copy(mut out[pos..], d.checksum())
	return out
}

fn process_block(mut out [block_length]u64, in1 [block_length]u64, in2 [block_length]u64) {
	process_block_generic(mut out, in1, in2, false)
}

fn process_block_generic(mut out [block_length]u64, in1 [block_length]u64, in2 [block_length]u64, xor bool) {
	mut t := [block_length]u64{}
	for i := 0; i < block_length; i++ {
		t[i] = in1[i] ^ in2[i]
	}
	for i := 0; i < block_length; i += 16 {
		blamka_generic(mut t, i + 0, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8, i + 9,

			i + 10, i + 11, i + 12, i + 13, i + 14, i + 15)
	}
	for i := 0; i < block_length / 8; i += 2 {
		blamka_generic(mut t, i, i + 1, 16 + i, 16 + i + 1, 32 + i, 32 + i + 1, 48 + i, 48 + i + 1,

			64 + i, 64 + i + 1, 80 + i, 80 + i + 1, 96 + i, 96 + i + 1, 112 + i, 112 + i + 1)
	}
	for i := 0; i < block_length; i++ {
		if xor {
			out[i] ^= in1[i] ^ in2[i] ^ t[i]
		} else {
			out[i] = in1[i] ^ in2[i] ^ t[i]
		}
	}
}

fn process_block_xor_in_place(mut blocks []u64, out_index u32, in1_index u32, in2_index u32) {
	mut t := [block_length]u64{}
	out_base := block_offset(out_index)
	in1_base := block_offset(in1_index)
	in2_base := block_offset(in2_index)
	for i := 0; i < block_length; i++ {
		t[i] = blocks[in1_base + i] ^ blocks[in2_base + i]
	}
	for i := 0; i < block_length; i += 16 {
		blamka_generic(mut t, i + 0, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8, i + 9,

			i + 10, i + 11, i + 12, i + 13, i + 14, i + 15)
	}
	for i := 0; i < block_length / 8; i += 2 {
		blamka_generic(mut t, i, i + 1, 16 + i, 16 + i + 1, 32 + i, 32 + i + 1, 48 + i, 48 + i + 1,

			64 + i, 64 + i + 1, 80 + i, 80 + i + 1, 96 + i, 96 + i + 1, 112 + i, 112 + i + 1)
	}
	for i := 0; i < block_length; i++ {
		blocks[out_base + i] ^= blocks[in1_base + i] ^ blocks[in2_base + i] ^ t[i]
	}
}

fn blamka_generic(mut block [block_length]u64, a int, b int, c int, d int, e int, f int, g int, h int, i int, j int, k int, l int, m int, n int, o int, p int) {
	mut v00 := block[a]
	mut v01 := block[b]
	mut v02 := block[c]
	mut v03 := block[d]
	mut v04 := block[e]
	mut v05 := block[f]
	mut v06 := block[g]
	mut v07 := block[h]
	mut v08 := block[i]
	mut v09 := block[j]
	mut v10 := block[k]
	mut v11 := block[l]
	mut v12 := block[m]
	mut v13 := block[n]
	mut v14 := block[o]
	mut v15 := block[p]

	v00 += v04 + 2 * u64(u32(v00)) * u64(u32(v04))
	v12 ^= v00
	v12 = bits.rotate_left_64(v12, -32)
	v08 += v12 + 2 * u64(u32(v08)) * u64(u32(v12))
	v04 ^= v08
	v04 = bits.rotate_left_64(v04, -24)

	v00 += v04 + 2 * u64(u32(v00)) * u64(u32(v04))
	v12 ^= v00
	v12 = bits.rotate_left_64(v12, -16)
	v08 += v12 + 2 * u64(u32(v08)) * u64(u32(v12))
	v04 ^= v08
	v04 = bits.rotate_left_64(v04, -63)

	v01 += v05 + 2 * u64(u32(v01)) * u64(u32(v05))
	v13 ^= v01
	v13 = bits.rotate_left_64(v13, -32)
	v09 += v13 + 2 * u64(u32(v09)) * u64(u32(v13))
	v05 ^= v09
	v05 = bits.rotate_left_64(v05, -24)

	v01 += v05 + 2 * u64(u32(v01)) * u64(u32(v05))
	v13 ^= v01
	v13 = bits.rotate_left_64(v13, -16)
	v09 += v13 + 2 * u64(u32(v09)) * u64(u32(v13))
	v05 ^= v09
	v05 = bits.rotate_left_64(v05, -63)

	v02 += v06 + 2 * u64(u32(v02)) * u64(u32(v06))
	v14 ^= v02
	v14 = bits.rotate_left_64(v14, -32)
	v10 += v14 + 2 * u64(u32(v10)) * u64(u32(v14))
	v06 ^= v10
	v06 = bits.rotate_left_64(v06, -24)

	v02 += v06 + 2 * u64(u32(v02)) * u64(u32(v06))
	v14 ^= v02
	v14 = bits.rotate_left_64(v14, -16)
	v10 += v14 + 2 * u64(u32(v10)) * u64(u32(v14))
	v06 ^= v10
	v06 = bits.rotate_left_64(v06, -63)

	v03 += v07 + 2 * u64(u32(v03)) * u64(u32(v07))
	v15 ^= v03
	v15 = bits.rotate_left_64(v15, -32)
	v11 += v15 + 2 * u64(u32(v11)) * u64(u32(v15))
	v07 ^= v11
	v07 = bits.rotate_left_64(v07, -24)

	v03 += v07 + 2 * u64(u32(v03)) * u64(u32(v07))
	v15 ^= v03
	v15 = bits.rotate_left_64(v15, -16)
	v11 += v15 + 2 * u64(u32(v11)) * u64(u32(v15))
	v07 ^= v11
	v07 = bits.rotate_left_64(v07, -63)

	v00 += v05 + 2 * u64(u32(v00)) * u64(u32(v05))
	v15 ^= v00
	v15 = bits.rotate_left_64(v15, -32)
	v10 += v15 + 2 * u64(u32(v10)) * u64(u32(v15))
	v05 ^= v10
	v05 = bits.rotate_left_64(v05, -24)

	v00 += v05 + 2 * u64(u32(v00)) * u64(u32(v05))
	v15 ^= v00
	v15 = bits.rotate_left_64(v15, -16)
	v10 += v15 + 2 * u64(u32(v10)) * u64(u32(v15))
	v05 ^= v10
	v05 = bits.rotate_left_64(v05, -63)

	v01 += v06 + 2 * u64(u32(v01)) * u64(u32(v06))
	v12 ^= v01
	v12 = bits.rotate_left_64(v12, -32)
	v11 += v12 + 2 * u64(u32(v11)) * u64(u32(v12))
	v06 ^= v11
	v06 = bits.rotate_left_64(v06, -24)

	v01 += v06 + 2 * u64(u32(v01)) * u64(u32(v06))
	v12 ^= v01
	v12 = bits.rotate_left_64(v12, -16)
	v11 += v12 + 2 * u64(u32(v11)) * u64(u32(v12))
	v06 ^= v11
	v06 = bits.rotate_left_64(v06, -63)

	v02 += v07 + 2 * u64(u32(v02)) * u64(u32(v07))
	v13 ^= v02
	v13 = bits.rotate_left_64(v13, -32)
	v08 += v13 + 2 * u64(u32(v08)) * u64(u32(v13))
	v07 ^= v08
	v07 = bits.rotate_left_64(v07, -24)

	v02 += v07 + 2 * u64(u32(v02)) * u64(u32(v07))
	v13 ^= v02
	v13 = bits.rotate_left_64(v13, -16)
	v08 += v13 + 2 * u64(u32(v08)) * u64(u32(v13))
	v07 ^= v08
	v07 = bits.rotate_left_64(v07, -63)

	v03 += v04 + 2 * u64(u32(v03)) * u64(u32(v04))
	v14 ^= v03
	v14 = bits.rotate_left_64(v14, -32)
	v09 += v14 + 2 * u64(u32(v09)) * u64(u32(v14))
	v04 ^= v09
	v04 = bits.rotate_left_64(v04, -24)

	v03 += v04 + 2 * u64(u32(v03)) * u64(u32(v04))
	v14 ^= v03
	v14 = bits.rotate_left_64(v14, -16)
	v09 += v14 + 2 * u64(u32(v09)) * u64(u32(v14))
	v04 ^= v09
	v04 = bits.rotate_left_64(v04, -63)

	block[a] = v00
	block[b] = v01
	block[c] = v02
	block[d] = v03
	block[e] = v04
	block[f] = v05
	block[g] = v06
	block[h] = v07
	block[i] = v08
	block[j] = v09
	block[k] = v10
	block[l] = v11
	block[m] = v12
	block[n] = v13
	block[o] = v14
	block[p] = v15
}

fn index_alpha(random u64, lanes u32, segments u32, threads u32, pass u32, slice u32, lane u32, index u32) u32 {
	mut ref_lane := u32(random >> 32) % threads
	if pass == 0 && slice == 0 {
		ref_lane = lane
	}
	mut m := u32(3) * segments
	mut s := ((slice + 1) % sync_points) * segments
	if lane == ref_lane {
		m += index
	}
	if pass == 0 {
		m = slice * segments
		s = 0
		if slice == 0 || lane == ref_lane {
			m += index
		}
	}
	if index == 0 || lane == ref_lane {
		m--
	}
	return phi(random, u64(m), u64(s), ref_lane, lanes)
}

fn phi(random u64, m u64, s u64, lane u32, lanes u32) u32 {
	mut p := random & u64(0xffffffff)
	p = (p * p) >> 32
	p = (p * m) >> 32
	return lane * lanes + u32((s + m - (p + 1)) % u64(lanes))
}

fn encode_hash(mode int, salt []u8, hash []u8, time u32, memory u32, threads u8) string {
	salt_b64 := base64.encode(salt).replace_each(['=', ''])
	hash_b64 := base64.encode(hash).replace_each(['=', ''])
	return '\$${mode_name(mode)}\$v=${version}\$m=${memory},t=${time},p=${threads}\$${salt_b64}\$${hash_b64}'
}

fn decode_hash(encoded string) !DecodedHash {
	parts := encoded.split('\$')
	if parts.len != 6 || parts[0] != '' {
		return InvalidHashError{
			reason: 'invalid argon2 hash format'
		}
	}
	if parts[2] != 'v=${version}' {
		return InvalidHashError{
			reason: 'unsupported argon2 hash version'
		}
	}
	mode := parse_mode(parts[1])!
	mut memory := u32(0)
	mut time := u32(0)
	mut threads := u8(0)
	for param in parts[3].split(',') {
		name := param.all_before('=')
		value := param.all_after('=')
		match name {
			'm' {
				memory = value.u32()
			}
			't' {
				time = value.u32()
			}
			'p' {
				threads = u8(value.int())
			}
			else {
				return InvalidHashError{
					reason: 'unsupported argon2 parameter ${name}'
				}
			}
		}
	}
	if time == 0 || threads == 0 {
		return InvalidHashError{
			reason: 'invalid argon2 parameters'
		}
	}
	salt := decode_base64(parts[4])
	hash := decode_base64(parts[5])
	if salt.len == 0 || hash.len == 0 {
		return InvalidHashError{
			reason: 'invalid argon2 hash payload'
		}
	}
	return DecodedHash{
		mode:    mode
		time:    time
		memory:  normalize_memory(memory, u32(threads))
		threads: threads
		salt:    salt
		hash:    hash
	}
}

fn decode_base64(value string) []u8 {
	mut padded := value
	match padded.len % 4 {
		2 { padded += '==' }
		3 { padded += '=' }
		else {}
	}
	return base64.decode(padded)
}

fn mode_name(mode int) string {
	return match mode {
		argon2_d { 'argon2d' }
		argon2_i { 'argon2i' }
		else { 'argon2id' }
	}
}

fn parse_mode(name string) !int {
	return match name {
		'argon2d' {
			argon2_d
		}
		'argon2i' {
			argon2_i
		}
		'argon2id' {
			argon2_id
		}
		else {
			return InvalidHashError{
				reason: 'unsupported argon2 algorithm ${name}'
			}
		}
	}
}

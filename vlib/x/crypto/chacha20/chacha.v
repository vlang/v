// Copyright (c) 2024 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Chacha20 symmetric key stream cipher encryption based on RFC 8439
module chacha20

import math.bits
import crypto.internal.subtle
import encoding.binary

// The size of ChaCha20 key, ie 256 bits size, in bytes
pub const key_size = 32
// The size of standard IETF ChaCha20 nonce, ie 96 bits size, in bytes
pub const nonce_size = 12
// The size of extended variant of standard ChaCha20 (XChaCha20) nonce, 192 bits
pub const x_nonce_size = 24
// The size of original ChaCha20 nonce, 64 bits
pub const orig_nonce_size = 8
// internal block size ChaCha20 operates on, in bytes
const block_size = 64

// four constants of ChaCha20 state.
const cc0 = u32(0x61707865) // expa
const cc1 = u32(0x3320646e) // nd 3
const cc2 = u32(0x79622d32) // 2-by
const cc3 = u32(0x6b206574) // te k

// CipherMode was enumeration of ChaCha20 supported variant.
enum CipherMode {
	// The standard IETF ChaCha20 (and XChaCha20), with 32-bit internal counter.
	standard
	// The original ChaCha20 with 64-bit internal counter.
	original
}

// Cipher represents ChaCha20 stream cipher instances.
pub struct Cipher {
	// The mode of ChaCha20 cipher, set on cipher's creation.
	mode CipherMode = .standard
mut:
	// The internal's of ChaCha20 states contains 512 bits (64 bytes), contains of
	// 4 words (16 bytes) of ChaCha20 constants,
	// 8 words (32 bytes) of ChaCha20 keys,
	// 4 words (16 bytes) of raw nonces, with internal counter, support for 32 and 64 bit counters.
	key   [8]u32
	nonce [4]u32

	// Flag that tells whether this cipher was an extended XChaCha20 standard variant.
	// only make sense when mode == .standard
	extended bool

	// internal buffer for storing key stream results
	block []u8 = []u8{len: block_size}
	// The last length of leftover unprocessed keystream from internal buffer
	length int

	// Additional fields, follows the go version. Its mainly used to optimize
	// standard IETF ciphers operations by pre-chache some quarter_round step.
	// vfmt off
	precomp bool
	p1  u32 p5  u32 p9  u32 p13 u32
	p2  u32 p6  u32 p10 u32 p14 u32
	p3  u32 p7  u32 p11 u32 p15 u32
	// vfmt on
}

// new_cipher creates a new ChaCha20 stream cipher with the given 32 bytes key
// and bytes of nonce with supported size, ie, 8, 12 or 24 bytes nonce.
// Standard IETF variant use 12 bytes nonce's, if you want create original ChaCha20 cipher
// with support for 64-bit counter, use 8 bytes length nonce's instead
// If 24 bytes of nonce was provided, the XChaCha20 construction will be used.
// It returns new ChaCha20 cipher instance or an error if key or nonce have any other length.
@[direct_array_access]
pub fn new_cipher(key []u8, nonce []u8) !&Cipher {
	if key.len != key_size {
		return error('Bad key size provided')
	}
	mut mode := CipherMode.standard
	mut extended := false
	match nonce.len {
		nonce_size {}
		x_nonce_size {
			extended = true
		}
		orig_nonce_size {
			mode = .original
		}
		else {
			return error('Unsupported nonce size')
		}
	}
	mut c := &Cipher{
		mode:     mode
		extended: extended
	}
	// we dont need reset on new cipher instance
	c.do_rekey(key, nonce)!

	return c
}

// encrypt encrypts plaintext bytes with ChaCha20 cipher instance with provided key and nonce.
// It was a thin wrapper around two supported nonce size, ChaCha20 with 96 bits
// and XChaCha20 with 192 bits nonce. Internally, encrypt start with 0's counter value.
// If you want more control, use Cipher instance and setup the counter by your self.
pub fn encrypt(key []u8, nonce []u8, plaintext []u8) ![]u8 {
	mut c := new_cipher(key, nonce)!
	mut out := []u8{len: plaintext.len}

	c.encrypt(mut out, plaintext)
	unsafe { c.reset() }
	return out
}

// decrypt does reverse of encrypt operation by decrypting ciphertext with ChaCha20 cipher
// instance with provided key and nonce.
pub fn decrypt(key []u8, nonce []u8, ciphertext []u8) ![]u8 {
	mut c := new_cipher(key, nonce)!
	mut out := []u8{len: ciphertext.len}

	c.encrypt(mut out, ciphertext)
	unsafe { c.reset() }
	return out
}

// xor_key_stream xors each byte in the given slice in the src with a byte from the
// cipher's key stream. It fulfills `cipher.Stream` interface. It encrypts the plaintext message
// in src and stores the ciphertext result in dst in a key stream fashion.
// You must never use the same (key, nonce) pair more than once for encryption.
// This would void any confidentiality guarantees for the messages encrypted with the same nonce and key.
@[direct_array_access]
pub fn (mut c Cipher) xor_key_stream(mut dst []u8, src []u8) {
	if src.len == 0 {
		return
	}
	if dst.len < src.len {
		panic('chacha20/chacha: dst buffer is to small')
	}

	mut idx := 0
	mut src_len := src.len
	dst = unsafe { dst[..src_len] }

	if subtle.inexact_overlap(dst, src) {
		panic('chacha20: invalid buffer overlap')
	}

	// We adapt and ports the go version here
	// First, drain any remaining key stream
	if c.length != 0 {
		// remaining keystream on internal buffer
		mut kstream := c.block[block_size - c.length..]
		if src_len < kstream.len {
			kstream = unsafe { kstream[..src_len] }
		}
		for i, b in kstream {
			dst[idx + i] = src[idx + i] ^ b
		}
		// updates the idx for dst and src
		c.length -= kstream.len
		idx += kstream.len
		src_len -= kstream.len
	}
	if src_len == 0 {
		return
	}

	// check for counter overflow
	num_blocks := (u64(src_len) + block_size - 1) / block_size
	if c.check_for_ctr_overflow(num_blocks) {
		panic('chacha20: internal counter overflow')
	}

	// take the most full bytes of multiples block_size from the src,
	// build the keystream from the cipher's state and stores the result
	// into dst
	full := src_len - src_len % block_size
	if full > 0 {
		c.chacha20_block_generic(mut dst[idx..idx + full], src[idx..idx + full])
	}
	idx += full
	src_len -= full

	// If we have a partial block, pad it for chacha20_block_generic, and
	// keep the leftover keystream for the next invocation.
	if src_len > 0 {
		// Make sure, internal buffer cleared with the new one
		// or the old garbaged data from previous call still there
		// See https://github.com/vlang/v/issues/24043
		c.block = []u8{len: block_size}
		// copy the last src block to internal buffer, and performs
		// chacha20_block_generic on this buffer, and stores into remaining dst
		_ := copy(mut c.block, src[idx..])
		c.chacha20_block_generic(mut c.block, c.block)
		n := copy(mut dst[idx..], c.block)
		// the length of remaining bytes of unprocessed keystream
		c.length = block_size - n
	}
}

// encrypt encrypts src and stores into dst buffer. It works like `xor_key_stream` except
// its ignore key streaming process by ignoring remaining key stream in the internal buffer,
// so, its works in one shot of fashion.
// Its added to allow `chacha20poly1305` modules to work without key stream fashion.
// TODO: integrates it with the rest
@[direct_array_access]
pub fn (mut c Cipher) encrypt(mut dst []u8, src []u8) {
	if src.len == 0 {
		return
	}
	if dst.len < src.len {
		panic('chacha20/chacha: dst buffer is to small')
	}
	if subtle.inexact_overlap(dst, src) {
		panic('chacha20: invalid buffer overlap')
	}

	nr_blocks := src.len / block_size
	for i := 0; i < nr_blocks; i++ {
		// get current src block to be xor-ed
		block := unsafe { src[i * block_size..(i + 1) * block_size] }
		// build keystream, xor-ed with the block and stores into dst
		c.chacha20_block_generic(mut dst[i * block_size..(i + 1) * block_size], block)
	}
	// process for partial block
	if src.len % block_size != 0 {
		// get the remaining last partial block
		block := unsafe { src[nr_blocks * block_size..] }
		// pad it into block_size, and then performs chacha20_block_generic
		// on this src_block
		mut src_block := []u8{len: block_size}
		_ := copy(mut src_block, block)
		c.chacha20_block_generic(mut src_block, src_block)

		// copy the src_block key stream result into desired dst
		n := copy(mut dst[nr_blocks * block_size..], src_block)
		assert n == block.len
	}
}

// chacha20_block_generic generates a generic ChaCha20  keystream.
// This is main building block for ChaCha20 keystream generator.
// This routine was intended to work only for msg source with multiples of block_size in size.
@[direct_array_access]
fn (mut c Cipher) chacha20_block_generic(mut dst []u8, src []u8) {
	// ChaCha20 keystream generator was relatively easy to understand.
	// Its contains steps:
	// - Loads current ChaCha20 into temporary state, used for later.
	// - Performs quarter_round function on this state and returns some new state.
	// - Adds back the new state with the old state.
	// - Performs xor-ing between src bytes (loaded as little endian number) with result from previous step.
	// - Serializes, in little endian form, this xor-ed state into destination buffer.
	//
	// Makes sure its works for size of multiple of block_size
	if dst.len != src.len || dst.len % block_size != 0 {
		panic('chacha20: internal error: wrong dst and/or src length')
	}
	// check for counter overflow
	num_blocks := u64((src.len + block_size - 1) / block_size)
	if c.check_for_ctr_overflow(num_blocks) {
		panic('chacha20: internal counter overflow')
	}

	// initializes ChaCha20 state
	//      0:cccccccc   1:cccccccc   2:cccccccc   3:cccccccc
	//      4:kkkkkkkk   5:kkkkkkkk   6:kkkkkkkk   7:kkkkkkkk
	//      8:kkkkkkkk   9:kkkkkkkk  10:kkkkkkkk  11:kkkkkkkk
	//     12:bbbbbbbb  13:nnnnnnnn  14:nnnnnnnn  15:nnnnnnnn
	//
	// where c=constant k=key b=blockcounter n=nonce
	c0, c1, c2, c3 := cc0, cc1, cc2, cc3
	c4, c5, c6, c7 := c.key[0], c.key[1], c.key[2], c.key[3]
	c8, c9, c10, c11 := c.key[4], c.key[5], c.key[6], c.key[7]

	// internal cipher's counter
	mut c12 := c.nonce[0]
	mut c13 := c.nonce[1]

	c14, c15 := c.nonce[2], c.nonce[3]

	// copy current cipher's states into temporary states
	mut x0, mut x1, mut x2, mut x3 := c0, c1, c2, c3
	mut x4, mut x5, mut x6, mut x7 := c4, c5, c6, c7
	mut x8, mut x9, mut x10, mut x11 := c8, c9, c10, c11
	mut x12, mut x13, mut x14, mut x15 := c12, c13, c14, c15

	// this only for standard mode
	if c.mode == .standard {
		// precomputes three first column rounds that do not depend on counter
		if !c.precomp {
			c.p1, c.p5, c.p9, c.p13 = quarter_round(c1, c5, c9, c13)
			c.p2, c.p6, c.p10, c.p14 = quarter_round(c2, c6, c10, c14)
			c.p3, c.p7, c.p11, c.p15 = quarter_round(c3, c7, c11, c15)
			c.precomp = true
		}
	}

	mut idx := 0
	mut src_len := src.len
	for src_len >= block_size {
		if c.mode == .standard {
			// this for standard mode
			// remaining first column round
			fcr0, fcr4, fcr8, fcr12 := quarter_round(c0, c4, c8, c12)

			// The second diagonal round.
			x0, x5, x10, x15 = quarter_round(fcr0, c.p5, c.p10, c.p15)
			x1, x6, x11, x12 = quarter_round(c.p1, c.p6, c.p11, fcr12)
			x2, x7, x8, x13 = quarter_round(c.p2, c.p7, fcr8, c.p13)
			x3, x4, x9, x14 = quarter_round(c.p3, fcr4, c.p9, c.p14)
		}

		// The remaining rounds, for standard its already precomputed,
		// for original, its use full quarter round
		n := if c.mode == .standard { 9 } else { 10 }
		for i := 0; i < n; i++ {
			// Column round.
			//  0 |  1 |  2 |  3
			//  4 |  5 |  6 |  7
			//  8 |  9 | 10 | 11
			// 12 | 13 | 14 | 15
			x0, x4, x8, x12 = quarter_round(x0, x4, x8, x12)
			x1, x5, x9, x13 = quarter_round(x1, x5, x9, x13)
			x2, x6, x10, x14 = quarter_round(x2, x6, x10, x14)
			x3, x7, x11, x15 = quarter_round(x3, x7, x11, x15)

			// Diagonal round.
			//   0 \  1 \  2 \  3
			//   5 \  6 \  7 \  4
			//  10 \ 11 \  8 \  9
			//  15 \ 12 \ 13 \ 14
			x0, x5, x10, x15 = quarter_round(x0, x5, x10, x15)
			x1, x6, x11, x12 = quarter_round(x1, x6, x11, x12)
			x2, x7, x8, x13 = quarter_round(x2, x7, x8, x13)
			x3, x4, x9, x14 = quarter_round(x3, x4, x9, x14)
		}

		// add back keystream result to initial state, xor-ing with the src and stores into dst
		binary.little_endian_put_u32(mut dst[idx + 0..idx + 4], binary.little_endian_u32(src[idx + 0..
			idx + 4]) ^ (x0 + c0))
		binary.little_endian_put_u32(mut dst[idx + 4..idx + 8], binary.little_endian_u32(src[idx + 4..
			idx + 8]) ^ (x1 + c1))
		binary.little_endian_put_u32(mut dst[idx + 8..idx + 12], binary.little_endian_u32(src[idx +
			8..idx + 12]) ^ (x2 + c2))
		binary.little_endian_put_u32(mut dst[idx + 12..idx + 16], binary.little_endian_u32(src[
			idx + 12..idx + 16]) ^ (x3 + c3))
		binary.little_endian_put_u32(mut dst[idx + 16..idx + 20], binary.little_endian_u32(src[
			idx + 16..idx + 20]) ^ (x4 + c4))
		binary.little_endian_put_u32(mut dst[idx + 20..idx + 24], binary.little_endian_u32(src[
			idx + 20..idx + 24]) ^ (x5 + c5))
		binary.little_endian_put_u32(mut dst[idx + 24..idx + 28], binary.little_endian_u32(src[
			idx + 24..idx + 28]) ^ (x6 + c6))
		binary.little_endian_put_u32(mut dst[idx + 28..idx + 32], binary.little_endian_u32(src[
			idx + 28..idx + 32]) ^ (x7 + c7))
		binary.little_endian_put_u32(mut dst[idx + 32..idx + 36], binary.little_endian_u32(src[
			idx + 32..idx + 36]) ^ (x8 + c8))
		binary.little_endian_put_u32(mut dst[idx + 36..idx + 40], binary.little_endian_u32(src[
			idx + 36..idx + 40]) ^ (x9 + c9))
		binary.little_endian_put_u32(mut dst[idx + 40..idx + 44], binary.little_endian_u32(src[
			idx + 40..idx + 44]) ^ (x10 + c10))
		binary.little_endian_put_u32(mut dst[idx + 44..idx + 48], binary.little_endian_u32(src[
			idx + 44..idx + 48]) ^ (x11 + c11))
		binary.little_endian_put_u32(mut dst[idx + 48..idx + 52], binary.little_endian_u32(src[
			idx + 48..idx + 52]) ^ (x12 + c12))
		binary.little_endian_put_u32(mut dst[idx + 52..idx + 56], binary.little_endian_u32(src[
			idx + 52..idx + 56]) ^ (x13 + c13))
		binary.little_endian_put_u32(mut dst[idx + 56..idx + 60], binary.little_endian_u32(src[
			idx + 56..idx + 60]) ^ (x14 + c14))
		binary.little_endian_put_u32(mut dst[idx + 60..idx + 64], binary.little_endian_u32(src[
			idx + 60..idx + 64]) ^ (x15 + c15))

		// Updates internal counter
		//
		// Its safe to update internal counter, its already checked before.
		if c.mode == .original {
			mut curr_ctr := u64(c13) << 32 | u64(c12)
			curr_ctr += 1
			// stores back the counter
			c.nonce[0] = u32(curr_ctr)
			c.nonce[1] = u32(curr_ctr >> 32)
		} else {
			c12 += 1
			c.nonce[0] = c12
		}

		idx += block_size
		src_len -= block_size
	}
}

// free the resources taken by the Cipher `c`. Dont use cipher after .free call
@[unsafe]
pub fn (mut c Cipher) free() {
	$if prealloc {
		return
	}
	unsafe {
		c.block.free()
	}
}

// reset quickly sets all Cipher's fields to default value
@[unsafe]
pub fn (mut c Cipher) reset() {
	unsafe {
		_ := vmemset(&c.key, 0, 32)
		_ := vmemset(&c.nonce, 0, 16)
		c.block.reset()
	}
	c.length = 0
	c.precomp = false

	c.p1, c.p5, c.p9, c.p13 = u32(0), u32(0), u32(0), u32(0)
	c.p2, c.p6, c.p10, c.p14 = u32(0), u32(0), u32(0), u32(0)
	c.p3, c.p7, c.p11, c.p15 = u32(0), u32(0), u32(0), u32(0)
}

// set_counter sets Cipher's counter
@[direct_array_access; inline]
pub fn (mut c Cipher) set_counter(ctr u64) {
	match c.mode {
		.original {
			c.nonce[0] = u32(ctr)
			c.nonce[1] = u32(ctr >> 32)
		}
		.standard {
			// check for ctr value that may exceed the counter limit
			if ctr > max_32bit_counter {
				panic('set_counter: counter value exceed the limit ')
			}
			c.nonce[0] = u32(ctr)
		}
	}
}

// rekey resets internal Cipher's state and reinitializes state with the provided key and nonce
pub fn (mut c Cipher) rekey(key []u8, nonce []u8) ! {
	unsafe { c.reset() }
	// this routine was publicly accessible to user, so we add a check here
	// to ensure the supplied key and nonce has the correct size.
	if key.len != key_size {
		return error('Bad key size provided for rekey')
	}
	// For the standard cipher, allowed nonce size was nonce_size or x_nonce_size
	if c.mode == .standard {
		if nonce.len != x_nonce_size && nonce.len != nonce_size {
			return error('Bad nonce size for standard cipher, use 12 or 24 bytes length nonce')
		}
		if c.extended && nonce.len != x_nonce_size {
			return error('Bad nonce size provided for extended variant cipher')
		}
	}
	// in the original variant, nonce should be orig_nonce_size length (8 bytes)
	if c.mode == .original && nonce.len != orig_nonce_size {
		return error('Bad nonce size provided for original mode')
	}
	c.do_rekey(key, nonce)!
}

// do_rekey reinitializes ChaCha20 instance with the provided key and nonce.
@[direct_array_access]
fn (mut c Cipher) do_rekey(key []u8, nonce []u8) ! {
	mut nonces := nonce.clone()
	mut keys := key.clone()

	// Its now awares of the new flag, mode and extended
	// If this cipher was standard mode with extended flag, derives a new key and nonce
	// for later setup 	operation
	if c.mode == .standard && c.extended {
		keys, nonces = derive_xchacha20_key_nonce(key, nonce)!
	}

	// Its shared the same cipher key setup on the both of mode.
	c.key[0] = binary.little_endian_u32(keys[0..4])
	c.key[1] = binary.little_endian_u32(keys[4..8])
	c.key[2] = binary.little_endian_u32(keys[8..12])
	c.key[3] = binary.little_endian_u32(keys[12..16])
	c.key[4] = binary.little_endian_u32(keys[16..20])
	c.key[5] = binary.little_endian_u32(keys[20..24])
	c.key[6] = binary.little_endian_u32(keys[24..28])
	c.key[7] = binary.little_endian_u32(keys[28..32])

	// first counter value
	c.nonce[0] = 0
	if c.mode == .standard {
		c.nonce[1] = binary.little_endian_u32(nonces[0..4])
		c.nonce[2] = binary.little_endian_u32(nonces[4..8])
		c.nonce[3] = binary.little_endian_u32(nonces[8..12])
	} else {
		// original mode
		// second of 64-bit counter value
		c.nonce[1] = 0

		// nonce size on original mode was 64 bits
		c.nonce[2] = binary.little_endian_u32(nonces[0..4])
		c.nonce[3] = binary.little_endian_u32(nonces[4..8])
	}
}

// Helper and core function for ChaCha20
//
// quarter_round is the basic operation of the ChaCha algorithm. It operates
// on four 32-bit unsigned integers, by performing AXR (add, xor, rotate)
// operation on this quartet u32 numbers.
fn quarter_round(a u32, b u32, c u32, d u32) (u32, u32, u32, u32) {
	// The operation is as follows (in C-like notation):
	// where `<<<=` denotes bits rotate left operation
	// a += b; d ^= a; d <<<= 16;
	// c += d; b ^= c; b <<<= 12;
	// a += b; d ^= a; d <<<= 8;
	// c += d; b ^= c; b <<<= 7;

	mut ax := a
	mut bx := b
	mut cx := c
	mut dx := d

	ax += bx
	dx ^= ax
	dx = bits.rotate_left_32(dx, 16)

	cx += dx
	bx ^= cx
	bx = bits.rotate_left_32(bx, 12)

	ax += bx
	dx ^= ax
	dx = bits.rotate_left_32(dx, 8)

	cx += dx
	bx ^= cx
	bx = bits.rotate_left_32(bx, 7)

	return ax, bx, cx, dx
}

// Cipher's counter handling routine
//
// We define counter limit to simplify the access
const max_64bit_counter = max_u64
const max_32bit_counter = u64(max_u32)

// load_ctr loads underlying cipher's counter as u64 value.
@[direct_array_access; inline]
fn (c Cipher) load_ctr() u64 {
	match c.mode {
		// In the original mode, counter was 64-bit size
		// stored on c.nonce[0], and c.nonce[1]
		.original {
			return u64(c.nonce[1]) << 32 | u64(c.nonce[0])
		}
		.standard {
			// in standard mode, counter was 32-bit value, stored on c.nonce[0]
			return u64(c.nonce[0])
		}
	}
}

// max_ctr_value returns maximum value of cipher's counter.
@[inline]
fn (c Cipher) max_ctr_value() u64 {
	match c.mode {
		.original { return max_64bit_counter }
		.standard { return max_32bit_counter }
	}
}

// derive_xchacha20_key_nonce derives a new key and nonces for extended
// variant of standard mode. Its separated for simplify the access.
@[direct_array_access; inline]
fn derive_xchacha20_key_nonce(key []u8, nonce []u8) !([]u8, []u8) {
	// Its only for x_nonce_size
	if nonce.len != x_nonce_size {
		return error('Bad nonce size for derive_xchacha20_key_nonce')
	}
	// derives a new key based on xchacha20 construction
	// first 16 bytes of nonce used to derive the key
	new_key := xchacha20(key, nonce[0..16])!
	mut new_nonce := []u8{len: nonce_size}
	// and the last of 8 bytes of nonce copied into new_nonce to build
	// nonce_size length of new_nonce
	_ := copy(mut new_nonce[4..12], nonce[16..24])

	return new_key, new_nonce
}

@[direct_array_access; inline]
fn (c Cipher) check_for_ctr_overflow(add_value u64) bool {
	// check for counter overflow
	ctr := c.load_ctr()
	sum := ctr + add_value
	max := c.max_ctr_value()
	if sum < ctr || sum < add_value || sum > max {
		return true
	}
	return false
}

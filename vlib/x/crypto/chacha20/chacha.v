// Copyright (c) 2024 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Chacha20 symmetric key stream cipher encryption based on RFC 8439
module chacha20

import math.bits
import crypto.internal.subtle
import encoding.binary

// size of ChaCha20 key, ie 256 bits size, in bytes
pub const key_size = 32
// size of ietf ChaCha20 nonce, ie 96 bits size, in bytes
pub const nonce_size = 12
// size of extended ChaCha20 nonce, called XChaCha20, 192 bits
pub const x_nonce_size = 24
// internal block size ChaCha20 operates on, in bytes
const block_size = 64

// vfmt off

// four constants of ChaCha20 state.
const cc0 = u32(0x61707865) // expa
const cc1 = u32(0x3320646e) // nd 3
const cc2 = u32(0x79622d32) // 2-by
const cc3 = u32(0x6b206574) // te k

// Cipher represents ChaCha20 stream cipher instances.
pub struct Cipher {
mut:
	// internal's of ChaCha20 states, ie, 16 of u32 words, 4 of ChaCha20 constants,
	// 8 word (32 bytes) of keys, 3 word (24 bytes) of nonces and 1 word of counter
	key      [8]u32
	nonce    [3]u32
	counter  u32
	overflow bool
	// internal buffer for storing key stream results
	block []u8 = []u8{len: chacha20.block_size}
	length int 
	// additional fields, follow the go version
	precomp bool
	p1  u32 p5  u32 p9  u32 p13 u32
	p2  u32 p6  u32 p10 u32 p14 u32
	p3  u32 p7  u32 p11 u32 p15 u32
}
// vfmt on

// new_cipher creates a new ChaCha20 stream cipher with the given 32 bytes key, a 12 or 24 bytes nonce.
// If 24 bytes of nonce was provided, the XChaCha20 construction will be used.
// It returns new ChaCha20 cipher instance or an error if key or nonce have any other length.
pub fn new_cipher(key []u8, nonce []u8) !&Cipher {
	mut c := &Cipher{}
	// we dont need reset on new cipher instance
	c.do_rekey(key, nonce)!

	return c
}

// encrypt encrypts plaintext bytes with ChaCha20 cipher instance with provided key and nonce.
// It was a thin wrapper around two supported nonce size, ChaCha20 with 96 bits
// and XChaCha20 with 192 bits nonce. Internally, encrypt start with 0's counter value.
// If you want more control, use Cipher instance and setup the counter by your self.
pub fn encrypt(key []u8, nonce []u8, plaintext []u8) ![]u8 {
	return encrypt_with_counter(key, nonce, u32(0), plaintext)
}

// decrypt does reverse of encrypt operation by decrypting ciphertext with ChaCha20 cipher
// instance with provided key and nonce.
pub fn decrypt(key []u8, nonce []u8, ciphertext []u8) ![]u8 {
	return encrypt_with_counter(key, nonce, u32(0), ciphertext)
}

// xor_key_stream xors each byte in the given slice in the src with a byte from the
// cipher's key stream. It fulfills `cipher.Stream` interface. It encrypts the plaintext message
// in src and stores the ciphertext result in dst in a single run of encryption.
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
	if subtle.inexact_overlap(dst, src) {
		panic('chacha20: invalid buffer overlap')
	}

	// We adapt and ports the go version here
	// First, drain any remaining key stream
	mut idx := 0
	mut src_len := src.len
	if c.length != 0 {
		mut kstream := c.block[block_size - c.length..]
		if src_len < kstream.len {
			kstream = unsafe { kstream[..src_len] }
		}
		_ = src[kstream.len - 1] // bounds check elimination hint
		for i, b in kstream {
			dst[idx + i] = src[idx + i] ^ b
		}
		c.length -= kstream.len
		idx += kstream.len
		src_len -= kstream.len
	}
	if src_len == 0 {
		return
	}

	full := src_len - src_len % block_size
	if full > 0 {
		c.chacha20_block_generic(mut dst[idx..idx + full], src[idx..idx + full])
	}
	idx += full
	src_len -= full

	// we dont support this
	if u64(c.counter) + 1 > max_u32 {
		c.block = []u8{len: block_size}
		numblocks := (src_len + block_size - 1) / block_size
		mut buf := c.block[block_size - numblocks * block_size..]
		_ := copy(mut buf, src[idx..])
		c.chacha20_block_generic(mut buf, buf)
		m := copy(mut dst[idx..], buf)
		c.length = buf.len - m
		return
	}
	// If we have a partial block, pad it for chacha20_block_generic, and
	// keep the leftover keystream for the next invocation.
	if src_len > 0 {
		c.block = []u8{len: block_size}
		_ := copy(mut c.block, src[idx..])
		c.chacha20_block_generic(mut c.block, c.block)
		x := copy(mut dst[idx..], c.block)
		c.length = block_size - x
	}
}

// chacha20_block_generic generates ChaCha20 generic keystream
@[direct_array_access]
fn (mut c Cipher) chacha20_block_generic(mut dst []u8, src []u8) {
	if dst.len != src.len || dst.len % block_size != 0 {
		panic('chacha20: internal error: wrong dst and/or src length')
	}
	// initializes ChaCha20 state
	//      0:cccccccc   1:cccccccc   2:cccccccc   3:cccccccc
	//      4:kkkkkkkk   5:kkkkkkkk   6:kkkkkkkk   7:kkkkkkkk
	//      8:kkkkkkkk   9:kkkkkkkk  10:kkkkkkkk  11:kkkkkkkk
	//     12:bbbbbbbb  13:nnnnnnnn  14:nnnnnnnn  15:nnnnnnnn
	//
	// where c=constant k=key b=blockcounter n=nonce
	c0, c1, c2, c3 := cc0, cc1, cc2, cc3
	c4 := c.key[0]
	c5 := c.key[1]
	c6 := c.key[2]
	c7 := c.key[3]
	c8 := c.key[4]
	c9 := c.key[5]
	c10 := c.key[6]
	c11 := c.key[7]

	_ := c.counter
	c13 := c.nonce[0]
	c14 := c.nonce[1]
	c15 := c.nonce[2]

	// precomputes three first column rounds that do not depend on counter
	if !c.precomp {
		c.p1, c.p5, c.p9, c.p13 = quarter_round(c1, c5, c9, c13)
		c.p2, c.p6, c.p10, c.p14 = quarter_round(c2, c6, c10, c14)
		c.p3, c.p7, c.p11, c.p15 = quarter_round(c3, c7, c11, c15)
		c.precomp = true
	}
	mut idx := 0
	mut src_len := src.len
	for src_len >= block_size {
		// remaining first column round
		fcr0, fcr4, fcr8, fcr12 := quarter_round(c0, c4, c8, c.counter)

		// The second diagonal round.
		mut x0, mut x5, mut x10, mut x15 := quarter_round(fcr0, c.p5, c.p10, c.p15)
		mut x1, mut x6, mut x11, mut x12 := quarter_round(c.p1, c.p6, c.p11, fcr12)
		mut x2, mut x7, mut x8, mut x13 := quarter_round(c.p2, c.p7, fcr8, c.p13)
		mut x3, mut x4, mut x9, mut x14 := quarter_round(c.p3, fcr4, c.p9, c.p14)

		// The remaining 18 rounds.
		for i := 0; i < 9; i++ {
			// Column round.
			x0, x4, x8, x12 = quarter_round(x0, x4, x8, x12)
			x1, x5, x9, x13 = quarter_round(x1, x5, x9, x13)
			x2, x6, x10, x14 = quarter_round(x2, x6, x10, x14)
			x3, x7, x11, x15 = quarter_round(x3, x7, x11, x15)

			// Diagonal round.
			x0, x5, x10, x15 = quarter_round(x0, x5, x10, x15)
			x1, x6, x11, x12 = quarter_round(x1, x6, x11, x12)
			x2, x7, x8, x13 = quarter_round(x2, x7, x8, x13)
			x3, x4, x9, x14 = quarter_round(x3, x4, x9, x14)
		}

		// add back to initial state, xor-ing with the src and stores to dst
		add_n_xor(mut dst[idx + 0..idx + 4], src[idx + 0..idx + 4], x0, c0)
		add_n_xor(mut dst[idx + 4..idx + 8], src[idx + 4..idx + 8], x1, c1)
		add_n_xor(mut dst[idx + 8..idx + 12], src[idx + 8..idx + 12], x2, c2)
		add_n_xor(mut dst[idx + 12..idx + 16], src[idx + 12..idx + 16], x3, c3)
		add_n_xor(mut dst[idx + 16..idx + 20], src[idx + 16..idx + 20], x4, c4)
		add_n_xor(mut dst[idx + 20..idx + 24], src[idx + 20..idx + 24], x5, c5)
		add_n_xor(mut dst[idx + 24..idx + 28], src[idx + 24..idx + 28], x6, c6)
		add_n_xor(mut dst[idx + 28..idx + 32], src[idx + 28..idx + 32], x7, c7)
		add_n_xor(mut dst[idx + 32..idx + 36], src[idx + 32..idx + 36], x8, c8)
		add_n_xor(mut dst[idx + 36..idx + 40], src[idx + 36..idx + 40], x9, c9)
		add_n_xor(mut dst[idx + 40..idx + 44], src[idx + 40..idx + 44], x10, c10)
		add_n_xor(mut dst[idx + 44..idx + 48], src[idx + 44..idx + 48], x11, c11)
		add_n_xor(mut dst[idx + 48..idx + 52], src[idx + 48..idx + 52], x12, c.counter)
		add_n_xor(mut dst[idx + 52..idx + 56], src[idx + 52..idx + 56], x13, c13)
		add_n_xor(mut dst[idx + 56..idx + 60], src[idx + 56..idx + 60], x14, c14)
		add_n_xor(mut dst[idx + 60..idx + 64], src[idx + 60..idx + 64], x15, c15)

		c.counter += 1

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
		_ := vmemset(&c.nonce, 0, 12)
		c.block.reset()
	}
	c.counter = u32(0)
	c.overflow = false
	c.precomp = false

	c.p1 = u32(0)
	c.p5 = u32(0)
	c.p9 = u32(0)
	c.p13 = u32(0)

	c.p2 = u32(0)
	c.p6 = u32(0)
	c.p10 = u32(0)
	c.p14 = u32(0)

	c.p3 = u32(0)
	c.p7 = u32(0)
	c.p11 = u32(0)
	c.p15 = u32(0)
}

// set_counter sets Cipher's counter
pub fn (mut c Cipher) set_counter(ctr u32) {
	if ctr >= max_u32 {
		c.overflow = true
	}
	if c.overflow {
		panic('counter would overflow')
	}
	c.counter = ctr
}

// rekey resets internal Cipher's state and reinitializes state with the provided key and nonce
pub fn (mut c Cipher) rekey(key []u8, nonce []u8) ! {
	unsafe { c.reset() }
	c.do_rekey(key, nonce)!
}

// do_rekey reinitializes ChaCha20 instance with the provided key and nonce.
@[direct_array_access]
fn (mut c Cipher) do_rekey(key []u8, nonce []u8) ! {
	// check for correctness of key and nonce length
	if key.len != key_size {
		return error('chacha20: bad key size provided ')
	}
	// check for nonce's length is 12 or 24
	if nonce.len != nonce_size && nonce.len != x_nonce_size {
		return error('chacha20: bad nonce size provided')
	}
	mut nonces := nonce.clone()
	mut keys := key.clone()

	// if nonce's length is 24 bytes, we derive a new key and nonce with xchacha20 function
	// and supplied to setup process.
	if nonces.len == x_nonce_size {
		keys = xchacha20(keys, nonces[0..16])!
		mut cnonce := []u8{len: nonce_size}
		_ := copy(mut cnonce[4..12], nonces[16..24])
		nonces = cnonce.clone()
	} else if nonces.len != nonce_size {
		return error('chacha20: wrong nonce size')
	}

	// bounds check elimination hint
	_ = keys[key_size - 1]
	_ = nonces[nonce_size - 1]

	// setup ChaCha20 cipher key
	c.key[0] = binary.little_endian_u32(keys[0..4])
	c.key[1] = binary.little_endian_u32(keys[4..8])
	c.key[2] = binary.little_endian_u32(keys[8..12])
	c.key[3] = binary.little_endian_u32(keys[12..16])
	c.key[4] = binary.little_endian_u32(keys[16..20])
	c.key[5] = binary.little_endian_u32(keys[20..24])
	c.key[6] = binary.little_endian_u32(keys[24..28])
	c.key[7] = binary.little_endian_u32(keys[28..32])

	// setup ChaCha20 cipher nonce
	c.nonce[0] = binary.little_endian_u32(nonces[0..4])
	c.nonce[1] = binary.little_endian_u32(nonces[4..8])
	c.nonce[2] = binary.little_endian_u32(nonces[8..12])
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

// encrypt_with_counter encrypts plaintext with internal counter set to ctr
fn encrypt_with_counter(key []u8, nonce []u8, ctr u32, plaintext []u8) ![]u8 {
	if key.len != key_size {
		return error('bad key size')
	}
	if nonce.len == x_nonce_size {
		ciphertext := xchacha20_encrypt_with_counter(key, nonce, ctr, plaintext)!
		return ciphertext
	}
	if nonce.len == nonce_size {
		ciphertext := chacha20_encrypt_with_counter(key, nonce, ctr, plaintext)!
		return ciphertext
	}
	return error('Wrong nonce size')
}

fn chacha20_encrypt(key []u8, nonce []u8, plaintext []u8) ![]u8 {
	return chacha20_encrypt_with_counter(key, nonce, u32(0), plaintext)
}

fn chacha20_encrypt_with_counter(key []u8, nonce []u8, ctr u32, plaintext []u8) ![]u8 {
	mut c := new_cipher(key, nonce)!
	c.set_counter(ctr)
	mut out := []u8{len: plaintext.len}

	c.xor_key_stream(mut out, plaintext)

	return out
}

// add_n_xor adds a+b, xor it with src and stores into dst
fn add_n_xor(mut dst []u8, src []u8, a u32, b u32) {
	_, _ = dst[3], src[3]
	v := binary.little_endian_u32(src[0..4]) ^ (a + b)
	binary.little_endian_put_u32(mut dst[0..4], v)
}

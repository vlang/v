// Copyright (c) 2026 blackshirt. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// AES-GCM (NIST SP 800-38D) built on top of the pure-V `crypto.aes` block
// cipher. Its build to support for QUIC protocol and another cases.
// QUIC (RFC 9001) mandates AES-128-GCM for Initial packets and allows
// AES-128-GCM / AES-256-GCM for 1-RTT packets. The GHASH multiplication uses
// the straightforward bit-by-bit algorithm, which is constant in structure and
// easy to audit (performance is adequate for handshakes and modest streams).
module aes

import crypto.cipher

// gcm_tag_size is the size of the GCM authentication tag in bytes.
pub const gcm_tag_size = 16

// gcm_nonce_size is the size of the GCM nonce in bytes (only support 96-bit nonce variant).
pub const gcm_nonce_size = 12

// zero_block is a preallocated 16-bytes of zeros block. Its currently sized by Aes block size.
const zero_block = []u8{len: block_size}

// maximum plaintext length per invocation GCM: MUST be <= 2^36 - 32 octets.
const max_plaintext_size = u64(1) << 36 - 32

// maximum ciphertext length per invocation GCM: MUST be <= 2^36 - 16 octets.
const max_ciphertext_size = u64(1) << 36 - 16

// maximum length of additional authenticated data. Even technically its was not limited,
// we give it a limit to reduce the risk. Intended purposes of additional data was
// as an additional authenticated data included into output, not act as a primary input.
const max_aad_size = max_u32

// AesGcm holds the AES block cipher and the derived GHASH subkey for a key.
@[heap; noinit]
pub struct AesGcm implements cipher.AEAD {
mut:
	block cipher.Block
	h     []u8 // GHASH subkey H = E(K, 0^128)
}

// new_aes_gcm builds AES-GCM state for a 16, 24 or 32-byte key.
@[direct_array_access]
pub fn new_aes_gcm(key []u8) !&AesGcm {
	if key.len != 16 && key.len != 24 && key.len != 32 {
		return error('AES-GCM key must be 16, 24 or 32 bytes, got ${key.len}')
	}
	block := new_cipher(key)
	mut h := zero_block.clone()
	block.encrypt(mut h, zero_block)
	return &AesGcm{
		block: block
		h:     h
	}
}

// nonce_size returns the size of nonce (in bytes).
pub fn (g &AesGcm) nonce_size() int {
	return gcm_nonce_size
}

// overhead returns the maximum difference between the lengths of a plaintext and its ciphertext.
pub fn (g &AesGcm) overhead() int {
	return gcm_tag_size
}

// encrypt encrypts `plaintext` with the given 12-byte `nonce` and additional
// authenticated data `ad`, returning ciphertext with the 16-byte tag appended.
@[direct_array_access]
pub fn (g &AesGcm) encrypt(plaintext []u8, nonce []u8, ad []u8) ![]u8 {
	// In V language, unlike C and Go, int is always a 32 bit integer, so technically safe
	// to assume if the arrays length not would exceed 32-bit integer (its maybe changed on the future).
	// For safety purposes, we check agains the limit.
	if u64(plaintext.len) > max_plaintext_size {
		return error('AES-GCM encrypt: plaintext size exceed the limit')
	}
	if u64(ad.len) > max_aad_size {
		return error('AES-GCM encrypt: additional data size exceed the limit')
	}
	if nonce.len != gcm_nonce_size {
		return error('AES-GCM nonce must be ${gcm_nonce_size} bytes')
	}
	j0 := j0_from_nonce(nonce)
	mut ctr := j0.clone()
	inc32(mut ctr)
	ciphertext := g.gctr(ctr, plaintext)

	mut ghash_in := pad_block(ad)
	ghash_in << pad_block(ciphertext)
	ghash_in << len_block(ad.len, ciphertext.len)
	s := g.ghash(zero_block, ghash_in)
	tag := g.gctr(j0, s)

	mut out := []u8{cap: ciphertext.len + gcm_tag_size}
	out << ciphertext
	out << tag
	return out
}

// decrypt verifies and decrypts `ciphertext` (which must include the trailing
// 16-byte tag) using `nonce` and additional authenticated data `ad`.
@[direct_array_access]
pub fn (g &AesGcm) decrypt(ciphertext []u8, nonce []u8, ad []u8) ![]u8 {
	// Check the array size for safety
	if u64(ciphertext.len) > max_ciphertext_size {
		return error('AES-GCM decrypt: ciphertext size exceed the limit')
	}
	if u64(ad.len) > max_aad_size {
		return error('AES-GCM decrypt: additional data size exceed the limit')
	}
	if nonce.len != gcm_nonce_size {
		return error('AES-GCM nonce must be ${gcm_nonce_size} bytes')
	}
	if ciphertext.len < gcm_tag_size {
		return error('AES-GCM ciphertext shorter than tag')
	}
	ct := ciphertext[..ciphertext.len - gcm_tag_size]
	tag := ciphertext[ciphertext.len - gcm_tag_size..]
	j0 := j0_from_nonce(nonce)

	mut ghash_in := pad_block(ad)
	ghash_in << pad_block(ct)
	ghash_in << len_block(ad.len, ct.len)
	s := g.ghash(zero_block, ghash_in)
	expected := g.gctr(j0, s)
	if !bytes_equal(expected, tag) {
		return error('AES-GCM authentication failed')
	}
	mut ctr := j0.clone()
	inc32(mut ctr)
	return g.gctr(ctr, ct)
}

// ghash computes GHASH_H over `data`, which the caller must already have padded
// to a multiple of the block size, starting from the running value `y`.
@[direct_array_access]
fn (g &AesGcm) ghash(y []u8, data []u8) []u8 {
	mut acc := y.clone()
	mut off := 0
	for off < data.len {
		for j in 0 .. block_size {
			acc[j] ^= data[off + j]
		}
		acc = gf_mult(acc, g.h)
		off += block_size
	}
	return acc
}

// gctr applies the GCM counter mode to `input` starting from counter block
// `icb`, returning the keystream-XORed output.
@[direct_array_access]
fn (g &AesGcm) gctr(icb []u8, input []u8) []u8 {
	if input.len == 0 {
		return []u8{}
	}
	mut out := []u8{len: input.len}
	mut ctr := icb.clone()
	mut ks := zero_block.clone()
	mut off := 0
	for off < input.len {
		g.block.encrypt(mut ks, ctr)
		n := if input.len - off < block_size { input.len - off } else { block_size }
		for j in 0 .. n {
			out[off + j] = input[off + j] ^ ks[j]
		}
		inc32(mut ctr)
		off += block_size
	}
	return out
}

// Helpers

// gf_mult multiplies two 16-byte blocks in GF(2^128) using the reduction
// polynomial from NIST SP 800-38D (the bit-reflected x^128 + x^7 + x^2 + x + 1).
@[direct_array_access]
fn gf_mult(x []u8, y []u8) []u8 {
	mut z := zero_block.clone()
	mut v := y.clone()

	// Process all 128 bits
	for i in 0 .. 128 {
		// Extract bit i from x (MSB first, left to right)
		// byte index: i >> 3 or (i / 8)
		// bit position: 7 - (i & 7) (7, 6, 5, ..., 0)
		bit := (x[i >> 3] >> (7 - u8(i & 7))) & 1

		// Create constant-time mask
		// If bit = 1: mask = 0xFF (all 1s)
		// If bit = 0: mask = 0x00 (all 0s)
		// This is constant-time: -(i8(bit)) produces correct result
		mask := u8(-(i8(bit)))

		// Conditional XOR: z ^= v * bit (constant-time)
		// Instead of: if bit == 1 { z[j] ^= v[j] }
		// We do:      z[j] ^= (v[j] & mask)
		for j in 0 .. block_size {
			z[j] ^= (v[j] & mask)
		}

		// Right shift v by 1 bit (always performed)
		// This is constant-time - no branches
		lsb := v[15] & 1 // Save LSB before shift

		for j := 15; j > 0; j-- {
			v[j] = (v[j] >> 1) | ((v[j - 1] & 1) << 7)
		}
		v[0] >>= 1

		// Polynomial reduction (constant-time)
		// If lsb = 1: v[0] ^= 0xe1
		// If lsb = 0: v[0] unchanged
		// Polynomial: x^128 + x^7 + x^2 + x + 1 = 0xe1 in lowest bits
		//
		// Instead of: if lsb == 1 { v[0] ^= 0xe1 }
		// We do:      v[0] ^= (0xe1 & mask_for_lsb)
		red_mask := u8(-(i8(lsb)))
		v[0] ^= (0xe1 & red_mask)
	}
	return z
}

// pad_block returns `data` right-padded with zeros to a multiple of 16 bytes.
@[direct_array_access]
fn pad_block(data []u8) []u8 {
	temp := data.clone()
	rem := data.len % block_size
	if rem == 0 {
		return temp
	}
	mut out := []u8{cap: data.len + block_size - rem}
	out << temp
	out << []u8{len: block_size - rem} // padding
	return out
}

// inc32 increments the last 32 bits of the 16-byte counter block in place.
@[direct_array_access]
fn inc32(mut ctr []u8) {
	mut c := (u32(ctr[12]) << 24) | (u32(ctr[13]) << 16) | (u32(ctr[14]) << 8) | u32(ctr[15])
	// detect for wrapping u32 counter
	if (c + 1) == 0 {
		panic('AES-GCM: inc32 overflow the counter')
	}
	c += 1
	ctr[12] = u8(c >> 24)
	ctr[13] = u8(c >> 16)
	ctr[14] = u8(c >> 8)
	ctr[15] = u8(c)
}

// len_block returns the GHASH length block: the bit lengths of the AAD and the
// ciphertext, each encoded as a big-endian 64-bit integer.
fn len_block(ad_len int, ct_len int) []u8 {
	a := u64(ad_len) * 8
	c := u64(ct_len) * 8
	mut b := zero_block.clone()
	for i in 0 .. 8 {
		b[7 - i] = u8(a >> (8 * u32(i)))
		b[15 - i] = u8(c >> (8 * u32(i)))
	}
	return b
}

// j0 derives the pre-counter block for a 96-bit nonce: nonce || 0^31 || 1.
@[direct_array_access]
fn j0_from_nonce(nonce []u8) []u8 {
	mut j := zero_block.clone()
	for i in 0 .. 12 {
		j[i] = nonce[i]
	}
	j[15] = 1
	return j
}

// bytes_equal compares two equal-length byte slices in constant time.
// similar to `crypto.internal.subtle.constant_time_compare()` ones
@[direct_array_access]
fn bytes_equal(a []u8, b []u8) bool {
	if a.len != b.len {
		return false
	}
	mut diff := u8(0)
	for i in 0 .. a.len {
		diff |= a[i] ^ b[i]
	}
	return diff == 0
}

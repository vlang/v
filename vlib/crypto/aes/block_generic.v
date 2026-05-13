// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// This implementation is derived from the golang implementation
// which itself is derived in part from the reference
// ANSI C implementation, which carries the following notice:
//
// rijndael-alg-fst.c
//
// @version 3.0 (December 2000)
//
// Optimised ANSI C code for the Rijndael cipher (now AES)
//
// @author Vincent Rijmen <vincent.rijmen@esat.kuleuven.ac.be>
// @author Antoon Bosselaers <antoon.bosselaers@esat.kuleuven.ac.be>
// @author Paulo Barreto <paulo.barreto@Terra.com.br>
//
// This code is hereby placed in the public domain.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
// BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// See FIPS 197 for specification, and see Daemen and Rijmen's Rijndael submission
// for implementation details.
// https://csrc.nist.gov/csrc/media/publications/fips/197/final/documents/fips-197.pdf
// https://csrc.nist.gov/archive/aes/rijndael/Rijndael-ammended.pdf
module aes

import encoding.binary

// ct_mask_u8 expands the low bit of `bit` to either 0x00 or 0xff.
@[inline]
fn ct_mask_u8(bit u8) u8 {
	return u8(~(int(bit & 1) - 1))
}

// xtime multiplies `x` by x in GF(2^8).
@[inline]
fn xtime(x u8) u8 {
	return u8(u32(x) << 1) ^ (u8(0x1b) & ct_mask_u8(x >> 7))
}

// gf_mul multiplies `x` and `y` in GF(2^8) without data-dependent branches.
@[direct_array_access; inline]
fn gf_mul(x u8, y u8) u8 {
	mut a := x
	mut b := y
	mut out := u8(0)
	for _ in 0 .. 8 {
		out ^= a & ct_mask_u8(b)
		a = xtime(a)
		b >>= 1
	}
	return out
}

// gf_square squares `x` in GF(2^8).
@[inline]
fn gf_square(x u8) u8 {
	return gf_mul(x, x)
}

@[inline]
fn rotl8(x u8, n int) u8 {
	return u8((u32(x) << u32(n)) | (u32(x) >> u32(8 - n)))
}

@[inline]
fn gf_inverse(x u8) u8 {
	x2 := gf_square(x)
	x4 := gf_square(x2)
	x8 := gf_square(x4)
	x16 := gf_square(x8)
	x32 := gf_square(x16)
	x64 := gf_square(x32)
	x128 := gf_square(x64)
	return gf_mul(gf_mul(gf_mul(gf_mul(gf_mul(gf_mul(x128, x64), x32), x16), x8), x4), x2)
}

// sub_byte applies the AES S-box without lookup tables.
@[inline]
fn sub_byte(x u8) u8 {
	inv := gf_inverse(x)
	return inv ^ rotl8(inv, 1) ^ rotl8(inv, 2) ^ rotl8(inv, 3) ^ rotl8(inv, 4) ^ u8(0x63)
}

// inv_sub_byte applies the inverse AES S-box without lookup tables.
@[inline]
fn inv_sub_byte(x u8) u8 {
	return gf_inverse(rotl8(x, 1) ^ rotl8(x, 3) ^ rotl8(x, 6) ^ u8(0x05))
}

@[direct_array_access; inline]
fn add_round_key(mut state [16]u8, xk []u32, round int) {
	for col in 0 .. 4 {
		word := xk[round * 4 + col]
		base := col * 4
		state[base + 0] ^= u8(word >> 24)
		state[base + 1] ^= u8(word >> 16)
		state[base + 2] ^= u8(word >> 8)
		state[base + 3] ^= u8(word)
	}
}

@[direct_array_access; inline]
fn sub_bytes(mut state [16]u8) {
	for i in 0 .. 16 {
		state[i] = sub_byte(state[i])
	}
}

@[direct_array_access; inline]
fn inv_sub_bytes(mut state [16]u8) {
	for i in 0 .. 16 {
		state[i] = inv_sub_byte(state[i])
	}
}

@[direct_array_access; inline]
fn shift_rows(mut state [16]u8) {
	t1 := state[1]
	state[1] = state[5]
	state[5] = state[9]
	state[9] = state[13]
	state[13] = t1

	t2 := state[2]
	t6 := state[6]
	state[2] = state[10]
	state[6] = state[14]
	state[10] = t2
	state[14] = t6

	t3 := state[3]
	state[3] = state[15]
	state[15] = state[11]
	state[11] = state[7]
	state[7] = t3
}

@[direct_array_access; inline]
fn inv_shift_rows(mut state [16]u8) {
	t13 := state[13]
	state[13] = state[9]
	state[9] = state[5]
	state[5] = state[1]
	state[1] = t13

	t2 := state[2]
	t6 := state[6]
	state[2] = state[10]
	state[6] = state[14]
	state[10] = t2
	state[14] = t6

	t3 := state[3]
	state[3] = state[7]
	state[7] = state[11]
	state[11] = state[15]
	state[15] = t3
}

@[direct_array_access; inline]
fn mix_columns(mut state [16]u8) {
	for col in 0 .. 4 {
		base := col * 4
		s0 := state[base + 0]
		s1 := state[base + 1]
		s2 := state[base + 2]
		s3 := state[base + 3]
		m2s0 := xtime(s0)
		m2s1 := xtime(s1)
		m2s2 := xtime(s2)
		m2s3 := xtime(s3)
		state[base + 0] = m2s0 ^ (m2s1 ^ s1) ^ s2 ^ s3
		state[base + 1] = s0 ^ m2s1 ^ (m2s2 ^ s2) ^ s3
		state[base + 2] = s0 ^ s1 ^ m2s2 ^ (m2s3 ^ s3)
		state[base + 3] = (m2s0 ^ s0) ^ s1 ^ s2 ^ m2s3
	}
}

@[direct_array_access; inline]
fn inv_mix_columns(mut state [16]u8) {
	for col in 0 .. 4 {
		base := col * 4
		s0 := state[base + 0]
		s1 := state[base + 1]
		s2 := state[base + 2]
		s3 := state[base + 3]
		state[base + 0] = gf_mul(s0, 14) ^ gf_mul(s1, 11) ^ gf_mul(s2, 13) ^ gf_mul(s3, 9)
		state[base + 1] = gf_mul(s0, 9) ^ gf_mul(s1, 14) ^ gf_mul(s2, 11) ^ gf_mul(s3, 13)
		state[base + 2] = gf_mul(s0, 13) ^ gf_mul(s1, 9) ^ gf_mul(s2, 14) ^ gf_mul(s3, 11)
		state[base + 3] = gf_mul(s0, 11) ^ gf_mul(s1, 13) ^ gf_mul(s2, 9) ^ gf_mul(s3, 14)
	}
}

// Encrypt one block from src into dst, using the expanded key xk.
@[direct_array_access]
fn encrypt_block_generic(xk []u32, mut dst []u8, src []u8) {
	_ = src[15] // early bounds check
	mut state := [16]u8{}
	for i in 0 .. 16 {
		state[i] = src[i]
	}
	nr := xk.len / 4 - 1
	add_round_key(mut state, xk, 0)
	for round in 1 .. nr {
		sub_bytes(mut state)
		shift_rows(mut state)
		mix_columns(mut state)
		add_round_key(mut state, xk, round)
	}
	sub_bytes(mut state)
	shift_rows(mut state)
	add_round_key(mut state, xk, nr)
	_ = dst[15] // early bounds check
	for i in 0 .. 16 {
		dst[i] = state[i]
	}
}

// Decrypt one block from src into dst, using the expanded key xk.
@[direct_array_access]
fn decrypt_block_generic(xk []u32, mut dst []u8, src []u8) {
	_ = src[15] // early bounds check
	mut state := [16]u8{}
	for i in 0 .. 16 {
		state[i] = src[i]
	}
	nr := xk.len / 4 - 1
	add_round_key(mut state, xk, 0)
	for round in 1 .. nr {
		inv_shift_rows(mut state)
		inv_sub_bytes(mut state)
		add_round_key(mut state, xk, round)
		inv_mix_columns(mut state)
	}
	inv_shift_rows(mut state)
	inv_sub_bytes(mut state)
	add_round_key(mut state, xk, nr)
	_ = dst[15] // early bounds check
	for i in 0 .. 16 {
		dst[i] = state[i]
	}
}

// Apply the AES S-box to each byte in w without lookup tables.
@[inline]
fn subw(w u32) u32 {
	return u32(sub_byte(u8(w >> 24))) << 24 | u32(sub_byte(u8(w >> 16))) << 16 | u32(sub_byte(u8(w >> 8))) << 8 | u32(sub_byte(u8(w)))
}

// Rotate
@[inline]
fn rotw(w u32) u32 {
	return (w << 8) | (w >> 24)
}

// Key expansion algorithm. See FIPS-197, Figure 11.
// Their rcon[i] is our powx[i-1] << 24.
@[direct_array_access]
fn expand_key_generic(key []u8, mut enc []u32, mut dec []u32) {
	// Encryption key setup.
	mut i := 0
	nk := key.len / 4
	for i = 0; i < nk; i++ {
		if 4 * i >= key.len {
			break
		}
		enc[i] = binary.big_endian_u32(key[4 * i..])
	}
	for i < enc.len {
		mut t := enc[i - 1]
		if i % nk == 0 {
			t = subw(rotw(t)) ^ u32(pow_x[i / nk - 1]) << 24
		} else if nk > 6 && i % nk == 4 {
			t = subw(t)
		}
		enc[i] = enc[i - nk] ^ t
		i++
	}
	// Derive decryption key from encryption key.
	// Reverse the 4-word round key sets from enc to produce dec.
	// The byte-wise block path applies InvMixColumns separately during decryption.
	if dec.len == 0 {
		return
	}
	n := enc.len
	for i = 0; i < n; i += 4 {
		ei := n - i - 4
		for j in 0 .. 4 {
			dec[i + j] = enc[ei + j]
		}
	}
}

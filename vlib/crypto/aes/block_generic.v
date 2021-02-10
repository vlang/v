// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
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

// Encrypt one block from src into dst, using the expanded key xk.
fn encrypt_block_generic(xk []u32, mut dst []byte, src []byte) {
	_ = src[15] // early bounds check
	mut s0 := binary.big_endian_u32(src[..4])
	mut s1 := binary.big_endian_u32(src[4..8])
	mut s2 := binary.big_endian_u32(src[8..12])
	mut s3 := binary.big_endian_u32(src[12..16])
	// First round just XORs input with key.
	s0 ^= xk[0]
	s1 ^= xk[1]
	s2 ^= xk[2]
	s3 ^= xk[3]
	// Middle rounds shuffle using tables.
	// Number of rounds is set by length of expanded key.
	nr := xk.len / 4 - 2 // - 2: one above, one more below
	mut k := 4
	mut t0 := u32(0)
	mut t1 := u32(0)
	mut t2 := u32(0)
	mut t3 := u32(0)
	for _ in 0 .. nr {
		t0 = xk[k + 0] ^ te0[byte(s0 >> 24)] ^ te1[byte(s1 >> 16)] ^ te2[byte(s2 >> 8)] ^ u32(te3[byte(s3)])
		t1 = xk[k + 1] ^ te0[byte(s1 >> 24)] ^ te1[byte(s2 >> 16)] ^ te2[byte(s3 >> 8)] ^ u32(te3[byte(s0)])
		t2 = xk[k + 2] ^ te0[byte(s2 >> 24)] ^ te1[byte(s3 >> 16)] ^ te2[byte(s0 >> 8)] ^ u32(te3[byte(s1)])
		t3 = xk[k + 3] ^ te0[byte(s3 >> 24)] ^ te1[byte(s0 >> 16)] ^ te2[byte(s1 >> 8)] ^ u32(te3[byte(s2)])
		k += 4
		s0 = t0
		s1 = t1
		s2 = t2
		s3 = t3
	}
	// Last round uses s-box directly and XORs to produce output.
	s0 = s_box0[t0 >> 24] << 24 |
		s_box0[t1 >> 16 & 0xff] << 16 | u32(s_box0[t2 >> 8 & 0xff] << 8) | s_box0[t3 & u32(0xff)]
	s1 = s_box0[t1 >> 24] << 24 |
		s_box0[t2 >> 16 & 0xff] << 16 | u32(s_box0[t3 >> 8 & 0xff] << 8) | s_box0[t0 & u32(0xff)]
	s2 = s_box0[t2 >> 24] << 24 |
		s_box0[t3 >> 16 & 0xff] << 16 | u32(s_box0[t0 >> 8 & 0xff] << 8) | s_box0[t1 & u32(0xff)]
	s3 = s_box0[t3 >> 24] << 24 |
		s_box0[t0 >> 16 & 0xff] << 16 | u32(s_box0[t1 >> 8 & 0xff] << 8) | s_box0[t2 & u32(0xff)]
	s0 ^= xk[k + 0]
	s1 ^= xk[k + 1]
	s2 ^= xk[k + 2]
	s3 ^= xk[k + 3]
	_ := dst[15] // early bounds check
	binary.big_endian_put_u32(mut (*dst)[0..4], s0)
	binary.big_endian_put_u32(mut (*dst)[4..8], s1)
	binary.big_endian_put_u32(mut (*dst)[8..12], s2)
	binary.big_endian_put_u32(mut (*dst)[12..16], s3)
}

// Decrypt one block from src into dst, using the expanded key xk.
fn decrypt_block_generic(xk []u32, mut dst []byte, src []byte) {
	_ = src[15] // early bounds check
	mut s0 := binary.big_endian_u32(src[0..4])
	mut s1 := binary.big_endian_u32(src[4..8])
	mut s2 := binary.big_endian_u32(src[8..12])
	mut s3 := binary.big_endian_u32(src[12..16])
	// First round just XORs input with key.
	s0 ^= xk[0]
	s1 ^= xk[1]
	s2 ^= xk[2]
	s3 ^= xk[3]
	// Middle rounds shuffle using tables.
	// Number of rounds is set by length of expanded key.
	nr := xk.len / 4 - 2 // - 2: one above, one more below
	mut k := 4
	mut t0 := u32(0)
	mut t1 := u32(0)
	mut t2 := u32(0)
	mut t3 := u32(0)
	for _ in 0 .. nr {
		t0 = xk[k + 0] ^ td0[byte(s0 >> 24)] ^ td1[byte(s3 >> 16)] ^ td2[byte(s2 >> 8)] ^ u32(td3[byte(s1)])
		t1 = xk[k + 1] ^ td0[byte(s1 >> 24)] ^ td1[byte(s0 >> 16)] ^ td2[byte(s3 >> 8)] ^ u32(td3[byte(s2)])
		t2 = xk[k + 2] ^ td0[byte(s2 >> 24)] ^ td1[byte(s1 >> 16)] ^ td2[byte(s0 >> 8)] ^ u32(td3[byte(s3)])
		t3 = xk[k + 3] ^ td0[byte(s3 >> 24)] ^ td1[byte(s2 >> 16)] ^ td2[byte(s1 >> 8)] ^ u32(td3[byte(s0)])
		k += 4
		s0 = t0
		s1 = t1
		s2 = t2
		s3 = t3
	}
	// Last round uses s-box directly and XORs to produce output.
	s0 = u32(s_box1[t0 >> 24]) << 24 |
		u32(s_box1[t3 >> 16 & 0xff]) << 16 | u32(s_box1[t2 >> 8 & 0xff] << 8) | u32(s_box1[t1 & u32(0xff)])
	s1 = u32(s_box1[t1 >> 24]) << 24 |
		u32(s_box1[t0 >> 16 & 0xff]) << 16 | u32(s_box1[t3 >> 8 & 0xff] << 8) | u32(s_box1[t2 & u32(0xff)])
	s2 = u32(s_box1[t2 >> 24]) << 24 |
		u32(s_box1[t1 >> 16 & 0xff]) << 16 | u32(s_box1[t0 >> 8 & 0xff] << 8) | u32(s_box1[t3 & u32(0xff)])
	s3 = u32(s_box1[t3 >> 24]) << 24 |
		u32(s_box1[t2 >> 16 & 0xff]) << 16 | u32(s_box1[t1 >> 8 & 0xff] << 8) | u32(s_box1[t0 & u32(0xff)])
	s0 ^= xk[k + 0]
	s1 ^= xk[k + 1]
	s2 ^= xk[k + 2]
	s3 ^= xk[k + 3]
	_ = dst[15] // early bounds check
	binary.big_endian_put_u32(mut (*dst)[..4], s0)
	binary.big_endian_put_u32(mut (*dst)[4..8], s1)
	binary.big_endian_put_u32(mut (*dst)[8..12], s2)
	binary.big_endian_put_u32(mut (*dst)[12..16], s3)
}

// Apply s_box0 to each byte in w.
fn subw(w u32) u32 {
	return u32(s_box0[w >> 24]) << 24 | u32(s_box0[w >> 16 & 0xff] << 16) | u32(s_box0[w >> 8 &
		0xff] << 8) | u32(s_box0[w & u32(0xff)])
}

// Rotate
fn rotw(w u32) u32 {
	return (w << 8) | (w >> 24)
}

// Key expansion algorithm. See FIPS-197, Figure 11.
// Their rcon[i] is our powx[i-1] << 24.
fn expand_key_generic(key []byte, mut enc []u32, mut dec []u32) {
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
	// All sets but the first and last get the MixColumn transform applied.
	if dec.len == 0 {
		return
	}
	n := enc.len
	for i = 0; i < n; i += 4 {
		ei := n - i - 4
		for j in 0 .. 4 {
			mut x := enc[ei + j]
			if i > 0 && i + 4 < n {
				x = td0[s_box0[x >> 24]] ^ td1[s_box0[x >> 16 & 0xff]] ^ td2[s_box0[x >> 8 & 0xff]] ^
					td3[s_box0[x & u32(0xff)]]
			}
			dec[i + j] = x
		}
	}
}

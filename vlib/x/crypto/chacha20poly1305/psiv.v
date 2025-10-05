// Copyright (c) 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file contains an experimental port of a Rust reference implementation of nonce-misuse
// resistant and key-committing authenticated encryption scheme called ChaCha20-Poly1305-PSIV.
// Its originally described by Michiel Verbauwhede and the teams on his papers.
// See the detail on the [A Robust Variant of ChaCha20-Poly1305](https://eprint.iacr.org/2025/222).
module chacha20poly1305

import arrays
import math.bits
import encoding.binary
import crypto.internal.subtle
import x.crypto.poly1305

// new_psiv creates a new Chacha20Poly1305 with psiv construct to operate on.
@[direct_array_access]
pub fn new_psiv(key []u8) !&AEAD {
	if key.len != key_size {
		return error('new_psiv: bad key size')
	}
	// derives and initializes the new key for later purposes
	mac_key, enc_key, po := psiv_init(key)!
	// set the values
	c := &Chacha20Poly1305RE{
		key:     key
		precomp: true
		mac_key: mac_key
		enc_key: enc_key
		po:      po
	}
	return c
}

// Chacha20Poly1305RE was a Chacha20Poly1305 opaque with nonce-misuse resistent
// and key-commiting AEAD scheme with PSIV construct.
@[noinit]
struct Chacha20Poly1305RE {
mut:
	// An underlying 32-bytes of key
	key []u8
	// flags that tells derivation keys has been precomputed
	precomp bool
	mac_key []u8
	enc_key []u8
	po      &poly1305.Poly1305 = unsafe { nil }
}

// free releases resources taken by c. Dont use c after `.free` call.
@[unsafe]
pub fn (mut c Chacha20Poly1305RE) free() {
	unsafe {
		c.key.free()
		c.mac_key.free()
		c.enc_key.free()
		c.po = nil
	}
	c.precomp = false
}

// nonce_size return the size of the nonce of underlying c.
// Currently, it only support for standard 12-bytes nonce.
pub fn (c &Chacha20Poly1305RE) nonce_size() int {
	return nonce_size
}

// overhead returns difference between the lengths of a plaintext and its ciphertext.
// Its normally returns a tag size produced by this scheme.
fn (c &Chacha20Poly1305RE) overhead() int {
	return tag_size
}

// encrypt encrypts and authenticates the provided plaintext along with a nonce, and
// to be authenticated additional data in `ad`. It returns a ciphertext with message authenticated
// code stored within the end of ciphertext.
@[direct_array_access]
pub fn (c Chacha20Poly1305RE) encrypt(plaintext []u8, nonce []u8, ad []u8) ![]u8 {
	if nonce.len != nonce_size {
		return error('Chacha20Poly1305RE.encrypt: bad nonce length, only support 12-bytes nonce')
	}

	// clone the initial poly1305
	mut po_ad := c.po.clone()
	po_ad.update(ad)

	mut po_ad_clone := po_ad.clone()
	// build the tag
	tag := psiv_gen_tag(mut po_ad_clone, plaintext, ad.len, c.mac_key, nonce)
	enc := psiv_encrypt_internal(plaintext, c.enc_key, tag, nonce)!

	// setup destination buffer
	mut out := []u8{cap: plaintext.len + tag_size}
	out << enc
	out << tag

	return out
}

// decrypt decrypts the ciphertext with provided key, nonce and additional data in ad.
// It also tries to validate message authenticated code within ciphertext compared with
// calculated tag. It returns successfully decrypted message or error on fails.
@[direct_array_access]
pub fn (c Chacha20Poly1305RE) decrypt(ciphertext []u8, nonce []u8, ad []u8) ![]u8 {
	if ciphertext.len < tag_size {
		return error('Chacha20Poly1305RE.decrypt: insufficient ciphertext length')
	}
	if nonce.len != nonce_size {
		return error('Chacha20Poly1305RE.decrypt: invalid nonce length provided')
	}
	enc := ciphertext[0..ciphertext.len - c.overhead()]
	tag := ciphertext[ciphertext.len - c.overhead()..]

	mut po_with_ad := c.po.clone()
	po_with_ad.update(ad)

	out := psiv_encrypt_internal(enc, c.enc_key, tag, nonce)!
	mut poad_clone := po_with_ad.clone()
	mac := psiv_gen_tag(mut poad_clone, out, ad.len, c.mac_key, nonce)
	if subtle.constant_time_compare(mac, tag) != 1 {
		unsafe {
			out.free()
			mac.free()
		}
		return error('unmatching tag')
	}
	return out
}

// psiv_encrypt encrypts plaintext with provided key, nonce and additional data ad.
// It returns a ciphertext that contains message authentication code (mac) stored
// within the end of ciphertext
@[direct_array_access]
pub fn psiv_encrypt(plaintext []u8, key []u8, nonce []u8, ad []u8) ![]u8 {
	c := new_psiv(key)!
	out := c.encrypt(plaintext, nonce, ad)!
	unsafe { c.free() }
	return out
}

// psiv_decrypt decrypts the ciphertext with provided key, nonce and additional data in ad.
// It also tries to validate message authenticated code within ciphertext compared with
// calculated tag. It returns successfully decrypted message or error on fails.
@[direct_array_access]
pub fn psiv_decrypt(ciphertext []u8, key []u8, nonce []u8, ad []u8) ![]u8 {
	c := new_psiv(key)!
	out := c.decrypt(ciphertext, nonce, ad)!
	unsafe { c.free() }
	return out
}

// PSIV Helpers
//

// returns 16-bytes of tag
@[direct_array_access]
fn psiv_gen_tag(mut po poly1305.Poly1305, input []u8, ad_len int, mac_key []u8, nonce []u8) []u8 {
	assert mac_key.len == 36
	assert nonce.len == 12

	po.update(input)
	po.update(length_to_block(ad_len, input.len))

	mut digest := []u8{len: tag_size}
	po.finish(mut digest)

	out := merge_drv_key(mac_key, nonce, digest[0..8], digest[8..16])
	tag := out[0..16].clone()
	unsafe { out.reset() }
	return tag
}

@[direct_array_access]
fn psiv_encrypt_internal(plaintext []u8, key []u8, tag []u8, nonce []u8) ![]u8 {
	assert key.len == 36
	assert tag.len == 16
	assert nonce.len == 12
	mut dst := []u8{cap: plaintext.len}
	ta, tb := split_tag(tag)
	mut ctr := binary.little_endian_u64(ta)

	chunks := arrays.chunk[u8](plaintext, 64)
	mut tc := []u8{len: 8}
	mut o32 := [16]u32{}
	for chunk in chunks {
		binary.big_endian_put_u64(mut tc, ctr)
		convert_64u8_into_16u32(mut o32, merge_drv_key(key, nonce, tc, tb))
		buf32 := chacha20_core(o32)
		mut b64 := []u8{len: 64}
		serialize(mut b64, buf32)
		for i, v in chunk {
			o := v ^ b64[i]
			dst << o
		}
		ctr += 1
		if ctr == 0 {
			return error('counter overflowing')
		}
	}
	return dst
}

@[direct_array_access; inline]
fn split_tag(tag []u8) ([]u8, []u8) {
	assert tag.len == 16
	return tag[0..8].clone(), tag[8..16].clone()
}

@[direct_array_access; inline]
fn psiv_init(key []u8) !([]u8, []u8, &poly1305.Poly1305) {
	assert key.len == 32
	// derives some keys
	pol_key := fk_k(key)
	mac_key := fm_k(key)
	enc_key := fe_k(key)

	mut x := [16]u32{}
	convert_64u8_into_16u32(mut x, merge_drvk_zeros(pol_key))
	buf := chacha20_core(x)

	mut kc := []u8{len: 64}
	serialize(mut kc, buf)
	poly1305_key := kc[0..32].clone()
	po := poly1305.new(poly1305_key)!

	return mac_key, enc_key, po
}

@[direct_array_access; inline]
fn convert_64u8_into_16u32(mut x [16]u32, s []u8) {
	assert s.len == 64
	for i := 0; i < 16; i++ {
		x[i] = binary.little_endian_u32(s[i * 4..(i + 1) * 4])
	}
}

@[direct_array_access; inline]
fn clone_s(s [16]u32) [16]u32 {
	mut x := [16]u32{}
	for i, v in s {
		x[i] = v
	}
	return x
}

@[direct_array_access; inline]
fn serialize(mut out []u8, s [16]u32) {
	assert out.len == 64
	for i, v in s {
		binary.little_endian_put_u32(mut out[i * 4..(i + 1) * 4], v)
	}
}

@[direct_array_access; inline]
fn chacha20_core(s [16]u32) [16]u32 {
	mut ws := clone_s(s)
	for i := 0; i < 10; i++ {
		// Column-round
		//  0 |  1 |  2 |  3
		//  4 |  5 |  6 |  7
		//  8 |  9 | 10 | 11
		// 12 | 13 | 14 | 15
		qround_on_state(mut ws, 0, 4, 8, 12) // 0
		qround_on_state(mut ws, 1, 5, 9, 13) // 1
		qround_on_state(mut ws, 2, 6, 10, 14) // 2
		qround_on_state(mut ws, 3, 7, 11, 15) // 3

		// Diagonal round.
		//   0 \  1 \  2 \  3
		//   5 \  6 \  7 \  4
		//  10 \ 11 \  8 \  9
		//  15 \ 12 \ 13 \ 14
		qround_on_state(mut ws, 0, 5, 10, 15)
		qround_on_state(mut ws, 1, 6, 11, 12)
		qround_on_state(mut ws, 2, 7, 8, 13)
		qround_on_state(mut ws, 3, 4, 9, 14)
	}
	for i := 0; i < 16; i++ {
		ws[i] += s[i]
	}
	return ws
}

@[direct_array_access]
fn qround_on_state(mut s [16]u32, a int, b int, c int, d int) {
	// a += b;  d ^= a;  d <<<= 16;
	s[a] += s[b]
	s[d] ^= s[a]
	s[d] = bits.rotate_left_32(s[d], 16)

	// c += d;  b ^= c;  b <<<= 12;
	s[c] += s[d]
	s[b] ^= s[c]
	s[b] = bits.rotate_left_32(s[b], 12)

	// a += b;  d ^= a;  d <<<=  8;
	s[a] += s[b]
	s[d] ^= s[a]
	s[d] = bits.rotate_left_32(s[d], 8)

	// c += d;  b ^= c;  b <<<=  7;
	s[c] += s[d]
	s[b] ^= s[c]
	s[b] = bits.rotate_left_32(s[b], 7)
}

// merge_drv_key merges provided bytes into 64-bytes key
@[direct_array_access; inline]
fn merge_drv_key(dkey []u8, nonce []u8, tag_ctr []u8, tag_rest []u8) []u8 {
	assert dkey.len == 36
	assert nonce.len == 12
	assert tag_ctr.len == 8
	assert tag_rest.len == 8

	mut x := []u8{len: 64}

	// 0..36
	for i := 0; i < dkey.len; i++ {
		x[i] = dkey[i]
	}
	// 36..48
	for i := 0; i < nonce.len; i++ {
		x[36 + i] = nonce[i]
	}
	// 48..56
	for i := 0; i < tag_ctr.len; i++ {
		x[i + 48] = tag_ctr[i]
	}
	// 56..64
	for i := 0; i < tag_rest.len; i++ {
		x[i + 56] = tag_rest[i]
	}

	return x
}

// merge_drvk_zeros merges derived key in dkey with zeros nonce and zeros tag into 64-bytes of key.
@[direct_array_access; inline]
fn merge_drvk_zeros(dkey []u8) []u8 {
	assert dkey.len == 36
	mut x := []u8{len: 64}
	_ := copy(mut x, dkey)
	// the others was null bytes
	return x
}

// fk_k maps and transforms 32-bytes of key into 36-bytes of new key used to
// derive a poly1305 construction.
// See the papers doc on the 3.3 Additional Details part, on page 12-13
@[direct_array_access; inline]
fn fk_k(k []u8) []u8 {
	assert k.len == 32
	// fk(K) = 	K1 ∥ K2 ∥ K3 ∥ 03 ∥ K5 ∥ K6 ∥ K7 ∥ 0c ∥ K9 ∥ K10 ∥ K11 ∥ 30
	//			∥ K4 ∥ K8 ∥ K12 ∥ c0 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32
	// with 0-based index
	// 			K0 ∥ K1 ∥ K2 ∥ 03 ∥ K4 ∥ K5 ∥ K6 ∥ 0c ∥ K8 ∥ K9 ∥ K10 ∥ 30
	//			∥ K3 ∥ K7 ∥ K11 ∥ c0 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := []u8{len: 36}
	// 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = k[i]
	}
	x[3] = u8(0x03)

	// 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = k[i]
	}
	x[7] = 0x0c

	// 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = k[i]
	}
	x[11] = 0x30

	// 12 .. 16
	x[12] = k[3]
	x[13] = k[7]
	x[14] = k[11]
	x[15] = 0xc0

	// 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = x[i - 4]
	}

	return x
}

// fm_k maps and transforms 32-bytes of key into 36-bytes of message authentication key.
// It later used for psiv tag generation.
@[direct_array_access; inline]
fn fm_k(k []u8) []u8 {
	assert k.len == 32
	// fm(K) = 	K1 ∥ K2 ∥ K3 ∥ 05 ∥ K5 ∥ K6 ∥ K7 ∥ 0a ∥ K9 ∥ K10 ∥ K11 ∥ 50 ∥
	//			K4 ∥ K8 ∥ K12 ∥ a0 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32 ,
	// Or, with 0-based index
	// fm(K) = 	K0 ∥ K1 ∥ K2 ∥ 05 ∥ K4 ∥ K5 ∥ K6 ∥ 0a ∥ K8 ∥ K9 ∥ K10 ∥ 50 ∥
	//			K3 ∥ K7 ∥ K11 ∥ a0 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := []u8{len: 36}
	// 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = k[i]
	}
	x[3] = u8(0x05)

	// 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = k[i]
	}
	x[7] = 0x0a

	// 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = k[i]
	}
	x[11] = 0x50

	// 12 .. 16
	x[12] = k[3]
	x[13] = k[7]
	x[14] = k[11]
	x[15] = 0xa0

	// 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = x[i - 4]
	}

	return x
}

// fe_k maps and transforms 32-bytes of key into 36-bytes of new encryption key
@[direct_array_access; inline]
fn fe_k(k []u8) []u8 {
	assert k.len == 32
	// fe(K) = 	K1 ∥ K2 ∥ K3 ∥ 06 ∥ K5 ∥ K6 ∥ K7 ∥ 09 ∥ K9 ∥ K10 ∥ K11 ∥ 60 ∥
	// 			K4 ∥ K8 ∥ K12 ∥ 90 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32
	// Or, with 0-based index
	// fe(K) = 	K0 ∥ K1 ∥ K2 ∥ 06 ∥ K4 ∥ K5 ∥ K6 ∥ 09 ∥ K8 ∥ K9 ∥ K10 ∥ 60 ∥
	// 			K3 ∥ K7 ∥ K11 ∥ 90 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := []u8{len: 36}
	// 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = k[i]
	}
	x[3] = u8(0x06)

	// 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = k[i]
	}
	x[7] = 0x09

	// 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = k[i]
	}
	x[11] = 0x60

	// 12 .. 16
	x[12] = k[3]
	x[13] = k[7]
	x[14] = k[11]
	x[15] = 0x90

	// 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = x[i - 4]
	}

	return x
}

// length_to_block transforms two's length in len1 and len2 into 16-bytes block
@[inline]
fn length_to_block(len1 int, len2 int) []u8 {
	mut block := []u8{len: 16}
	binary.little_endian_put_u64(mut block[0..8], u64(len1))
	binary.little_endian_put_u64(mut block[8..16], u64(len2))

	return block
}

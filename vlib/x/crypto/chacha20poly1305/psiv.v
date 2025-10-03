// Copyright (c) 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file contains a port of a Rust reference implementation of nonce-misuse resistant
// and key-committing authenticated encryption scheme called ChaCha20-Poly1305-PSIV
// Its originally described by Michiel Verbauwhede and the teams on his papers.
// See [A Robust Variant of ChaCha20-Poly1305](https://eprint.iacr.org/2025/222)
module chacha20poly1305

import arrays
import math.bits
import encoding.binary
import x.crypto.poly1305

enum ReMode {
	psiv
	daence
}

@[noinit]
struct Chacha20Poly1305RE {
mut:
	mode ReMode
	key  [32]u8
}

@[direct_array_access]
fn (c Chacha20Poly1305RE) encrypt(plaintext []u8, nonce [12]u8, ad []u8) ![]u8 {
	if nonce.len != nonce_size {
		return error('Chacha20Poly1305RE.encrypt: unsupported nonce size, only standard 12 size')
	}
	mac_key, enc_key, po := psiv_init(c.key)!
	// clone the initial poly1305
	mut po_ad := po
	po_ad.update(ad)

	po_ad_clone := po_ad
	mut out := []u8{cap: plaintext.len + tag_size}
	tag := psiv_gen_tag(mut po_ad_clone, plaintext, ad.len, mac_key)!
	enc := psiv_stream_encrypt_internal(plaintext, enc_key, tag, nonce)!

	out << enc
	out << tag
	return out
}

// returns 16-bytes of tag
@[direct_array_access]
fn psiv_gen_tag(mut po poly1305.Poly1305, input []u8, ad_len int, mac_key [36]u8, nonce [12]u8) ![]u8 {
	po.update(input)
	po.update(length_block(usize(ad_len), usize(input.len)))

	mut digest := []u8{len: tag_size}
	po.finish(mut digest)
	mut tag_ctr := [8]u8{}
	mut tag_rest := [8]u8{}
	out := key_merge(mac_key, nonce, dyn_to_fixed8(mut tag_ctr, digest[0..8]), dyn_to_fixed8(mut tag_rest,
		digest[8..16]))!
	tag := out[0..16].clone()
	unsafe { out.reset() }
	return tag
}

@[direct_array_access]
fn psiv_stream_encrypt_internal(plaintext []u8, key [36]u8, tag [16]u8, nonce [12]u8) ![]u8 {
	mut dst := []u8{cap: plaintext.len}
	ta, tb := split_tag(tag)
	mut ctr := binary.little_endian_u64(ta)

	chunks := arrays.chunk[u8](plaintext, 64)
	for chunk in chunks {
		mut tc := [8]u8{}
		binary.big_endian_put_u64_fixed(mut tc, ctr)
		obj32 := [16]u32{}
		convert_64u8_into_16u32(mut obj32, key_merge(key, nonce, tc, tb))
		buf := chacha20_core(obj32)

		for i, v in chunk {
			o := v ^ buf[i]
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
fn split_tag(tag [16]u8) ([8]u8, [8]u8) {
	mut a := [8]u8{}
	for i := 0; i < 8; i++ {
		a[i] = tag[i]
	}
	mut b := [8]u8{}
	for i := 8; i < 16; i++ {
		b[i] = tag[i]
	}
	return a, b
}

@[direct_array_access; inline]
fn dyn_to_fixed8(mut o [8]u8, src []u8) {
	for i := 0; i < o.len; i++ {
		o[i] = src[i]
	}
}

@[direct_array_access]
fn psiv_init(key [32]u8) !([36]u8, [36]u8, &poly1305.Poly1305) {
	pol_key := fk_k(key)
	mac_key := fm_k(key)
	enc_key := fe_k(key)

	mut x := [16]u32{}
	convert_64u8_into_16u32(mut x, poly1305_key_merge(pol_key))
	buf := chacha20_core(x)

	mut kc := [64]u8{}
	convert_16u32_into_64u8(mut kc, buf)
	poly1305_key := kc[0..32].clone()
	po := poly1305.new(poly1305_key)!

	return mac_key, enc_key, po
}

@[direct_array_access; inline]
fn convert_64u8_into_16u32(mut x [16]u32, s [64]u8) {
	for i := 0; i < 16; i++ {
		x[i] = binary.little_endian_u32(s[i * 4..(i + 1) * 4])
	}
}

@[direct_array_access; inline]
fn convert_16u32_into_64u8(mut b [64]u8, s [16]u32) {
	for i, v in s {
		b[i * 4 + 0] = u8(v)
		b[i * 4 + 1] = u8(v >> u32(8))
		b[i * 4 + 2] = u8(v >> u32(16))
		b[i * 4 + 3] = u8(v >> u32(24))
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

@[direct_array_access]
fn key_merge(key [36]u8, nonce [12]u8, tag_ctr [8]u8, tag_rest [8]u8) [64]u8 {
	mut x := [64]u8{}

	// 0..36
	for i := 0; i < key.len; i++ {
		x[i] = key[i]
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

@[direct_array_access; inline]
fn poly1305_key_merge(pkey [36]u8) [64]u8 {
	return key_merge(pkey, [12]u8{}, [8]u8{}, [8]u8{})
}

// See the papers doc on the 3.3 Additional Details part, on page 12-13
// derives new key
@[direct_array_access; inline]
fn fk_k(k [32]u8) [36]u8 {
	// fk(K) = 	K1 ∥ K2 ∥ K3 ∥ 03 ∥ K5 ∥ K6 ∥ K7 ∥ 0c ∥ K9 ∥ K10 ∥ K11 ∥ 30
	//			∥ K4 ∥ K8 ∥ K12 ∥ c0 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32
	// with 0-based index
	// 			K0 ∥ K1 ∥ K2 ∥ 03 ∥ K4 ∥ K5 ∥ K6 ∥ 0c ∥ K8 ∥ K9 ∥ K10 ∥ 30
	//			∥ K3 ∥ K7 ∥ K11 ∥ c0 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := [36]u8{}
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

// derives mac key
@[direct_array_access; inline]
fn fm_k(k [32]u8) [36]u8 {
	// fm(K) = 	K1 ∥ K2 ∥ K3 ∥ 05 ∥ K5 ∥ K6 ∥ K7 ∥ 0a ∥ K9 ∥ K10 ∥ K11 ∥ 50 ∥
	//			K4 ∥ K8 ∥ K12 ∥ a0 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32 ,
	// Or, with 0-based index
	// fm(K) = 	K0 ∥ K1 ∥ K2 ∥ 05 ∥ K4 ∥ K5 ∥ K6 ∥ 0a ∥ K8 ∥ K9 ∥ K10 ∥ 50 ∥
	//			K3 ∥ K7 ∥ K11 ∥ a0 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := [36]u8{}
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

// derives encryption key
fn fe_k(k [32]u8) [36]u8 {
	// fe(K) = 	K1 ∥ K2 ∥ K3 ∥ 06 ∥ K5 ∥ K6 ∥ K7 ∥ 09 ∥ K9 ∥ K10 ∥ K11 ∥ 60 ∥
	// 			K4 ∥ K8 ∥ K12 ∥ 90 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32
	// Or, with 0-based index
	// fe(K) = 	K0 ∥ K1 ∥ K2 ∥ 06 ∥ K4 ∥ K5 ∥ K6 ∥ 09 ∥ K8 ∥ K9 ∥ K10 ∥ 60 ∥
	// 			K3 ∥ K7 ∥ K11 ∥ 90 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := [36]u8{}
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

@[inline]
fn lengths_to_block(len1 usize, len2 usize) []u8 {
	mut length_block := []u8{len: 16}
	// precautions, usize is not the same size on every machine
	size := int(sizeof(len1)) // its also for len2
	mut b1 := []u8{len: size}
	mut b2 := []u8{len: size}
	if size == 8 {
		binary.little_endian_put_u64(mut b1, u64(len1))
		binary.little_endian_put_u64(mut b2, u64(len2))
	}
	if size == 4 {
		binary.little_endian_put_u32(mut b1, u32(len1))
		binary.little_endian_put_u32(mut b2, u32(len2))
	}
	min := if size < 8 { size } else { 8 }
	_ := copy(mut length_block[0..min], b1)
	_ := copy(mut length_block[8..min + 8], b2)
	//     length_block[..min(l1_bytes.len(), 8)].copy_from_slice(&l1_bytes[..min(l1_bytes.len(), 8)]);
	// length_block[8..min(l2_bytes.len(), 8) + 8]
	//    .copy_from_slice(&l2_bytes[..min(l2_bytes.len(), 8)]);

	return length_block
}

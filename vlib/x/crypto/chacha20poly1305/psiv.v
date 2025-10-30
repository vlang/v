// Copyright (c) 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file contains an experimental port of a Rust reference implementation of nonce-misuse
// resistant and key-committing authenticated encryption scheme called ChaCha20-Poly1305-PSIV,
// It backed by `chacha20` stream cipher and `poly1305` message authentication code module.
// Its originally described by Michiel Verbauwhede and the teams on his papers.
// See the detail on the [A Robust Variant of ChaCha20-Poly1305](https://eprint.iacr.org/2025/222).
module chacha20poly1305

import encoding.binary
import crypto.internal.subtle
import x.crypto.chacha20
import x.crypto.poly1305

// new_psiv creates a new Chacha20Poly1305RE with PSIV construct to operate on.
@[direct_array_access]
pub fn new_psiv(key []u8) !&Chacha20Poly1305RE {
	if key.len != key_size {
		return error('new_psiv: bad key size')
	}
	// derives and initializes the new key for later purposes
	pol_key := fk_k(key)
	mac_key := fm_k(key)
	enc_key := fe_k(key)

	mut s := chacha20.State{}
	mut x64 := [64]u8{}
	unsafe { vmemcpy(x64, pol_key[0], 36) }
	unpack_into_state(mut s, x64)
	ws := chacha20_core(s)

	// For poly1305 mac, we only take a first 32-bytes of the state as a key
	mut poly1305_key := []u8{len: 32}
	pack32_from_state(mut poly1305_key, ws)
	po := poly1305.new(poly1305_key)!

	// set the values
	c := &Chacha20Poly1305RE{
		key:     key.clone()
		precomp: true
		mac_key: mac_key
		enc_key: enc_key
		po:      po
	}
	return c
}

// psiv_encrypt encrypts plaintext with provided key, nonce and additional data ad.
// It returns a ciphertext plus message authentication code (mac) contained
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

// Chacha20Poly1305RE is a Chacha20Poly1305 opaque with nonce-misuse resistent
// and key-commiting AEAD scheme with PSIV construct.
@[noinit]
pub struct Chacha20Poly1305RE implements AEAD {
mut:
	// An underlying 32-bytes of key
	key []u8
	// flags that tells derivation keys has been precomputed
	precomp bool
	mac_key [36]u8
	enc_key [36]u8
	po      &poly1305.Poly1305 = unsafe { nil }
}

// free releases resources taken by c. Dont use c after `.free` call.
@[unsafe]
pub fn (mut c Chacha20Poly1305RE) free() {
	unsafe {
		c.key.free()
		// we reset derived keys
		vmemset(c.mac_key, 0, 36)
		vmemset(c.enc_key, 0, 36)
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
pub fn (c &Chacha20Poly1305RE) overhead() int {
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

	// clone the initial poly1305 state and updates it with additional data ad
	mut po_ad := c.po.clone()
	update_with_padding(mut po_ad, ad)
	// make a clone of updated poly1305
	mut po_ad_clone := po_ad.clone()

	// setup output buffer
	mut out := []u8{len: plaintext.len + tag_size}
	// write out an authentication tag into the last tag_size bytes of output
	psiv_gen_tag(mut out[plaintext.len..], mut po_ad_clone, plaintext, ad.len, c.mac_key,
		nonce)
	// write out authenticated encrypted plaintext into the first plaintext.len bytes of output
	psiv_encrypt_internal(mut out[0..plaintext.len], plaintext, c.enc_key, out[plaintext.len..],
		nonce)!

	// return the result
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

	// updates a clone of poly1305 with additional data
	mut po_with_ad := c.po.clone()
	update_with_padding(mut po_with_ad, ad)
	mut poad_clone := po_with_ad.clone()

	// generates authenticated encrypted plaintext with associated mac
	mut out := []u8{len: enc.len}
	psiv_encrypt_internal(mut out, enc, c.enc_key, tag, nonce)!

	mut mac := []u8{len: tag_size}
	psiv_gen_tag(mut mac, mut poad_clone, out, ad.len, c.mac_key, nonce)

	// check if authentication tag was matching or error on fails.
	if subtle.constant_time_compare(mac, tag) != 1 {
		unsafe {
			out.free()
			mac.free()
		}
		return error('unmatching tag')
	}
	// return the decrypted ciphertext
	return out
}

// The AEAD_CHACHA20_POLY1305 PSIV construct helpers
//

// psiv_encrypt_internal is an internal encryption routine used by the core of psiv construct
// for encrypting (or decrypting) message.
@[direct_array_access]
fn psiv_encrypt_internal(mut dst []u8, plaintext []u8, dkey [36]u8, tag []u8, nonce []u8) ! {
	// loads the counter from the first 8-bytes of the tag input
	mut ctr := binary.little_endian_u64(tag[0..8])

	// setup some temporary vars
	mut tc := []u8{len: 8} // counter buffer
	mut s := chacha20.State{}
	mut b64 := [64]u8{} // state buffer
	mut tt := merge_drv_key(dkey, nonce, tag[0..8], tag[8..16])

	mut j := 0
	mut n := 0

	// process for every bytes on plaintext input
	for plaintext[n..].len > 0 {
		// how many block of bytes available to process on
		want_len := if plaintext[n..].len < 64 { plaintext[n..].len } else { 64 }
		// loads current counter
		binary.little_endian_put_u64(mut tc, ctr)

		// updates derived keys with current counter, scrambled with chacha20_core and
		// puts state into b64 buffer
		unsafe { vmemcpy(tt[48], tc.data, tc.len) }
		unpack_into_state(mut s, tt)
		ws := chacha20_core(s)
		pack64_from_state(mut b64, ws)

		// xor every bytes of plaintext with bytes on b64, stores result in dst
		for i in 0 .. want_len {
			dst[j] = plaintext[j] ^ b64[i]
			j++
		}
		// updates current counter and returns error on overflow.
		ctr += 1
		if ctr == 0 {
			return error('counter overflowing')
		}
		n += want_len
	}
	// explicitly reset (release) temporary allocated resources and return the result.
	unsafe {
		tc.free()
		s.reset()
		vmemset(b64, 0, b64.len)
		vmemset(tt, 0, tt.len)
	}
}

// psiv_gen_tag computes a tag from the key, nonce, and Poly1305 tag of the associated data
// and plaintext using the ChaCha20 permutation with the feed-forward, truncating the output.
@[direct_array_access]
fn psiv_gen_tag(mut out []u8, mut po poly1305.Poly1305, input []u8, ad_len int, mac_key [36]u8, nonce []u8) {
	// updates poly1305 mac by input message, associated data length and input length.
	update_with_padding(mut po, input)
	po.update(length_to_block(ad_len, input.len))

	// produces 16-bytes of mac from current poly1305 state.
	po.finish(mut out)

	// The tag was produced from derived key scrambled with chacha20 quarter round routine,
	// and then truncating the output into 16-bytes tag.
	drv_key := merge_drv_key(mac_key, nonce, out[0..8], out[8..16])
	mut x := chacha20.State{}
	unpack_into_state(mut x, drv_key)
	ws := chacha20_core(x)

	// truncating state output into tag_sized bytes. As a note, we reuse buffer previously allocated
	// to store the result.
	pack16_from_state(mut out, ws)

	// explicitly releases (reset) temporary allocated resources
	unsafe {
		vmemset(drv_key, 0, 36)
		ws.reset()
		x.reset()
	}
}

// update_with_padding updates poly1305 mac with data, padding the tail block if necessary.
@[direct_array_access; inline]
fn update_with_padding(mut po poly1305.Poly1305, data []u8) {
	po.update(data)
	rem := data.len % tag_size
	if rem != 0 {
		block := []u8{len: tag_size}
		po.update(block[..tag_size - rem])
	}
}

// merge_drv_key merges provided bytes into 64-bytes key
@[direct_array_access; inline]
fn merge_drv_key(dkey [36]u8, nonce []u8, tag_ctr []u8, tag_rest []u8) [64]u8 {
	mut x64 := [64]u8{}

	// 0..36
	for i := 0; i < dkey.len; i++ {
		x64[i] = dkey[i]
	}
	// 36..48
	for i := 0; i < nonce.len; i++ {
		x64[36 + i] = nonce[i]
	}
	// 48..56
	for i := 0; i < tag_ctr.len; i++ {
		x64[i + 48] = tag_ctr[i]
	}
	// 56..64
	for i := 0; i < tag_rest.len; i++ {
		x64[i + 56] = tag_rest[i]
	}

	return x64
}

// fk_k maps and transforms 32-bytes of key into 36-bytes of new key used to
// derive a poly1305 construction.
// See the papers doc on the 3.3 Additional Details part, on page 12-13
@[direct_array_access; inline]
fn fk_k(key []u8) [36]u8 {
	// fk(K) = 	K1 ∥ K2 ∥ K3 ∥ 03 ∥ K5 ∥ K6 ∥ K7 ∥ 0c ∥ K9 ∥ K10 ∥ K11 ∥ 30
	//			∥ K4 ∥ K8 ∥ K12 ∥ c0 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32
	// with 0-based index
	// 			K0 ∥ K1 ∥ K2 ∥ 03 ∥ K4 ∥ K5 ∥ K6 ∥ 0c ∥ K8 ∥ K9 ∥ K10 ∥ 30
	//			∥ K3 ∥ K7 ∥ K11 ∥ c0 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := [36]u8{}
	// 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = key[i]
	}
	x[3] = u8(0x03)

	// 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = key[i]
	}
	x[7] = 0x0c

	// 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = key[i]
	}
	x[11] = 0x30

	// 12 .. 16
	x[12] = key[3]
	x[13] = key[7]
	x[14] = key[11]
	x[15] = 0xc0

	// 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = key[i - 4]
	}

	return x
}

// fm_k maps and transforms 32-bytes of key into 36-bytes of message authentication key.
// It later used for psiv tag generation.
@[direct_array_access; inline]
fn fm_k(key []u8) [36]u8 {
	// fm(K) = 	K1 ∥ K2 ∥ K3 ∥ 05 ∥ K5 ∥ K6 ∥ K7 ∥ 0a ∥ K9 ∥ K10 ∥ K11 ∥ 50 ∥
	//			K4 ∥ K8 ∥ K12 ∥ a0 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32 ,
	// Or, with 0-based index
	// fm(K) = 	K0 ∥ K1 ∥ K2 ∥ 05 ∥ K4 ∥ K5 ∥ K6 ∥ 0a ∥ K8 ∥ K9 ∥ K10 ∥ 50 ∥
	//			K3 ∥ K7 ∥ K11 ∥ a0 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := [36]u8{}
	// 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = key[i]
	}
	x[3] = u8(0x05)

	// 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = key[i]
	}
	x[7] = 0x0a

	// 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = key[i]
	}
	x[11] = 0x50

	// 12 .. 16
	x[12] = key[3]
	x[13] = key[7]
	x[14] = key[11]
	x[15] = 0xa0

	// 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = key[i - 4]
	}

	return x
}

// fe_k maps and transforms 32-bytes of key into 36-bytes of new encryption key
@[direct_array_access; inline]
fn fe_k(key []u8) [36]u8 {
	// fe(K) = 	K1 ∥ K2 ∥ K3 ∥ 06 ∥ K5 ∥ K6 ∥ K7 ∥ 09 ∥ K9 ∥ K10 ∥ K11 ∥ 60 ∥
	// 			K4 ∥ K8 ∥ K12 ∥ 90 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32
	// Or, with 0-based index
	// fe(K) = 	K0 ∥ K1 ∥ K2 ∥ 06 ∥ K4 ∥ K5 ∥ K6 ∥ 09 ∥ K8 ∥ K9 ∥ K10 ∥ 60 ∥
	// 			K3 ∥ K7 ∥ K11 ∥ 90 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	mut x := [36]u8{}
	// 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = key[i]
	}
	x[3] = u8(0x06)

	// 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = key[i]
	}
	x[7] = 0x09

	// 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = key[i]
	}
	x[11] = 0x60

	// 12 .. 16
	x[12] = key[3]
	x[13] = key[7]
	x[14] = key[11]
	x[15] = 0x90

	// 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = key[i - 4]
	}

	return x
}

// unpack_into_state deserializes (in little-endian form) 64-bytes of data in x into state s.
@[direct_array_access; inline]
fn unpack_into_state(mut s chacha20.State, x [64]u8) {
	for i := 0; i < 16; i++ {
		s[i] = u32(x[i * 4]) | (u32(x[i * 4 + 1]) << u32(8)) | (u32(x[i * 4 + 2]) << u32(16)) | (u32(x[
			i * 4 + 3]) << u32(24))
	}
}

// pack64_from_state serializes state s into 64-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack64_from_state(mut out [64]u8, s chacha20.State) {
	mut j := 0
	for v in s {
		out[j] = u8(v)
		out[j + 1] = u8(v >> u32(8))
		out[j + 2] = u8(v >> u32(16))
		out[j + 3] = u8(v >> u32(24))
		j += 4
	}
}

// pack32_from_state serializes only a half of state s into 32-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack32_from_state(mut out []u8, s chacha20.State) {
	mut j := 0
	for i in 0 .. 8 {
		out[j] = u8(s[i])
		out[j + 1] = u8(s[i] >> u32(8))
		out[j + 2] = u8(s[i] >> u32(16))
		out[j + 3] = u8(s[i] >> u32(24))
		j += 4
	}
}

// pack16_from_state serializes the first quartet of state s into 16-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack16_from_state(mut out []u8, s chacha20.State) {
	mut j := 0
	for i in 0 .. 4 {
		out[j] = u8(s[i])
		out[j + 1] = u8(s[i] >> u32(8))
		out[j + 2] = u8(s[i] >> u32(16))
		out[j + 3] = u8(s[i] >> u32(24))
		j += 4
	}
}

// chacha20_core performs chacha20 quarter round on the state s.
// It returns a copy of updated state after quarter round.
@[direct_array_access; inline]
fn chacha20_core(s chacha20.State) chacha20.State {
	mut ws := s.clone()
	ws.qround(10)
	for i := 0; i < 16; i++ {
		ws[i] += s[i]
	}
	return ws
}

// length_to_block transforms two's length in len1 and len2 into 16-bytes block
@[inline]
fn length_to_block(len1 int, len2 int) []u8 {
	mut block := []u8{len: 16}
	binary.little_endian_put_u64(mut block[0..8], u64(len1))
	binary.little_endian_put_u64(mut block[8..16], u64(len2))

	return block
}

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
	unpack64_into_state(mut s, x64)
	ws := chacha20_core(s)

	// For poly1305 mac, we only take a first 32-bytes of the state as a key
	mut poly1305_key := []u8{len: 32}
	pack32_from_state(mut poly1305_key, ws)
	po := poly1305.new(poly1305_key)!

	// set the values
	c := &Chacha20Poly1305RE{
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
	mac_key [36]u8
	enc_key [36]u8
	po      &poly1305.Poly1305 = unsafe { nil }
	done    bool
}

// free releases resources taken by c. Dont use c after `.free` call.
@[unsafe]
pub fn (mut c Chacha20Poly1305RE) free() {
	unsafe {
		// we only reset derived keys
		vmemset(c.mac_key, 0, 36)
		vmemset(c.enc_key, 0, 36)
		c.po = nil
	}
	// marked this cipher as an unusable anymore
	c.done = true
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
	if c.done {
		panic('encrypt on ciphers that marked as done')
	}
	if nonce.len != nonce_size {
		return error('Chacha20Poly1305RE.encrypt: bad nonce length, only support 12-bytes nonce')
	}
	// clone the initial poly1305 and update poly1305 with additional data
	mut po_ad := c.po.clone()
	update_with_padding(mut po_ad, ad)
	mut po_ad_clone := po_ad.clone()

	// setup destination buffer
	mut out := []u8{len: plaintext.len + tag_size}
	// build the tag
	psiv_gen_tag(mut out[plaintext.len..], mut po_ad_clone, plaintext, ad.len, c.mac_key,
		nonce)
	// generates ciphertext
	psiv_encrypt_internal(mut out[0..plaintext.len], plaintext, c.enc_key, out[plaintext.len..],
		nonce)!

	return out
}

// decrypt decrypts the ciphertext with provided key, nonce and additional data in ad.
// It also tries to validate message authenticated code within ciphertext compared with
// calculated tag. It returns successfully decrypted message or error on fails.
@[direct_array_access]
pub fn (c Chacha20Poly1305RE) decrypt(ciphertext []u8, nonce []u8, ad []u8) ![]u8 {
	if c.done {
		panic('decrypt on ciphers that marked as done')
	}
	if ciphertext.len < tag_size {
		return error('Chacha20Poly1305RE.decrypt: insufficient ciphertext length')
	}
	if nonce.len != nonce_size {
		return error('Chacha20Poly1305RE.decrypt: invalid nonce length provided')
	}
	enc := ciphertext[0..ciphertext.len - c.overhead()]
	tag := ciphertext[ciphertext.len - c.overhead()..]

	mut po_with_ad := c.po.clone()
	update_with_padding(mut po_with_ad, ad)
	mut poad_clone := po_with_ad.clone()

	mut out := []u8{len: enc.len}
	psiv_encrypt_internal(mut out, enc, c.enc_key, tag, nonce)!

	mut mac := []u8{len: tag_size}
	psiv_gen_tag(mut mac, mut poad_clone, out, ad.len, c.mac_key, nonce)

	if subtle.constant_time_compare(mac, tag) != 1 {
		unsafe {
			out.free()
			mac.free()
		}
		return error('unmatching tag')
	}
	return out
}

// The AEAD_CHACHA20_POLY1305 PSIV construct helpers
//

// psiv_encrypt_internal is an internal encryption routine used by the core of psiv construct
// for encrypting (or decrypting) message.
@[direct_array_access]
fn psiv_encrypt_internal(mut dst []u8, plaintext []u8, dkey [36]u8, tag []u8, nonce []u8) ! {
	// loads counter from first 8-bytes of tag input
	mut ctr := binary.little_endian_u64(tag[0..8])

	// setup some temporary vars
	mut tc := []u8{len: 8} // counter buffer
	mut tt := merge_drv_key(dkey, nonce, tag[0..8], tag[8..16])
	mut s := chacha20.State{}
	mut b64 := [64]u8{} // state buffer

	mut j := 0
	mut n := 0
	// process block by block of plaintext bytes
	for plaintext[n..].len > 0 {
		// the length of block of bytes we want to proceed on
		want_len := if plaintext[n..].len < 64 { plaintext[n..].len } else { 64 }
		// loads current counter
		binary.little_endian_put_u64(mut tc, ctr)

		// updates derived keys with the loaded current counter, scrambled with
		// chacha20_core and puts into b64 buffer
		unsafe { vmemcpy(tt[48], tc.data, tc.len) }
		unpack64_into_state(mut s, tt)
		buf := chacha20_core(s)
		pack64_from_state(mut b64, buf)

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
}

// psiv_gen_tag computes a tag from the key, nonce, and Poly1305 tag of the associated data
// and plaintext using the ChaCha20 permutation with the feed-forward, truncating the output.
@[direct_array_access]
fn psiv_gen_tag(mut tag []u8, mut po poly1305.Poly1305, input []u8, ad_len int, mac_key [36]u8, nonce []u8) {
	// updates poly1305 mac by input message, associated data length and input length.
	update_with_padding(mut po, input)
	po.update(length_to_block(ad_len, input.len))

	// produces 16-bytes of tag from current poly1305 state.
	// As a note, we reuse the tag buffer as a temporary output, internally its has the same length.
	po.finish(mut tag)

	// The tag was produced from derived key scrambled with chacha20 quarter round routine,
	// and then truncating the output into 16-bytes tag.
	drv_key := merge_drv_key(mac_key, nonce, tag[0..8], tag[8..16])
	mut x := chacha20.State{}
	unpack64_into_state(mut x, drv_key)
	ws := chacha20_core(x)

	// truncating 64-bytes state and serialized into 16-bytes of tag buffer output
	pack16_from_state(mut tag, ws)
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
	mut x := [64]u8{}

	// index at 0..36
	for i := 0; i < dkey.len; i++ {
		x[i] = dkey[i]
	}

	// index at 36..48
	for i := 0; i < nonce.len; i++ {
		x[36 + i] = nonce[i]
	}

	// index at 48..56
	for i := 0; i < tag_ctr.len; i++ {
		x[i + 48] = tag_ctr[i]
	}

	// index at 56..64
	for i := 0; i < tag_rest.len; i++ {
		x[i + 56] = tag_rest[i]
	}

	return x
}

// fk_k maps and transforms 32-bytes of key into 36-bytes of new key used to
// derive a poly1305 construction.
// See the papers doc on the 3.3 Additional Details part, on page 12-13
@[direct_array_access; inline]
fn fk_k(k []u8) [36]u8 {
	// fk(K) = 	K1 ∥ K2 ∥ K3 ∥ 03 ∥ K5 ∥ K6 ∥ K7 ∥ 0c ∥ K9 ∥ K10 ∥ K11 ∥ 30
	//			∥ K4 ∥ K8 ∥ K12 ∥ c0 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32
	// or with 0-based index
	// 			K0 ∥ K1 ∥ K2 ∥ 03 ∥ K4 ∥ K5 ∥ K6 ∥ 0c ∥ K8 ∥ K9 ∥ K10 ∥ 30
	//			∥ K3 ∥ K7 ∥ K11 ∥ c0 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	//
	// set output buffer
	mut x := [36]u8{}

	// index at 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = k[i]
	}
	x[3] = u8(0x03)

	// index at 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = k[i]
	}
	x[7] = 0x0c

	// index at 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = k[i]
	}
	x[11] = 0x30

	// index at 12 .. 16
	x[12] = k[3]
	x[13] = k[7]
	x[14] = k[11]
	x[15] = 0xc0

	// index 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = k[i - 4]
	}

	return x
}

// fm_k maps and transforms 32-bytes of key into 36-bytes of message authentication key.
// It later used for psiv tag generation.
@[direct_array_access; inline]
fn fm_k(k []u8) [36]u8 {
	// fm(K) = 	K1 ∥ K2 ∥ K3 ∥ 05 ∥ K5 ∥ K6 ∥ K7 ∥ 0a ∥ K9 ∥ K10 ∥ K11 ∥ 50 ∥
	//			K4 ∥ K8 ∥ K12 ∥ a0 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32 ,
	// Or, with 0-based index
	// fm(K) = 	K0 ∥ K1 ∥ K2 ∥ 05 ∥ K4 ∥ K5 ∥ K6 ∥ 0a ∥ K8 ∥ K9 ∥ K10 ∥ 50 ∥
	//			K3 ∥ K7 ∥ K11 ∥ a0 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	//
	// set output buffer
	mut x := [36]u8{}
	// index at 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = k[i]
	}
	x[3] = u8(0x05)

	// index at 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = k[i]
	}
	x[7] = 0x0a

	// index at 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = k[i]
	}
	x[11] = 0x50

	// index at 12 .. 16
	x[12] = k[3]
	x[13] = k[7]
	x[14] = k[11]
	x[15] = 0xa0

	// index at 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = k[i - 4]
	}

	return x
}

// fe_k maps and transforms 32-bytes of key into 36-bytes of new encryption key
@[direct_array_access; inline]
fn fe_k(k []u8) [36]u8 {
	// fe(K) = 	K1 ∥ K2 ∥ K3 ∥ 06 ∥ K5 ∥ K6 ∥ K7 ∥ 09 ∥ K9 ∥ K10 ∥ K11 ∥ 60 ∥
	// 			K4 ∥ K8 ∥ K12 ∥ 90 ∥ K13 ∥ K14 ∥  · · ·  ∥ K32
	// Or, with 0-based index
	// fe(K) = 	K0 ∥ K1 ∥ K2 ∥ 06 ∥ K4 ∥ K5 ∥ K6 ∥ 09 ∥ K8 ∥ K9 ∥ K10 ∥ 60 ∥
	// 			K3 ∥ K7 ∥ K11 ∥ 90 ∥ K12 ∥ K13 ∥  · · ·  ∥ K31
	//
	// set buffer output
	mut x := [36]u8{}

	// index at 0 .. 4
	for i := 0; i < 3; i++ {
		x[i] = k[i]
	}
	x[3] = u8(0x06)

	// index at 4 .. 8
	for i := 4; i < 7; i++ {
		x[i] = k[i]
	}
	x[7] = 0x09

	// index at 8 .. 12
	for i := 8; i < 11; i++ {
		x[i] = k[i]
	}
	x[11] = 0x60

	// index at 12 .. 16
	x[12] = k[3]
	x[13] = k[7]
	x[14] = k[11]
	x[15] = 0x90

	// index at 16 .. 36
	for i := 16; i < 36; i++ {
		x[i] = k[i - 4]
	}

	return x
}

// unpack64_into_state deserializes (in little-endian form) 64-bytes of data in x into state s.
@[direct_array_access; inline]
fn unpack64_into_state(mut s chacha20.State, x [64]u8) {
	for i := 0; i < 16; i++ {
		s[i] = u32(x[i * 4]) | (u32(x[i * 4 + 1]) << u32(8)) | (u32(x[i * 4 + 2]) << u32(16)) | (u32(x[
			i * 4 + 3]) << u32(24))
	}
}

// pack64_from_state serializes state s into 64-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack64_from_state(mut b [64]u8, s chacha20.State) {
	mut j := 0
	for v in s {
		b[j] = u8(v)
		b[j + 1] = u8(v >> u32(8))
		b[j + 2] = u8(v >> u32(16))
		b[j + 3] = u8(v >> u32(24))
		j += 4
	}
}

// pack32_from_state serializes only a half of state s into 32-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack32_from_state(mut b []u8, s chacha20.State) {
	mut j := 0
	for i in 0 .. 8 {
		b[j] = u8(s[i])
		b[j + 1] = u8(s[i] >> u32(8))
		b[j + 2] = u8(s[i] >> u32(16))
		b[j + 3] = u8(s[i] >> u32(24))
		j += 4
	}
}

// pack16_from_state serializes the first quartet of state s into 16-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack16_from_state(mut b []u8, s chacha20.State) {
	mut j := 0
	for i in 0 .. 4 {
		b[j] = u8(s[i])
		b[j + 1] = u8(s[i] >> u32(8))
		b[j + 2] = u8(s[i] >> u32(16))
		b[j + 3] = u8(s[i] >> u32(24))
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

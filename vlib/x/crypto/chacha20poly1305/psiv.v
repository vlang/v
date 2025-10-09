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

import arrays
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

	// clone the initial poly1305
	mut po_ad := c.po.clone()
	update_with_padding(mut po_ad, ad)

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
	update_with_padding(mut po_with_ad, ad)

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

// The AEAD_CHACHA20_POLY1305 PSIV construct helpers
//

// psiv_encrypt_internal is an internal encryption routine used by the core of psiv construct
// for encrypting (or decrypting) message.
@[direct_array_access]
fn psiv_encrypt_internal(plaintext []u8, key []u8, tag []u8, nonce []u8) ![]u8 {
	tctr, trest := split_tag(tag)
	mut ctr := binary.little_endian_u64(tctr)

	mut dst := []u8{cap: plaintext.len}
	mut tc := []u8{len: 8} // counter buffer
	mut s := chacha20.State{}
	mut b64 := []u8{len: 64} // state buffer

	// split out plaintext messages into 64-bytes chunk, and process them
	// chunk by chunk.
	chunks := arrays.chunk[u8](plaintext, 64)
	for chunk in chunks {
		// loads current counter
		binary.little_endian_put_u64(mut tc, ctr)

		// loads 64-bytes of merged key into state s and then perform chacha20 qround.
		// then xor-ing every bytes of result with the bytes in chunk and appended into
		// destination output buffer.
		unpack_into_state(mut s, merge_drv_key(key, nonce, tc, trest))
		buf := chacha20_core(s)
		pack64_from_state(mut b64, buf)
		for i, v in chunk {
			o := v ^ b64[i]
			dst << o
		}
		// updates current counter and returns error on overflow.
		ctr += 1
		if ctr == 0 {
			return error('counter overflowing')
		}
	}
	// reset (release) temporary allocated resources
	unsafe {
		tc.free()
		s.reset()
		b64.free()
	}
	return dst
}

// psiv_gen_tag computes a tag from the key, nonce, and Poly1305 tag of the associated data
// and plaintext using the ChaCha20 permutation with the feed-forward, truncating the output.
@[direct_array_access]
fn psiv_gen_tag(mut po poly1305.Poly1305, input []u8, ad_len int, mac_key []u8, nonce []u8) []u8 {
	// updates poly1305 mac by input message, associated data length and input length.
	update_with_padding(mut po, input)
	po.update(length_to_block(ad_len, input.len))

	// produces 16-bytes of mac from current poly1305 state.
	mut digest := []u8{len: tag_size}
	po.finish(mut digest)

	// The tag was produced from derived key scrambled with chacha20 quarter round routine,
	// and then truncating the output into 16-bytes tag.
	drv_key := merge_drv_key(mac_key, nonce, digest[0..8], digest[8..16])
	mut x := chacha20.State{}
	unpack_into_state(mut x, drv_key)
	ws := chacha20_core(x)

	// truncating state output into tag sized bytes
	mut tag := []u8{len: tag_size}
	pack16_from_state(mut tag, ws)

	// releases (reset) temporary allocated resources
	unsafe {
		drv_key.free()
		digest.free()
		ws.reset()
		x.reset()
	}
	return tag
}

// psiv_init initializes and expands master key into desired psiv needed construct.
@[direct_array_access; inline]
fn psiv_init(key []u8) !([]u8, []u8, &poly1305.Poly1305) {
	// derives some keys
	pol_key := fk_k(key)
	mac_key := fm_k(key)
	enc_key := fe_k(key)

	mut x := chacha20.State{}
	unpack_into_state(mut x, merge_drvk_zeros(pol_key))
	ws := chacha20_core(x)

	// For poly1305 mac, we only take a first 32-bytes of the state as a key
	mut poly1305_key := []u8{len: 32}
	pack32_from_state(mut poly1305_key, ws)
	po := poly1305.new(poly1305_key)!

	// reset (release) temporary allocated resources
	unsafe {
		x.reset()
		ws.reset()
		pol_key.free()
		poly1305_key.free()
	}
	return mac_key, enc_key, po
}

// update_with_padding updates poly1305 mac with data, padding the tail block if necessary.
@[direct_array_access; inline]
fn update_with_padding(mut po poly1305.Poly1305, data []u8) {
	nr_blocks := data.len / tag_size

	// update poly1305 state with block that aligns with tag_size-d block
	if nr_blocks > 0 {
		block := unsafe { data[0..nr_blocks * tag_size] }
		po.update(block)
	}
	// for partial non-aligned block, pad it with zeros to align with tag_size
	if data.len % tag_size != 0 {
		last_block := unsafe { data[nr_blocks * tag_size..] }
		mut padded_block := []u8{len: tag_size}
		_ := copy(mut padded_block, last_block)
		po.update(padded_block)
	}
}

// merge_drv_key merges provided bytes into 64-bytes key
@[direct_array_access; inline]
fn merge_drv_key(dkey []u8, nonce []u8, tag_ctr []u8, tag_rest []u8) []u8 {
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
		x[i] = k[i - 4]
	}

	return x
}

// fm_k maps and transforms 32-bytes of key into 36-bytes of message authentication key.
// It later used for psiv tag generation.
@[direct_array_access; inline]
fn fm_k(k []u8) []u8 {
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
		x[i] = k[i - 4]
	}

	return x
}

// fe_k maps and transforms 32-bytes of key into 36-bytes of new encryption key
@[direct_array_access; inline]
fn fe_k(k []u8) []u8 {
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
		x[i] = k[i - 4]
	}

	return x
}

// unpack_into_state deserializes (in little-endian form) 64-bytes of data in x into state s.
@[direct_array_access; inline]
fn unpack_into_state(mut s chacha20.State, x []u8) {
	for i := 0; i < 16; i++ {
		s[i] = binary.little_endian_u32(x[i * 4..(i + 1) * 4])
	}
}

// pack64_from_state serializes state s into 64-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack64_from_state(mut out []u8, s chacha20.State) {
	for i, v in s {
		binary.little_endian_put_u32(mut out[i * 4..(i + 1) * 4], v)
	}
}

// pack32_from_state serializes only a half of state s into 32-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack32_from_state(mut out []u8, s chacha20.State) {
	for i := 0; i < 8; i++ {
		binary.little_endian_put_u32(mut out[i * 4..(i + 1) * 4], s[i])
	}
}

// pack16_from_state serializes the first quartet of state s into 16-bytes output in little-endian form.
@[direct_array_access; inline]
fn pack16_from_state(mut out []u8, s chacha20.State) {
	for i := 0; i < 4; i++ {
		binary.little_endian_put_u32(mut out[i * 4..(i + 1) * 4], s[i])
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

// split_tag splits 16-bytes of tag into two's 8-bytes block.
@[direct_array_access; inline]
fn split_tag(tag []u8) ([]u8, []u8) {
	return tag[0..8].clone(), tag[8..16].clone()
}

// length_to_block transforms two's length in len1 and len2 into 16-bytes block
@[inline]
fn length_to_block(len1 int, len2 int) []u8 {
	mut block := []u8{len: 16}
	binary.little_endian_put_u64(mut block[0..8], u64(len1))
	binary.little_endian_put_u64(mut block[8..16], u64(len2))

	return block
}

// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file implements an Authenticated Encryption with Associated Data based on Ascon-AEAD128,
// AEAD Scheme defined in NIST.SP.800-232 standard.
module ascon

import encoding.binary
import crypto.internal.subtle

// The constants for Ascon-AEAD128
//
// key_size is 128-bit size of Ascon-AEAD128 key
pub const key_size = 16
// nonce_size is 128-bit size of Ascon-AEAD128 nonce
pub const nonce_size = 16
// tag_size is 128-bit size of Ascon-AEAD128 authentication tag
pub const tag_size = 16

// aead128_iv is a precomputed initialization phase values for Ascon-AEAD128
// See Table 14 of NIST SP 800-232 doc.
const aead128_iv = u64(0x0000_1000_808c_0001)

// aead128_data_limit is a limit amount of data processed during encryption and decryption,
// including the nonce, shall not exceed 2⁵⁴ bytes for a given key.
const aead128_data_limit = u64(1) << 54 - 1

// aead128_block_size is a number (rate) of input bytes processed per invocation of the underlying of state.
// Ascon-AEAD128 working with 128-bit rate
const aead128_block_size = 16

// encrypt encrypts the message under provided key and nonce and supplied additional data in ad.
// It returns an authenticated output of Ascon-AEAD128 ciphertext where authentication tag
// was stored within the end of ciphertext.
// Note: The Ascon-AEAD128 key shall be kept secret,
pub fn encrypt(key []u8, nonce []u8, ad []u8, msg []u8) ![]u8 {
	// Preliminary check
	if key.len != key_size {
		return error('encrypt: invalid key size')
	}
	if nonce.len != nonce_size {
		return error('encrypt: invalid nonce size')
	}
	// The key shall be updated to a new key once the total amount of input data reaches the limit
	data_length := u64(nonce.len) + u64(msg.len) + u64(ad.len)
	if data_length > aead128_data_limit {
		return error('encrypt: exceed data limit')
	}
	mut s := State{}
	mut out := []u8{len: msg.len + tag_size}

	// Ascon-AEAD128 comprises four phases:
	// 	- initialization of the state,
	// 	- associated data processing,
	// 	- plaintext processing,
	// 	- and finalization (includes writing the tag).
	k0, k1 := aead128_init(mut s, key, nonce)
	aead128_process_ad(mut s, ad)
	loc := aead128_process_msg(mut out, mut s, msg)
	aead128_finalize(mut s, k0, k1)
	aead128_write_tag(mut out, s, loc)

	// clean out the intermediate Ascon state
	reset_state(mut s)
	return out
}

// decrypt decrypts authenticated encrypted messages in ciphertext that encrypted under
// provided key and nonce with additional data in `ad`.
// It would check if authentication tag mas matching and return decrypted message
// if success or error on fails.
@[direct_array_access]
pub fn decrypt(key []u8, nonce []u8, ad []u8, ciphertext []u8) ![]u8 {
	// Preliminary check
	if key.len != key_size {
		return error('decrypt: invalid key size')
	}
	if nonce.len != nonce_size {
		return error('decrypt: invalid nonce size')
	}
	if ciphertext.len < tag_size {
		return error('decrypt: invalid ciphertext size')
	}
	data_length := u64(nonce.len) + u64(ciphertext.len) + u64(ad.len)
	if data_length > aead128_data_limit {
		return error('decrypt: exceed data limit')
	}
	mut s := State{}
	// Initialization phase and additional data processing
	k0, k1 := aead128_init(mut s, key, nonce)
	aead128_process_ad(mut s, ad)

	// Decryption phase, start by slicing the ciphertext
	cmsg := ciphertext[0..ciphertext.len - tag_size]
	stag := ciphertext[ciphertext.len - tag_size..ciphertext.len]
	mut msg := []u8{len: ciphertext.len - tag_size}

	// Partially decrypt the cmsg and stored into msg buffer
	aead128_partial_dec(mut msg, mut s, cmsg)

	// Finalizes the state and calc the tag and compares with expected tag.
	// It would return error if the tag was unmatching.
	aead128_finalize(mut s, k0, k1)
	mut ctag := []u8{len: tag_size}
	aead128_write_tag(mut ctag, s, 0)
	if subtle.constant_time_compare(ctag, stag) != 1 {
		// clean up
		unsafe {
			msg.reset()
			ctag.reset()
		}
		reset_state(mut s)
		return error('decrypt: unmatching tag')
	}
	return msg
}

// Aead128 is an opaque provides an implementation of Ascon-AEAD128 from NIST.SP.800-232 standard.
// Its implements `x.crypto.chacha20poly1305.AEAD` interfaces.
@[noinit]
pub struct Aead128 {
	State
mut:
	// 32-bytes of underlying key
	key [2]u64
}

// new_aead128 creates a new Aead128 instance and initialized with supplied key.
@[direct_array_access]
pub fn new_aead128(key []u8) !&Aead128 {
	if key.len != key_size {
		return error('invalid cipher key size')
	}
	k0 := binary.little_endian_u64(key[0..8])
	k1 := binary.little_endian_u64(key[8..16])

	mut c := &Aead128{}
	// Partially initializes state
	c.State.e0 = aead128_iv
	c.State.e1 = k0
	c.State.e2 = k1
	// stores the key
	c.key[0] = k0
	c.key[1] = k1

	return c
}

// nonce_size returns the nonce size of Ascon-AEAD128 Aead128.
pub fn (c &Aead128) nonce_size() int {
	return nonce_size
}

// overhead returns the maximum difference between the lengths of a plaintext and its ciphertext.
pub fn (c &Aead128) overhead() int {
	return tag_size
}

// encrypt encrypts the message under provided key and nonce and supplied additional data in ad.
// It returns an authenticated output of Ascon-AEAD128 ciphertext where authentication tag
// was stored within the end of ciphertext.
@[direct_array_access]
pub fn (mut c Aead128) encrypt(msg []u8, nonce []u8, ad []u8) ![]u8 {
	// Check for the nonce
	if nonce.len != nonce_size {
		return error('encrypt: invalid nonce size')
	}
	data_length := u64(msg.len) + u64(ad.len) + 32
	if data_length > aead128_data_limit {
		return error('encrypt: exceed data limit')
	}
	// Initialization phase
	n0 := binary.little_endian_u64(nonce[0..8])
	n1 := binary.little_endian_u64(nonce[8..16])
	// setup state
	c.State.e0 = aead128_iv
	c.State.e1 = c.key[0]
	c.State.e2 = c.key[1]
	c.State.e3 = n0
	c.State.e4 = n1

	// Update state by permutation
	ascon_pnr(mut c.State, .ascon_prnd_12)
	// XOR-ing with the cipher's key
	c.State.e3 ^= c.key[0]
	c.State.e4 ^= c.key[1]

	// Associated data processing
	aead128_process_ad(mut c.State, ad)

	// Message processing
	mut dst := []u8{len: msg.len + tag_size}
	n := aead128_process_msg(mut dst, mut c.State, msg)

	// Finalization and writes out the tag into dst
	aead128_finalize(mut c.State, c.key[0], c.key[1])
	aead128_write_tag(mut dst, c.State, n)

	return dst
}

// decrypt decrypts the ciphertext and validates authentication tag with
// provided nonce and additional data ad. It returns error on fails or tag unmatching.
@[direct_array_access]
pub fn (mut c Aead128) decrypt(ciphertext []u8, nonce []u8, ad []u8) ![]u8 {
	// Check for nonce
	if nonce.len != nonce_size {
		return error('bad nonce size')
	}
	// Check for ciphertext length, its ahould have length >= tag_size
	if ciphertext.len < tag_size {
		return error('bad ciphertext size')
	}
	// check for data limit overflow
	data_length := u64(ciphertext.len) + u64(ad.len) + 32
	if data_length > aead128_data_limit {
		return error('decrypt: exceed data limit')
	}
	// load nonce
	n0 := binary.little_endian_u64(nonce[0..8])
	n1 := binary.little_endian_u64(nonce[8..16])

	// Reinitialize internal state
	c.State.e0 = aead128_iv
	c.State.e1 = c.key[0]
	c.State.e2 = c.key[1]
	c.State.e3 = n0
	c.State.e4 = n1

	// scrambled with permutation routine
	ascon_pnr(mut c.State, .ascon_prnd_12)
	// xor-ing with the cipher's key
	c.State.e3 ^= c.key[0]
	c.State.e4 ^= c.key[1]

	// Associated data processing
	//
	aead128_process_ad(mut c.State, ad)

	// As we know, ciphertext length was sum of encrypted mesage length plus tag_size
	// Lets slicing it
	cxt_len := ciphertext.len
	cmsg := ciphertext[0..cxt_len - tag_size]
	ctag := ciphertext[cxt_len - tag_size..cxt_len]

	mut out := []u8{len: cxt_len - tag_size}
	aead128_partial_dec(mut out, mut c.State, cmsg)
	aead128_finalize(mut c.State, c.key[0], c.key[1])

	// tag verification
	mut tag := []u8{len: tag_size}
	aead128_write_tag(mut tag, c.State, 0)
	if subtle.constant_time_compare(ctag, tag) != 1 {
		// Cleans up previously produces bytes (state)
		reset_state(mut c.State)
		unsafe {
			tag.free()
			out.free()
		}
		return error('Aead128.decrypt: unmatching tag')
	}
	return out
}

// Helpers for Ascon-AEAD128
//

// aead128_init initializes Ascon-AEAD128 state by provided key and nonce.
// Its return two's u64 values from deserialized key bytes in little-endian form.
@[direct_array_access; inline]
fn aead128_init(mut s State, key []u8, nonce []u8) (u64, u64) {
	// load key and nonce into state in little-endian form,
	// The endianness has been switched from big endian to little endian
	k0 := binary.little_endian_u64(key[0..8])
	k1 := binary.little_endian_u64(key[8..16])

	n0 := binary.little_endian_u64(nonce[0..8])
	n1 := binary.little_endian_u64(nonce[8..16])

	// Given a 128-bit 𝐾 and a 128-bit 𝑁, the 320-bit internal
	// state S is initialized as the concatenation of 𝐼𝑉, 𝐾, and 𝑁:
	// S ← 𝐼𝑉 || 𝐾 || 𝑁,
	s.e0 = aead128_iv
	s.e1 = k0
	s.e2 = k1
	s.e3 = n0
	s.e4 = n1

	// updates State using the permutation 𝐴𝑠𝑐𝑜𝑛-𝑝[12], S ← 𝐴𝑠𝑐𝑜𝑛-𝑝[12](S)
	ascon_pnr(mut s, .ascon_prnd_12)

	// Then XORing the secret key 𝐾 into the last 128 bits of internal state:
	// S ← S ⊕ (0¹⁹² ∥ 𝐾).
	s.e3 ^= k0
	s.e4 ^= k1

	return k0, k1
}

// aead128_process_ad absorbs associated data into Ascon-AEAD128 state.
@[direct_array_access; inline]
fn aead128_process_ad(mut s State, ad []u8) {
	mut ad_length := ad.len
	mut ad_idx := 0
	if ad_length > 0 {
		for ad_length >= aead128_block_size {
			// Each associated data block 𝐴𝑖 (0 ≤ 𝑖 ≤ 𝑚) is absorbed into the first 128 bits of
			// state as S[0∶127] ← S[0∶127] ⊕ 𝐴𝑖,
			block := unsafe { ad[ad_idx..ad_idx + aead128_block_size] }
			s.e0 ^= binary.little_endian_u64(block[0..8])
			s.e1 ^= binary.little_endian_u64(block[8..16])

			// Apply permutation 𝐴𝑠𝑐𝑜𝑛-𝑝[8] to the state
			ascon_pnr(mut s, .ascon_prnd_8)
			// Updates index
			ad_length -= aead128_block_size
			ad_idx += aead128_block_size
		}
		// process partial block if it exists
		if ad_length >= 8 {
			first_block := unsafe { ad[ad_idx..ad_idx + 8] }
			s.e0 ^= binary.little_endian_u64(first_block)

			// Is there more bytes to process on?
			last_block := unsafe { ad[ad_idx + 8..] }
			s.e1 ^= pad(last_block.len)
			if last_block.len > 0 {
				s.e1 ^= u64_from_partial_bytes(last_block)
			}
			// update index
			ad_length -= first_block.len + last_block.len
			ad_idx += first_block.len + last_block.len
		} else {
			last_block := unsafe { ad[ad_idx..] }
			s.e0 ^= pad(last_block.len)
			if last_block.len > 0 {
				s.e0 ^= u64_from_partial_bytes(last_block)
			}
		}
		// Apply permutation 𝐴𝑠𝑐𝑜𝑛-𝑝[8] to the state
		ascon_pnr(mut s, .ascon_prnd_8)
	}
	// The final step of processing associated data is to update the state
	// with a constant that provides domain separation.
	s.e4 ^= u64(0x8000_0000_0000_0000)
}

// aead128_process_msg process (encrypt) the messages msg and asborb it into Ascon-AEAD128 state
// Its written the result into out buffer and return the number of bytes has been written.
@[direct_array_access; inline]
fn aead128_process_msg(mut out []u8, mut s State, msg []u8) int {
	mut pos := 0
	mut mlen := msg.len
	mut midx := 0
	for mlen >= aead128_block_size {
		block := unsafe { msg[midx..midx + aead128_block_size] }
		s.e0 ^= binary.little_endian_u64(block[0..8])
		s.e1 ^= binary.little_endian_u64(block[8..16])
		// stores
		binary.little_endian_put_u64(mut out[pos..pos + 8], s.e0)
		binary.little_endian_put_u64(mut out[pos + 8..], s.e1)
		// apply permutation
		ascon_pnr(mut s, .ascon_prnd_8)

		// updates index
		mlen -= aead128_block_size
		pos += aead128_block_size
		midx += aead128_block_size
	}
	// process partial block if it exists
	if mlen >= 8 {
		mut block := unsafe { msg[midx..] }
		s.e0 ^= load_bytes(block[0..8], 8)
		s.e1 ^= load_bytes(block[8..], mlen - 8)
		store_bytes(mut out[pos..], s.e0, 8)
		store_bytes(mut out[pos + 8..], s.e1, mlen - 8)
		s.e1 ^= pad(mlen - 8)
	} else {
		last_block := unsafe { msg[midx..] }
		s.e0 ^= load_bytes(last_block, last_block.len)
		store_bytes(mut out[pos..], s.e0, last_block.len)
		s.e0 ^= pad(last_block.len)
	}
	// how much we have written
	pos += mlen

	return pos
}

// aead128_partial_dec partially decrypts the encrypted part of the message in the cmsg,
// and stored into out buffer.
// Note: The output buffer should have the same length with cmsg, ie, out.len == cmsg.len
@[direct_array_access]
fn aead128_partial_dec(mut out []u8, mut s State, cmsg []u8) {
	mut cmsg_len := cmsg.len
	mut pos := 0
	// assert out.len == cmsg.len
	for cmsg_len >= aead128_block_size {
		block := unsafe { cmsg[pos..pos + aead128_block_size] }
		c0 := binary.little_endian_u64(block[0..8])
		c1 := binary.little_endian_u64(block[8..16])

		binary.little_endian_put_u64(mut out[pos..pos + 8], s.e0 ^ c0)
		binary.little_endian_put_u64(mut out[pos + 8..pos + 16], s.e1 ^ c1)

		s.e0 = c0
		s.e1 = c1

		ascon_pnr(mut s, .ascon_prnd_8)
		// updates index
		pos += aead128_block_size
		cmsg_len -= aead128_block_size
	}
	// partial block
	if cmsg_len >= 8 {
		mut first_block := unsafe { cmsg[pos..pos + 8] }
		c0 := binary.little_endian_u64(first_block)
		binary.little_endian_put_u64(mut out[pos..pos + 8], c0 ^ s.e0)

		last_block := unsafe { cmsg[pos + 8..] }
		c1 := load_bytes(last_block, last_block.len)
		store_bytes(mut out[pos + 8..], c1 ^ s.e1, last_block.len)

		s.e0 = c0
		s.e1 = clear_bytes(s.e1, last_block.len)
		s.e1 |= c1
		s.e1 ^= pad(last_block.len)
	} else {
		last_block := unsafe { cmsg[pos..] }
		c0 := load_bytes(last_block, last_block.len)
		store_bytes(mut out[pos..], s.e0 ^ c0, last_block.len)
		s.e0 = clear_bytes(s.e0, last_block.len)
		s.e0 |= c0
		s.e0 ^= pad(last_block.len)
	}
}

// aead128_finalize does finalization step and generates tag value.
fn aead128_finalize(mut s State, k0 u64, k1 u64) {
	// Load the key into state, S ← S ⊕ (0¹²⁸ ∥ 𝐾 ∥ 0⁶⁴),
	s.e2 ^= k0
	s.e3 ^= k1
	// then updated using the permutation 𝐴𝑠𝑐𝑜𝑛-𝑝[12]
	ascon_pnr(mut s, .ascon_prnd_12)

	// Finally, the tag 𝑇 is generated by XORing the key with the last 128 bits of the state:
	// 𝑇 ← 𝑆[192∶319] ⊕ 𝐾.
	s.e3 ^= k0
	s.e4 ^= k1
}

// aead128_write_tag writes tag from state into out at loc offset.
@[direct_array_access; inline]
fn aead128_write_tag(mut out []u8, s State, loc int) {
	binary.little_endian_put_u64(mut out[loc..loc + 8], s.e3)
	binary.little_endian_put_u64(mut out[loc + 8..loc + 16], s.e4)
}

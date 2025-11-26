// Copyright (c) 2024 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// AEAD_CHACHA20_POLY1305 is an authenticated encryption with additional data algorithm.
// The inputs to AEAD_CHACHA20_POLY1305 are:
//   A 256-bit key
//   A 64-bit nonce, 96-bit (or bigger 192 bit nonce) -- different for each invocation with the same key
//   An arbitrary length plaintext
//   Arbitrary length additional authenticated data (AAD)
module chacha20poly1305

import encoding.binary
import crypto.internal.subtle
import x.crypto.chacha20
import x.crypto.poly1305

// This interface was a proposed draft for Authenticated Encryption with Additional Data (AEAD)
// interface `AEAD` likes discussion at discord channel.
// see https://discord.com/channels/592103645835821068/592320321995014154/1206029352412778577
// But its little modified to be more v-idiomatic.
// Note: This interface should be more appropriately located in `crypto.cipher`, we can move
// it into `crypto.cipher` later.
// Authenticated Encryption with Additional Data (AEAD) interface
pub interface AEAD {
	// nonce_size return the nonce size (in bytes) used by this AEAD algorithm that should be
	// passed to `.encrypt` or `.decrypt`.
	nonce_size() int
	// overhead returns the maximum difference between the lengths of a plaintext and its ciphertext.
	overhead() int
	// encrypt encrypts and authenticates the provided plaintext along with a nonce, and
	// to be authenticated additional data in `ad`.
	// It returns ciphertext bytes where its encoded form is up to implementation and
	// not dictated by the interfaces.
	// Usually its contains encrypted text plus some authentication tag, and maybe some other bytes.
	encrypt(plaintext []u8, nonce []u8, ad []u8) ![]u8
	// decrypt decrypts and authenticates (verifies) the provided ciphertext along with a nonce, and
	// additional data. If verified successfully, it returns the plaintext and error otherwise.
	decrypt(ciphertext []u8, nonce []u8, ad []u8) ![]u8
}

// key_size is the size of key (in bytes) which the Chacha20Poly1305 AEAD accepts.
pub const key_size = 32

// orig_nonce_size is the size (in bytes) of nonce of the original (DJ Bernstein) variant
// which the Chacha20Poly1305 AEAD accepts.
pub const orig_nonce_size = 8
// nonce_size is the size of the standard nonce (in bytes) which the Chacha20Poly1305 AEAD accepts.
pub const nonce_size = 12
// nonce_size is the size of the extended nonce (in bytes) which the Chacha20Poly1305 AEAD accepts.
pub const x_nonce_size = 24

// tag_size is the size of the message authenticated code (in bytes) produced by Chacha20Poly1305 AEAD.
pub const tag_size = 16

// encrypt does one-shot encryption of given plaintext with associated key, nonce and additional data.
// It return ciphertext output and authenticated tag appended into it.
pub fn encrypt(plaintext []u8, key []u8, nonce []u8, ad []u8, opt chacha20.Options) ![]u8 {
	mut c := new(key, nonce.len, opt)!
	return c.encrypt(plaintext, nonce, ad)!
}

// decrypt does one-shot decryption of given ciphertext with associated key, nonce and additional data.
// It return plaintext output and verify if resulting tag is a valid message authenticated code (mac)
// for given message, key and additional data.
pub fn decrypt(ciphertext []u8, key []u8, nonce []u8, ad []u8, opt chacha20.Options) ![]u8 {
	mut c := new(key, nonce.len, opt)!
	return c.decrypt(ciphertext, nonce, ad)!
}

// Chacha20Poly1305 represents AEAD algorithm backed by `x.crypto.chacha20` and `x.crypto.poly1305`.
@[noinit]
struct Chacha20Poly1305 {
mut:
	key   []u8 = []u8{len: key_size}
	nsize int
	opt   chacha20.Options // for XChaCha20 construct
}

// new creates a new Chacha20Poly1305 AEAD instance with given 32 bytes of key
// and the nonce size in nsize. The nsize should be 8, 12 or 24 length, otherwise it would return error.
pub fn new(key []u8, nsize int, opt chacha20.Options) !&AEAD {
	if key.len != key_size {
		return error('chacha20poly1305: bad key size')
	}
	if nsize != orig_nonce_size && nsize != nonce_size && nsize != x_nonce_size {
		return error('chacha20poly1305: bad nonce size supplied, its should 8, 12 or 24')
	}
	return &Chacha20Poly1305{
		key:   key
		nsize: nsize
		opt:   opt
	}
}

// nonce_size returns the size of underlying nonce (in bytes) of AEAD algorithm.
pub fn (c Chacha20Poly1305) nonce_size() int {
	return c.nsize
}

// overhead returns maximum difference between the lengths of a plaintext to be encrypted and
// ciphertext's output. In the context of Chacha20Poly1305, `.overhead() == .tag_size`.
pub fn (c Chacha20Poly1305) overhead() int {
	return tag_size
}

// encrypt encrypts plaintext, along with nonce and additional data and generates
// authenticated tag appended into ciphertext's output.
pub fn (c Chacha20Poly1305) encrypt(plaintext []u8, nonce []u8, ad []u8) ![]u8 {
	// makes sure if the nonce length is matching with internal nonce size
	if nonce.len != c.nonce_size() {
		return error('chacha20poly1305: unmatching nonce size')
	}
	// check if the plaintext length doesn't exceed the amount of limit.
	// its comes from the internal of chacha20 mechanism, where the counter are u32
	// with the facts of chacha20 operates on 64 bytes block, we can measure the amount
	// of encrypted data possible in a single invocation, ie.,
	// amount = (2^32-1)*64 = 274,877,906,880 bytes, or nearly 256 GB
	if u64(plaintext.len) > (u64(1) << 38) - 64 {
		panic('chacha20poly1305: plaintext too large')
	}
	if ad.len > max_u64 {
		return error('chacha20poly1305: something bad in your additional data')
	}
	return c.generic_crypt(plaintext, nonce, ad, .encrypt)!
}

// decrypt decrypts ciphertext along with provided nonce and additional data.
// Decryption is similar with the encryption process with slight differences in:
// The roles of ciphertext and plaintext are reversed, so the ChaCha20 encryption
// function is applied to the ciphertext, producing the plaintext.
// The Poly1305 function is still run on the AAD and the ciphertext, not the plaintext.
// The calculated mac is bitwise compared to the received mac.
// The message is authenticated if and only if the tags match, return error if failed to verify.
pub fn (c Chacha20Poly1305) decrypt(ciphertext []u8, nonce []u8, ad []u8) ![]u8 {
	// Preliminary check
	if ciphertext.len < tag_size {
		return error('chacha20poly1305: ciphertext length does not meet minimum required length')
	}
	if nonce.len != c.nonce_size() {
		return error('chacha20poly1305: unmatching nonce size')
	}
	// ciphertext max = plaintext max length  + tag length
	// ie, (2^32-1)*64 + overhead = (u64(1) << 38) - 64 + 16 = 274,877,906,896 octets.
	if u64(ciphertext.len) > (u64(1) << 38) - 48 {
		return error('chacha20poly1305: ciphertext too large')
	}
	return c.generic_crypt(ciphertext, nonce, ad, .decrypt)!
}

// Helpers

// generic_crypt direction
enum Mode {
	encrypt = 0
	decrypt = 1
}

// generic_crypt does generic encryption or decryption based on the mode flag was passed.
// See AEAD Construction at https://datatracker.ietf.org/doc/html/rfc8439#section-2.8
@[direct_array_access; inline]
fn (c Chacha20Poly1305) generic_crypt(msg []u8, nonce []u8, ad []u8, mode Mode) ![]u8 {
	// Setup some values
	mut src := unsafe { msg }
	mut mac := []u8{} // used in decryption
	if mode == .decrypt {
		src = unsafe { msg[0..msg.len - c.overhead()] }
		mac = unsafe { msg[msg.len - c.overhead()..] }
	}

	// generates 32-bytes of one-time key for later poly1305 operation
	mut otkey := []u8{len: key_size}
	mut s := chacha20.new_cipher(c.key, nonce, c.opt)!
	s.encrypt(mut otkey, otkey)!

	// destination buffer, with overhead spaces for generated tag without reallocating
	mut dst := []u8{len: src.len, cap: src.len + c.overhead()}

	// Next, the ChaCha20 encryption function is called to encrypt (decrypt) message input,
	// using the same key and nonce, and with the initial counter set to 1.
	s.set_counter(1)
	s.encrypt(mut dst, src)!

	// Finally, the Poly1305 function is called with the Poly1305 key calculated above
	// to build message authentication code (tag).
	// length of constructed message
	cm_length := if mode == .encrypt {
		length_constructed_msg(ad, dst)
	} else {
		length_constructed_msg(ad, src)
	}
	mut constructed_msg := []u8{len: cm_length}
	if mode == .encrypt {
		construct_msg(mut constructed_msg, ad, dst)
	} else {
		construct_msg(mut constructed_msg, ad, src)
	}
	mut tag := []u8{len: tag_size}
	mut po := poly1305.new(otkey)!
	po.update(constructed_msg)
	po.finish(mut tag)

	// If this a decryption mode, lets verify whether this calculated tag was matching
	// with the supplied mac, otherwise return error on fails and free allocated resources.
	if mode == .decrypt {
		if subtle.constant_time_compare(mac, tag) != 1 {
			// free allocated resource
			unsafe {
				s.free()
				tag.free()
				otkey.free()
				dst.free()
				constructed_msg.free()
			}
			return error('chacha20poly1305: unmatching tag')
		} else {
			// return the decrypted message (plaintext) when the tag was matching
			return dst
		}
	}
	// In the encryption mode, appends the tag into end of destination buffer
	dst << tag
	return dst
}

// pad x to 16 bytes block
@[direct_array_access; inline]
fn pad_to_16(x []u8) []u8 {
	if x.len % 16 == 0 {
		return x
	}
	mut out := []u8{len: x.len + (16 - x.len % 16)}
	_ := copy(mut out, x)
	return out
}

// The length of padded x
@[inline]
fn length_pad_to_16(x []u8) int {
	if x.len % 16 == 0 {
		return x.len
	}
	//
	return x.len + (16 - x.len % 16)
}

// The length of constructed message
@[inline]
fn length_constructed_msg(ad []u8, bytes []u8) int {
	mut n := 0
	n += length_pad_to_16(ad)
	n += length_pad_to_16(bytes)
	n += 16 // 2 * 8 bytes
	return n
}

// construct_msg builds a message for later usage and stored into out.
// The last step on the AEAD Construction on the how the message was constructed.
// See the details on the [2.8](https://datatracker.ietf.org/doc/html/rfc8439#section-2.8)
// The message constructed as a concatenation of the following:
// 	*  padded to multiple of 16 bytes block of the additional data bytes
// 	*  padded to multiple of 16 bytes block of the ciphertext (or plaintext) bytes
// 	*  The length of the additional data in octets (as a 64-bit little-endian integer).
// 	*  The length of the ciphertext (or plaintext) in octets (as a 64-bit little-endian integer).
// Assumed the output buffer length was correctly initialized.
@[direct_array_access; inline]
fn construct_msg(mut out []u8, ad []u8, bytes []u8) {
	n0 := copy(mut out, pad_to_16(ad))
	n1 := copy(mut out[n0..], pad_to_16(bytes))
	binary.little_endian_put_u64(mut out[n0 + n1..], u64(ad.len))
	binary.little_endian_put_u64(mut out[n0 + n1 + 8..], u64(bytes.len))
}

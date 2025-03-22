// Copyright (c) 2024 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// AEAD_CHACHA20_POLY1305 is an authenticated encryption with additional data algorithm.
// The inputs to AEAD_CHACHA20_POLY1305 are:
//   A 256-bit key
//   A 96-bit nonce (or bigger 192 bit nonce) -- different for each invocation with the same key
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
// nonce_size is the size of the standard nonce (in bytes) which the Chacha20Poly1305 AEAD accepts.
pub const nonce_size = 12
// nonce_size is the size of the extended nonce (in bytes) which the Chacha20Poly1305 AEAD accepts.
pub const x_nonce_size = 24
// tag_size is the size of the message authenticated code (in bytes) produced by Chacha20Poly1305 AEAD.
pub const tag_size = 16

// encrypt does one-shot encryption of given plaintext with associated key, nonce and additional data.
// It return ciphertext output and authenticated tag appended into it.
pub fn encrypt(plaintext []u8, key []u8, nonce []u8, ad []u8) ![]u8 {
	mut c := new(key, nonce.len)!
	out := c.encrypt(plaintext, nonce, ad)!
	return out
}

// decrypt does one-shot decryption of given ciphertext with associated key, nonce and additional data.
// It return plaintext output and verify if resulting tag is a valid message authenticated code (mac)
// for given message, key and additional data.
pub fn decrypt(ciphertext []u8, key []u8, nonce []u8, ad []u8) ![]u8 {
	mut c := new(key, nonce.len)!
	out := c.decrypt(ciphertext, nonce, ad)!
	return out
}

// Chacha20Poly1305 represents AEAD algorithm backed by `x.crypto.chacha20` and `x.crypto.poly1305`.
struct Chacha20Poly1305 {
	key    []u8 = []u8{len: key_size}
	ncsize int  = nonce_size
}

// new creates a new Chacha20Poly1305 AEAD instance with given 32 bytes of key
// and the nonce size in ncsize. The ncsize should be 12 or 24 length, otherwise it would return error.
pub fn new(key []u8, ncsize int) !&AEAD {
	if key.len != key_size {
		return error('chacha20poly1305: bad key size')
	}
	if ncsize != nonce_size && ncsize != x_nonce_size {
		return error('chacha20poly1305: bad nonce size supplied, its should 12 or 24')
	}
	c := &Chacha20Poly1305{
		key:    key
		ncsize: ncsize
	}
	return c
}

// nonce_size returns the size of underlying nonce (in bytes) of AEAD algorithm.
pub fn (c Chacha20Poly1305) nonce_size() int {
	return c.ncsize
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
	return c.encrypt_generic(plaintext, nonce, ad)
}

// encrypt_generic encrypts plaintext along with nonce and additional data
fn (c Chacha20Poly1305) encrypt_generic(plaintext []u8, nonce []u8, ad []u8) ![]u8 {
	// First, generates a Poly1305 one-time key from the 256-bit key
	// and given nonce. Actually its generates by performing ChaCha20 key stream function,
	// and take the first 32 bytes as a one-time key for Poly1305 from 64 bytes results.
	// see https://datatracker.ietf.org/doc/html/rfc8439#section-2.6
	mut polykey := []u8{len: key_size}
	mut s := chacha20.new_cipher(c.key, nonce)!
	s.encrypt(mut polykey, polykey)

	// Next, the ChaCha20 encryption function is called to encrypt the plaintext,
	// using the same key and nonce, and with the initial ChaCha20 counter set to 1.
	mut ciphertext := []u8{len: plaintext.len}
	s.set_counter(1)
	s.encrypt(mut ciphertext, plaintext)

	// Finally, the Poly1305 function is called with the generated Poly1305 one-time key
	// calculated above, and a message constructed as described in
	// https://datatracker.ietf.org/doc/html/rfc8439#section-2.8
	mut constructed_msg := []u8{}
	poly1305_construct_msg(mut constructed_msg, ad, ciphertext)

	// Lets creates Poly1305 instance with one-time key generates in above step,
	// updates Poly1305 state with this constructed_msg and finally generates tag.
	mut tag := []u8{len: tag_size}
	mut po := poly1305.new(polykey)!
	po.update(constructed_msg)
	po.finish(mut tag)

	// add this tag to ciphertext output
	ciphertext << tag

	return ciphertext
}

// decrypt decrypts ciphertext along with provided nonce and additional data.
// Decryption is similar with the encryption process with slight differences in:
// The roles of ciphertext and plaintext are reversed, so the ChaCha20 encryption
// function is applied to the ciphertext, producing the plaintext.
// The Poly1305 function is still run on the AAD and the ciphertext, not the plaintext.
// The calculated mac is bitwise compared to the received mac.
// The message is authenticated if and only if the tags match, return error if failed to verify.
pub fn (c Chacha20Poly1305) decrypt(ciphertext []u8, nonce []u8, ad []u8) ![]u8 {
	if nonce.len != c.nonce_size() {
		return error('chacha20poly1305: unmatching nonce size')
	}
	// ciphertext max = plaintext max length  + tag length
	// ie, (2^32-1)*64 + overhead = (u64(1) << 38) - 64 + 16 = 274,877,906,896 octets.
	if u64(ciphertext.len) > (u64(1) << 38) - 48 {
		return error('chacha20poly1305: ciphertext too large')
	}
	return c.decrypt_generic(ciphertext, nonce, ad)
}

fn (c Chacha20Poly1305) decrypt_generic(ciphertext []u8, nonce []u8, ad []u8) ![]u8 {
	// generates poly1305 one-time key for later calculation
	mut polykey := []u8{len: key_size}
	mut s := chacha20.new_cipher(c.key, nonce)!
	s.encrypt(mut polykey, polykey)

	// Remember, ciphertext is concatenation of associated cipher output plus tag (mac) bytes
	encrypted := ciphertext[0..ciphertext.len - c.overhead()]
	mac := ciphertext[ciphertext.len - c.overhead()..]

	mut plaintext := []u8{len: encrypted.len}
	s.set_counter(1)
	// doing reverse encrypt on cipher output part produces plaintext
	s.encrypt(mut plaintext, encrypted)

	// authenticated messages part
	mut constructed_msg := []u8{}
	poly1305_construct_msg(mut constructed_msg, ad, encrypted)

	mut tag := []u8{len: tag_size}
	mut po := poly1305.new(polykey)!
	po.update(constructed_msg)
	po.finish(mut tag)

	// lets verify if received mac is matching with calculated tag,
	// return error on fail and free allocated resource.
	if subtle.constant_time_compare(mac, tag) != 1 {
		// free allocated resource
		unsafe {
			s.free()
			tag.free()
			polykey.free()
			plaintext.free()
		}
		return error('chacha20poly1305: unmatching tag')
	}

	return plaintext
}

// Helper function

// pad x to 16 bytes block
fn pad_to_16(x []u8) []u8 {
	mut buf := x.clone()
	if buf.len % 16 == 0 {
		return buf
	}
	pad_bytes := []u8{len: 16 - buf.len % 16, init: 0}
	buf << pad_bytes
	return buf
}

// poly1305_construct_msg constructs poly1305 message for later usage and stores to out.
// The message constructed as a concatenation of the following:
// 	*  padded to multiple of 16 bytes block of the additional data bytes
// 	*  padded to multiple of 16 bytes block of the ciphertext (or plaintext) bytes
// 	*  The length of the additional data in octets (as a 64-bit little-endian integer).
// 	*  The length of the ciphertext (or plaintext) in octets (as a 64-bit little-endian integer).
fn poly1305_construct_msg(mut out []u8, ad []u8, bytes []u8) {
	mut b8 := []u8{len: 8}
	out << pad_to_16(ad)
	out << pad_to_16(bytes)
	binary.little_endian_put_u64(mut b8, u64(ad.len))
	out << b8
	binary.little_endian_put_u64(mut b8, u64(bytes.len))
	out << b8
}

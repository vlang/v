// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Surface of aead128.v that aead128_test.v does not reach: the Aead128.nonce_size
// and Aead128.overhead accessors, the ciphertext length implied by them, and the
// rejection paths of both the one-shot and the object based entry points.
//
// The tamper expectations are behavioural rather than vector based: for every
// single byte position of a known ciphertext, flipping it must make decryption
// fail. That is the property an AEAD has to have, and it is cheaper to state
// than a table of one vector per position.
module ascon

import encoding.hex

// kat_key is the key from the LWC_AEAD_KAT_128_128 vector in aead128_test.v,
// reused here so the tamper checks run against a ciphertext that is already
// known to be correct.
fn kat_key() ![]u8 {
	return hex.decode('000102030405060708090a0b0c0d0e0f')!
}

fn kat_nonce() ![]u8 {
	return hex.decode('101112131415161718191a1b1c1d1e1f')!
}

fn test_aead128_nonce_size_and_overhead() {
	key := kat_key()!
	mut c := new_aead128(key)!

	// The accessors have to agree with the constants they are named after,
	// otherwise a caller sizing buffers from them would be wrong.
	assert c.nonce_size() == nonce_size
	assert c.nonce_size() == 16
	assert c.overhead() == tag_size
	assert c.overhead() == 16

	// The accessors do not depend on the key material, only on the scheme.
	mut other := new_aead128(hex.decode('ffffffffffffffffffffffffffffffff')!)!
	assert other.nonce_size() == c.nonce_size()
	assert other.overhead() == c.overhead()
}

fn test_aead128_module_constants_are_128_bit() {
	assert key_size == 16
	assert nonce_size == 16
	assert tag_size == 16
}

fn test_aead128_rejects_wrong_key_size() {
	nonce := kat_nonce()!
	msg := 'ascon'.bytes()
	ad := []u8{}

	for n in [0, 15, 17, 32] {
		encrypt([]u8{len: n}, nonce, ad, msg) or {
			assert err.msg() == 'encrypt: invalid key size', 'key length ${n}'
			continue
		}
		assert false, 'one-shot encrypt accepted a ${n}-byte key'
	}

	new_aead128([]u8{len: 15}) or {
		assert err.msg() == 'invalid cipher key size'
		return
	}
	assert false, 'new_aead128 accepted a 15-byte key'
}

fn test_aead128_rejects_wrong_nonce_size() {
	key := kat_key()!
	msg := 'ascon'.bytes()
	ad := []u8{}

	for n in [0, 15, 17] {
		encrypt(key, []u8{len: n}, ad, msg) or {
			assert err.msg() == 'encrypt: invalid nonce size', 'nonce length ${n}'
			continue
		}
		assert false, 'one-shot encrypt accepted a ${n}-byte nonce'
	}

	mut c := new_aead128(key) or { panic(err) }
	c.encrypt(msg, []u8{len: 15}, ad) or {
		assert err.msg() == 'encrypt: invalid nonce size'
		return
	}
	assert false, 'Aead128.encrypt accepted a 15-byte nonce'
}

fn test_aead128_ciphertext_length_is_message_plus_overhead() {
	key := kat_key()!
	nonce := kat_nonce()!
	mut c := new_aead128(key)!

	// Lengths crossing the 16-byte rate boundary and the 8-byte sub-block path,
	// so every branch of the message processing loop is exercised.
	for n in 0 .. 40 {
		msg := []u8{len: n, init: u8(n)}
		ad := []u8{len: n % 5, init: u8(0xa5)}

		oneshot := encrypt(key, nonce, ad, msg)!
		assert oneshot.len == msg.len + c.overhead(), 'message length ${n}'

		streaming := c.encrypt(msg, nonce, ad)!
		assert streaming.len == msg.len + c.overhead(), 'message length ${n}'
		assert streaming == oneshot, 'one-shot and Aead128 disagree at length ${n}'
	}
}

fn test_aead128_rejects_ciphertext_shorter_than_tag() {
	key := kat_key()!
	nonce := kat_nonce()!
	msg := 'ascon'.bytes()
	ct := encrypt(key, nonce, []u8{}, msg)!

	for n in 0 .. ct.len - tag_size {
		short := ct[..n]
		decrypt(key, nonce, []u8{}, short) or {
			assert err.msg() == 'decrypt: invalid ciphertext size', 'ciphertext length ${n}'
			continue
		}
		assert false, 'one-shot decrypt accepted a ${n}-byte ciphertext'
	}

	mut c := new_aead128(key) or { panic(err) }
	for n in 0 .. ct.len - tag_size {
		c.decrypt(ct[..n], nonce, []) or {
			assert err.msg() == 'bad ciphertext size', 'ciphertext length ${n}'
			continue
		}
		assert false, 'Aead128.decrypt accepted a ${n}-byte ciphertext'
	}
}

fn test_aead128_every_ciphertext_byte_is_authenticated() {
	key := kat_key()!
	nonce := kat_nonce()!
	msg := 'the quick brown fox'.bytes()
	ad := 'associated data'.bytes()
	ct := encrypt(key, nonce, ad, msg)!

	// A single flipped bit anywhere must fail the tag check, in the ciphertext
	// body as well as in the appended tag itself.
	for i in 0 .. ct.len {
		mut bad := ct.clone()
		bad[i] ^= u8(0x01)

		decrypt(key, nonce, ad, bad) or {
			assert err.msg() == 'decrypt: unmatching tag', 'byte ${i}'
			continue
		}
		assert false, 'one-shot decrypt accepted a flip at byte ${i}'
	}
}

fn test_aead128_object_rejects_every_ciphertext_byte() {
	key := kat_key()!
	nonce := kat_nonce()!
	msg := 'the quick brown fox'.bytes()
	ad := 'associated data'.bytes()
	ct := encrypt(key, nonce, ad, msg)!
	mut c := new_aead128(key) or { panic(err) }

	for i in 0 .. ct.len {
		mut bad := ct.clone()
		bad[i] ^= u8(0x80)

		c.decrypt(bad, nonce, ad) or {
			assert err.msg() == 'Aead128.decrypt: unmatching tag', 'byte ${i}'
			continue
		}
		assert false, 'Aead128.decrypt accepted a flip at byte ${i}'
	}
}

fn test_aead128_rejects_wrong_associated_data() {
	key := kat_key()!
	nonce := kat_nonce()!
	msg := 'ascon associated data'.bytes()
	ct := encrypt(key, nonce, 'original ad'.bytes(), msg)!

	// Associated data is covered by the tag, so a mismatch has to fail.
	for other in ['original ae', 'original AD', '', 'original ad plus more'] {
		decrypt(key, nonce, other.bytes(), ct) or {
			assert err.msg() == 'decrypt: unmatching tag', 'ad "${other}"'
			continue
		}
		assert false, 'one-shot decrypt accepted ad "${other}"'
	}

	mut c := new_aead128(key) or { panic(err) }
	c.decrypt(ct, nonce, 'wrong ad'.bytes()) or {
		assert err.msg() == 'Aead128.decrypt: unmatching tag'
		return
	}
	assert false, 'Aead128.decrypt accepted wrong associated data'
}

fn test_aead128_rejects_wrong_key() {
	key := kat_key()!
	nonce := kat_nonce()!
	msg := 'ascon key check'.bytes()
	ct := encrypt(key, nonce, []u8{}, msg)!

	wrong := hex.decode('0f0e0d0c0b0a09080706050403020100')!
	decrypt(wrong, nonce, []u8{}, ct) or {
		assert err.msg() == 'decrypt: unmatching tag'
		return
	}
	assert false, 'one-shot decrypt accepted a different key'
}

// NOTE: the same Aead128 instance is reused across encrypt calls. The existing
// aead128_test.v does the same inside one loop, but never asserts that a second
// call on the same instance produces the same ciphertext as a fresh instance,
// which is what a stateless AEAD interface promises.
fn test_aead128_instance_is_reusable() {
	key := kat_key()!
	nonce := kat_nonce()!
	msg := 'reuse me'.bytes()
	ad := 'ad'.bytes()

	mut c := new_aead128(key) or { panic(err) }
	first := c.encrypt(msg, nonce, ad)!
	for _ in 0 .. 4 {
		again := c.encrypt(msg, nonce, ad)!
		assert again == first
		assert c.decrypt(again, nonce, ad)! == msg
	}
	assert first == encrypt(key, nonce, ad, msg)!
}

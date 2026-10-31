import encoding.hex
import x.crypto.chacha20poly1305

// Coverage for the accessors and the parameter validation of
// chacha20poly1305, which chacha20poly1305_test.v, usage_test.v and
// xchacha20poly1305_test.v do not reach.
//
// nonce_size() on the object is the only untested public accessor, but it is
// checked against the constants it is derived from for all three nonce sizes the
// AEAD accepts, because a mismatch at only one of them would be invisible
// otherwise. The one-shot and object entry points are also cross-checked, and the
// rejection paths that return errors rather than panicking are asserted against
// their exact messages.

// rfc8439_key is the AEAD key from the RFC 8439 section 2.8.2 test vector, which
// usage_test.v also uses.
fn rfc8439_key() ![]u8 {
	return hex.decode('1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0')!
}

// rfc8439_non12 is that vector's 96-bit nonce.
fn rfc8439_nonce12() ![]u8 {
	return hex.decode('000000000102030405060708')!
}

fn rfc8439_msg() []u8 {
	// 43 bytes, so a single ChaCha20 block plus a partial one.
	return 'In the end, we remember only the struggle'.bytes()
}

fn test_chacha20poly1305_nonce_size_matches_the_constant() {
	key := rfc8439_key()!

	// Each accepted nonce size gets its own instance, and its nonce_size()
	// accessor has to report what the instance was built with.
	for n in [chacha20poly1305.nonce_size, chacha20poly1305.x_nonce_size,
		chacha20poly1305.orig_nonce_size] {
		c := chacha20poly1305.new(key, n)!
		assert c.nonce_size() == n, 'instance built for ${n}-byte nonces'
		assert c.overhead() == chacha20poly1305.tag_size
		assert c.overhead() == 16
	}
}

fn test_chacha20poly1305_rejects_unaccepted_nonce_size() {
	key := rfc8439_key()!

	// The constructor validates nsize, not the nonce itself: 9 and 16 bytes are
	// not sizes it accepts.
	for n in [0, 11, 13, 16, 32] {
		chacha20poly1305.new(key, n) or {
			assert err.msg() == 'chacha20poly1305: bad nonce size supplied, its should 8, 12 or 24'
			continue
		}
		assert false, 'new() accepted a declared nonce size of ${n}'
	}
}

fn test_chacha20poly1305_rejects_wrong_key_size() {
	for n in [chacha20poly1305.nonce_size, chacha20poly1305.x_nonce_size,
		chacha20poly1305.orig_nonce_size] {
		bad := []u8{len: 31}
		chacha20poly1305.new(bad, n) or {
			assert err.msg() == 'chacha20poly1305: bad key size'
			continue
		}
		assert false, 'new() accepted a 31-byte key'
	}
}

// The ciphertext is the plaintext plus the tag, and that holds for every accepted
// nonce size, including the two the existing tests do not use together.
fn test_chacha20poly1305_ciphertext_length_is_msg_plus_tag() ! {
	key := rfc8439_key()!
	msg := rfc8439_msg()
	ad := 'some ad'.bytes()

	for n in [chacha20poly1305.nonce_size, chacha20poly1305.x_nonce_size,
		chacha20poly1305.orig_nonce_size] {
		nonce := []u8{len: n, init: u8(0x42)}

		c := chacha20poly1305.new(key, n)!
		assert c.nonce_size() == n

		ct := c.encrypt(msg, nonce, ad)!
		assert ct.len == msg.len + c.overhead(), 'nonce size ${n}'
		assert ct.len == msg.len + chacha20poly1305.tag_size, 'nonce size ${n}'
		assert c.decrypt(ct, nonce, ad)! == msg, 'nonce size ${n}'

		// The one-shot path agrees with the instance path.
		oneshot := chacha20poly1305.encrypt(msg, key, nonce, ad)!
		assert oneshot.len == ct.len, 'nonce size ${n}'
		assert chacha20poly1305.decrypt(oneshot, key, nonce, ad)! == msg, 'nonce size ${n}'
	}
}

// The object API checks the nonce length against what it was constructed with,
// so a nonce of the right size for the scheme but the wrong size for this
// instance is rejected rather than silently used.
fn test_chacha20poly1305_rejects_wrong_nonce_length_for_instance() ! {
	key := rfc8439_key()!
	msg := rfc8439_msg()
	ad := []u8{}

	c := chacha20poly1305.new(key, chacha20poly1305.nonce_size)!
	for n in [0, 8, 24] {
		c.encrypt(msg, []u8{len: n}, ad) or {
			assert err.msg() == 'chacha20poly1305: unmatching nonce size', 'nonce length ${n}'
			continue
		}
		assert false, 'encrypt accepted a ${n}-byte nonce on a 12-byte instance'
	}

	// A ciphertext shorter than the tag fails its own check before the nonce is
	// even looked at.
	short := []u8{len: chacha20poly1305.tag_size - 1}
	c.decrypt(short, rfc8439_nonce12()!, ad) or {
		assert err.msg() == 'chacha20poly1305: ciphertext length does not meet minimum required length'
		return
	}
	assert false, 'decrypt accepted a 15-byte ciphertext'
}

// Tampering with any byte of the ciphertext body or of the appended tag has to
// fail the Poly1305 tag check rather than return a plausible message.
fn test_chacha20poly1305_rejects_any_flipped_byte() ! {
	key := rfc8439_key()!
	nonce := rfc8439_nonce12()!
	msg := rfc8439_msg()
	ad := 'some ad'.bytes()
	ct := chacha20poly1305.encrypt(msg, key, nonce, ad)!

	for i in 0 .. ct.len {
		mut bad := ct.clone()
		bad[i] ^= u8(0x01)
		chacha20poly1305.decrypt(bad, key, nonce, ad) or {
			assert err.msg() == 'chacha20poly1305: unmatching tag', 'byte ${i}'
			continue
		}
		assert false, 'decrypt accepted a flip at byte ${i}'
	}
}

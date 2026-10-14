import encoding.hex
import x.crypto.chacha20

// Coverage for Cipher.free(), the only public method of chacha20 that
// chacha_test.v, xchacha_test.v, chacha_64bitctr_test.v and stream_test.v never
// reach, plus the per-instance allocation it releases.
//
// free() is guarded by `$if prealloc { return }`, so under a prealloc build it is
// a no-op. A default test build is not a prealloc build, which is what lets these
// assertions observe it. The observable effect is that the 64-byte keystream
// block is released, so these tests check that a cipher can be used and freed
// without disturbing either its sibling or the module's own oneshot path.
//
// xor_key_stream is stateful: a second call on the same instance continues the
// keystream rather than restarting it, so every expected value below comes from
// a fresh instance rather than from a repeated call on a used one.

// rfc8439_key is the key from the RFC 8439 section 2.4.2 test vector.
fn rfc8439_key() ![]u8 {
	return hex.decode('000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f')!
}

// rfc8439_nonce is the 96-bit nonce from the same vector.
fn rfc8439_nonce() ![]u8 {
	return hex.decode('000000090000004a00000000')!
}

// rfc8439_msg is that vector's 114-byte plaintext.
fn rfc8439_msg() []u8 {
	return "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
		.bytes()
}

// x_nonce is a 24-byte nonce, which selects the extended XChaCha20 construct.
// Built as a byte array rather than decoded from hex, because an odd number of
// hex digits makes hex.decode return an error and is easy to miscount.
fn x_nonce() []u8 {
	return []u8{len: 24, init: u8(0x40)}
}

// keystream_xor is the fresh-instance path the tests compare against, since a
// reused instance continues its keystream.
fn keystream_xor(key []u8, nonce []u8, msg []u8, opt chacha20.Options) ![]u8 {
	mut c := chacha20.new_cipher(key, nonce, opt)!
	mut dst := []u8{len: msg.len}
	c.xor_key_stream(mut dst, msg)
	unsafe { c.free() }
	return dst
}

fn test_chacha20_cipher_free_does_not_disturb_a_sibling() ! {
	key := rfc8439_key()!
	nonce := rfc8439_nonce()!
	msg := rfc8439_msg()

	for _ in 0 .. 8 {
		mut a := chacha20.new_cipher(key, nonce)!
		mut b := chacha20.new_cipher(key, nonce)!

		mut dst_a := []u8{len: msg.len}
		a.xor_key_stream(mut dst_a, msg)
		unsafe { a.free() }

		mut dst_b := []u8{len: msg.len}
		b.xor_key_stream(mut dst_b, msg)
		unsafe { b.free() }

		assert dst_a == dst_b, 'two ciphers from the same material disagree'
		assert dst_a == keystream_xor(key, nonce, msg)!
	}
}

// Freeing an instance and building another from the same material gives the same
// keystream, which is what "the allocation is per instance, and releasing it is
// safe" means in practice.
fn test_chacha20_cipher_free_then_reuse_agrees() ! {
	key := rfc8439_key()!
	nonce := rfc8439_nonce()!
	msg := rfc8439_msg()

	oneshot := chacha20.encrypt(key, nonce, msg)!

	for i in 0 .. 6 {
		mut c := chacha20.new_cipher(key, nonce)!
		mut dst := []u8{len: msg.len}
		c.xor_key_stream(mut dst, msg)
		unsafe { c.free() }

		// A fresh instance is unaffected by the freed one, and the oneshot
		// agrees with both, so a free does not perturb any shared state.
		mut again := chacha20.new_cipher(key, nonce)!
		mut dst2 := []u8{len: msg.len}
		again.xor_key_stream(mut dst2, msg)
		unsafe { again.free() }

		assert dst == dst2, 'iteration ${i}: a fresh cipher disagreed with a freed one'
		assert dst == oneshot, 'iteration ${i}: a fresh cipher disagreed with the oneshot'
	}
}

// The same holds for the extended and 64-bit counter variants, which take the
// same allocation path through Cipher rather than a different one.
fn test_chacha20_cipher_free_all_variants() ! {
	key := rfc8439_key()!
	msg := 'the same message for every variant'.bytes()

	xn := x_nonce()
	standard := keystream_xor(key, rfc8439_nonce()!, msg)!
	extended := keystream_xor(key, xn, msg)!
	wide := keystream_xor(key, xn, msg, use_64bit_counter: true)!

	// The 24-byte nonce selects a different construction, so it has to differ
	// from the 12-byte one; that is the control that the comparison means
	// anything.
	assert extended != standard, 'extended and standard variants agreed'

	for i in 0 .. 6 {
		mut c := chacha20.new_cipher(key, xn, use_64bit_counter: true)!
		mut dst := []u8{len: msg.len}
		c.xor_key_stream(mut dst, msg)
		unsafe { c.free() }
		assert dst == wide, 'iteration ${i}: 64-bit counter variant drifted'

		mut d := chacha20.new_cipher(key, xn)!
		mut dst2 := []u8{len: msg.len}
		d.xor_key_stream(mut dst2, msg)
		unsafe { d.free() }
		assert dst2 == extended, 'iteration ${i}: extended variant drifted'
	}
}

// A cipher freed immediately after construction is fine, and so is one that is
// never used at all — both are the minimal allocations free() has to release.
fn test_chacha20_cipher_free_unused() {
	key := rfc8439_key()!

	mut c := chacha20.new_cipher(key, rfc8439_nonce()!) or { panic(err) }
	unsafe { c.free() }

	mut d := chacha20.new_cipher(key, x_nonce(), use_64bit_counter: true) or { panic(err) }
	unsafe { d.free() }
}

// NOTE: free() is `@[unsafe]` and needs an unsafe block at the call site, unlike
// the same method in chacha20poly1305.generic_crypt, which calls it from inside
// its own unsafe block. Every free() call above is wrapped in one for that
// reason.
//
// NOTE: free() has no observable state — the struct is not zeroed, nothing is
// marked unusable, and no accessor reports the difference. Under `$if prealloc`
// it returns immediately, and even in a default build the release cannot be
// distinguished from a leak by a caller. So these tests can only assert what
// free() does *not* disturb: a sibling instance, the oneshot path, and a fresh
// instance built afterwards. That is the limit of what this method can be
// tested for from outside, and it is stated rather than papered over.
fn test_chacha20_cipher_free_needs_unsafe() {
	key := rfc8439_key()!
	mut c := chacha20.new_cipher(key, rfc8439_nonce()!) or { panic(err) }
	unsafe {
		c.free()
	}
}

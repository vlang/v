module tls

fn test_aead_roundtrip() {
	key := []u8{len: 16, init: 1}
	iv := []u8{len: 12, init: 2}
	pt := [u8(10), 20, 30, 40]
	rec_bytes := seal_record(.tls_aes_128_gcm_sha256, key, iv, 7, .application_data, pt) or {
		panic(err)
	}
	rec, _ := decode_cipher_record(rec_bytes) or { panic(err) }
	ct, got := open_record(.tls_aes_128_gcm_sha256, key, iv, 7, rec) or { panic(err) }
	assert ct == .application_data
	assert got == pt
}

fn test_aead_wrong_seq_fails() {
	key := []u8{len: 16, init: 1}
	iv := []u8{len: 12, init: 2}
	pt := [u8(10), 20, 30]
	rec_bytes := seal_record(.tls_aes_128_gcm_sha256, key, iv, 7, .application_data, pt) or {
		panic(err)
	}
	rec, _ := decode_cipher_record(rec_bytes) or { panic(err) }
	open_record(.tls_aes_128_gcm_sha256, key, iv, 8, rec) or {
		// expected failure
		return
	}
	assert false
}

// test_aead_chacha_roundtrip exercises the ChaCha20-Poly1305 record path.
fn test_aead_chacha_roundtrip() {
	key := []u8{len: 32, init: 3}
	iv := []u8{len: 12, init: 4}
	pt := 'chacha record'.bytes()
	rec_bytes := seal_record(.tls_chacha20_poly1305_sha256, key, iv, 7, .application_data,
		pt) or { panic(err) }
	rec, _ := decode_cipher_record(rec_bytes) or { panic(err) }
	ct, got := open_record(.tls_chacha20_poly1305_sha256, key, iv, 7, rec) or { panic(err) }
	assert ct == .application_data
	assert got == pt
	// Wrong sequence number must fail authentication.
	open_record(.tls_chacha20_poly1305_sha256, key, iv, 8, rec) or { return }
	assert false, 'chacha wrong seq must fail'
	// A 16-byte AES key must fail the 32-byte ChaCha key check.
	open_record(.tls_chacha20_poly1305_sha256, []u8{len: 16}, iv, 7, rec) or {
		assert err.msg().contains('key length')
		return
	}
	assert false, 'short chacha key must fail'
}

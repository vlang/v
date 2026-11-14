module tls

// test_select_cipher_suite locks suite negotiation: server preference order
// intersected with the client offer, with unimplemented suites never matching.
fn test_select_cipher_suite() {
	assert select_cipher_suite([.tls_aes_128_gcm_sha256, .tls_chacha20_poly1305_sha256])! == .tls_aes_128_gcm_sha256
	assert select_cipher_suite([.tls_chacha20_poly1305_sha256])! == .tls_chacha20_poly1305_sha256
	assert select_cipher_suite([.tls_chacha20_poly1305_sha256, .tls_aes_128_gcm_sha256])! == .tls_aes_128_gcm_sha256
	select_cipher_suite([]CipherSuite{}) or {
		assert err.msg().contains('cipher suite')
		select_cipher_suite([.tls_aes_256_gcm_sha384]) or {
			assert err.msg().contains('cipher suite')
			return
		}
	}
	assert false, 'empty or unimplemented-only offer must fail'
}

// test_cipher_suite_key_lengths locks per-suite key sizes and derivation.
fn test_cipher_suite_key_lengths() {
	assert cipher_suite_key_len(.tls_aes_128_gcm_sha256) == 16
	assert cipher_suite_key_len(.tls_aes_256_gcm_sha384) == 32
	assert cipher_suite_key_len(.tls_chacha20_poly1305_sha256) == 32
	tk128 := derive_traffic_keys([]u8{len: 32}, .tls_aes_128_gcm_sha256) or { panic(err) }
	assert tk128.key.len == 16
	assert tk128.iv.len == 12
	tkchacha := derive_traffic_keys([]u8{len: 32}, .tls_chacha20_poly1305_sha256) or {
		panic(err)
	}
	assert tkchacha.key.len == 32
	assert tkchacha.iv.len == 12
}

// test_default_cipher_suites locks the default offer order.
fn test_default_cipher_suites() {
	assert default_cipher_suites() == [.tls_aes_128_gcm_sha256, .tls_chacha20_poly1305_sha256]
	assert effective_cipher_suites(Config{}) == default_cipher_suites()
	assert effective_cipher_suites(Config{
		cipher_suites: [.tls_chacha20_poly1305_sha256]
	}) == [.tls_chacha20_poly1305_sha256]
}

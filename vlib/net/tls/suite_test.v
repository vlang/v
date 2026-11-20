module tls

// test_select_cipher_suite locks suite negotiation: server preference order
// intersected with the client offer. All three known suites are implemented.
fn test_select_cipher_suite() {
	assert select_cipher_suite([.tls_aes_128_gcm_sha256, .tls_chacha20_poly1305_sha256])! == .tls_aes_128_gcm_sha256
	assert select_cipher_suite([.tls_chacha20_poly1305_sha256])! == .tls_chacha20_poly1305_sha256
	assert select_cipher_suite([.tls_chacha20_poly1305_sha256, .tls_aes_128_gcm_sha256])! == .tls_aes_128_gcm_sha256
	assert select_cipher_suite([.tls_aes_256_gcm_sha384])! == .tls_aes_256_gcm_sha384
	assert select_cipher_suite([.tls_chacha20_poly1305_sha256, .tls_aes_256_gcm_sha384])! == .tls_chacha20_poly1305_sha256
	select_cipher_suite([]CipherSuite{}) or {
		assert err.msg().contains('cipher suite')
		return
	}
	assert false, 'empty offer must fail'
}

// test_cipher_suite_key_lengths locks per-suite key sizes and derivation.
fn test_cipher_suite_key_lengths() {
	assert cipher_suite_key_len(.tls_aes_128_gcm_sha256) == 16
	assert cipher_suite_key_len(.tls_aes_256_gcm_sha384) == 32
	assert cipher_suite_key_len(.tls_chacha20_poly1305_sha256) == 32
	assert cipher_suite_hash_len(.tls_aes_128_gcm_sha256) == 32
	assert cipher_suite_hash_len(.tls_aes_256_gcm_sha384) == 48
	assert cipher_suite_hash_len(.tls_chacha20_poly1305_sha256) == 32
	tk128 := derive_traffic_keys([]u8{len: 32}, .tls_aes_128_gcm_sha256) or { panic(err) }
	assert tk128.key.len == 16
	assert tk128.iv.len == 12
	tkchacha := derive_traffic_keys([]u8{len: 32}, .tls_chacha20_poly1305_sha256) or {
		panic(err)
	}
	assert tkchacha.key.len == 32
	assert tkchacha.iv.len == 12
	tk256 := derive_traffic_keys([]u8{len: 48}, .tls_aes_256_gcm_sha384) or { panic(err) }
	assert tk256.key.len == 32
	assert tk256.iv.len == 12
	// SHA-384 schedule produces 48-byte secrets.
	es384 := derive_early_secret_for_suite(.tls_aes_256_gcm_sha384) or { panic(err) }
	assert es384.len == 48
	hs384 := derive_handshake_secrets_for_suite(es384, []u8{len: 32, init: 1}, transcript_hash_for_suite([]u8{len: 4},
		.tls_aes_256_gcm_sha384), .tls_aes_256_gcm_sha384) or { panic(err) }
	assert hs384.client_secret.len == 48
	assert hs384.server_secret.len == 48
	vd384 := finished_verify_data_for_suite(hs384.server_secret, transcript_hash_for_suite('abc'.bytes(),
		.tls_aes_256_gcm_sha384), .tls_aes_256_gcm_sha384) or { panic(err) }
	assert vd384.len == 48
}

// test_default_cipher_suites locks the default offer order.
fn test_default_cipher_suites() {
	assert default_cipher_suites() == [.tls_aes_128_gcm_sha256, .tls_chacha20_poly1305_sha256,
		.tls_aes_256_gcm_sha384]
	assert effective_cipher_suites(Config{}) == default_cipher_suites()
	assert effective_cipher_suites(Config{
		cipher_suites: [.tls_chacha20_poly1305_sha256]
	}) == [.tls_chacha20_poly1305_sha256]
}

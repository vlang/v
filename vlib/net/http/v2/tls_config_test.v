module v2

// Tests for RFC 7540 §9.2.2 cipher blacklist validation.

fn test_forbidden_cipher_rsa_with_prefix() {
	// TLS_RSA_WITH_* ciphers are non-PFS and forbidden per RFC 7540 Appendix A.
	assert is_forbidden_cipher('TLS_RSA_WITH_AES_128_CBC_SHA') == true
}

fn test_forbidden_cipher_null() {
	// TLS_NULL_* ciphers provide no encryption and are forbidden.
	assert is_forbidden_cipher('TLS_NULL_WITH_NULL_NULL') == true
}

fn test_allowed_cipher_ecdhe_rsa() {
	// ECDHE_RSA with AEAD is a modern cipher and should be allowed.
	assert is_forbidden_cipher('TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256') == false
}

fn test_allowed_cipher_tls13() {
	// TLS 1.3 ciphers should be allowed.
	assert is_forbidden_cipher('TLS_AES_128_GCM_SHA256') == false
}

fn test_forbidden_cipher_case_insensitive() {
	// Cipher names may arrive in mixed case; detection must be case-insensitive.
	assert is_forbidden_cipher('tls_rsa_with_aes_128_cbc_sha') == true
	assert is_forbidden_cipher('Tls_Null_With_Null_Null') == true
}

fn test_forbidden_cipher_export() {
	// EXPORT ciphers are weak and forbidden.
	assert is_forbidden_cipher('TLS_RSA_EXPORT_WITH_RC4_40_MD5') == true
}

fn test_forbidden_cipher_des() {
	// DES ciphers are weak and forbidden.
	assert is_forbidden_cipher('TLS_DES_CBC_SHA') == true
}

fn test_forbidden_cipher_3des() {
	// 3DES ciphers are weak and forbidden.
	assert is_forbidden_cipher('TLS_3DES_EDE_CBC_SHA') == true
}

fn test_forbidden_cipher_rc4() {
	// RC4 ciphers are forbidden.
	assert is_forbidden_cipher('TLS_RC4_128_SHA') == true
}

fn test_forbidden_cipher_null_substring() {
	// NULL appearing as substring (not prefix) should also be caught.
	assert is_forbidden_cipher('TLS_ECDH_anon_WITH_NULL_SHA') == true
}

fn test_forbidden_cipher_rc4_substring() {
	// RC4 appearing as substring should be caught.
	assert is_forbidden_cipher('TLS_ECDHE_RSA_WITH_RC4_128_SHA') == true
}

fn test_allowed_cipher_chacha20() {
	// ChaCha20 is modern and should be allowed.
	assert is_forbidden_cipher('TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256') == false
}

fn test_forbidden_ciphers_list_not_empty() {
	// The constant must contain actual prefixes.
	assert http2_forbidden_ciphers.len > 0
}

fn test_allowed_cipher_empty_string() {
	// Empty string should not match any forbidden cipher.
	assert is_forbidden_cipher('') == false
}

fn test_client_config_accepts_tls_fields() {
	// ClientConfig must accept TLS settings so callers can forward
	// verify/cert/cert_key/validate/in_memory_verification to the SSL layer.
	config := ClientConfig{
		verify:                 '/path/to/ca.pem'
		cert:                   '/path/to/cert.pem'
		cert_key:               '/path/to/key.pem'
		validate:               true
		in_memory_verification: false
	}
	assert config.verify == '/path/to/ca.pem'
	assert config.cert == '/path/to/cert.pem'
	assert config.cert_key == '/path/to/key.pem'
	assert config.validate == true
	assert config.in_memory_verification == false
}

fn test_client_config_tls_fields_default_empty() {
	// Default ClientConfig should have empty/false TLS fields.
	config := ClientConfig{}
	assert config.verify == ''
	assert config.cert == ''
	assert config.cert_key == ''
	assert config.validate == false
	assert config.in_memory_verification == false
}

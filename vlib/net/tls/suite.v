module tls

// Cipher-suite negotiation (RFC 8446 §4.2.1, §9.1).
//
// Implemented suites: AES-128-GCM-SHA256 and ChaCha20-Poly1305-SHA256.
// AES-256-GCM-SHA384 parses as a known suite but is never negotiated: it
// needs SHA-384 transcript hashing, which the v1 key schedule does not do
// yet. The server picks by its own preference order below, so default
// handshakes keep negotiating AES-128-GCM exactly as before.

// default_cipher_suites lists the suites offered and accepted when
// Config.cipher_suites is empty, in server preference order.
pub fn default_cipher_suites() []CipherSuite {
	return [.tls_aes_128_gcm_sha256, .tls_chacha20_poly1305_sha256]
}

// cipher_suite_key_len returns the AEAD key length for a negotiated suite.
pub fn cipher_suite_key_len(cs CipherSuite) int {
	return match cs {
		.tls_aes_128_gcm_sha256 { 16 }
		.tls_aes_256_gcm_sha384 { 32 }
		.tls_chacha20_poly1305_sha256 { 32 }
	}
}

// aead_tag_size is the authentication tag length for every implemented
// suite (AES-GCM and ChaCha20-Poly1305 both use 16-byte tags).
pub const aead_tag_size = 16

// select_cipher_suite picks the suite for a handshake: server preference
// order intersected with the client's offer. Only implemented suites can
// be selected; parsed-but-unimplemented suites (AES-256-GCM-SHA384) never
// match.
pub fn select_cipher_suite(offered []CipherSuite) !CipherSuite {
	for want in default_cipher_suites() {
		if want in offered {
			return want
		}
	}
	return error('tls: no supported cipher suite')
}

// effective_cipher_suites resolves Config.cipher_suites, defaulting to the
// implemented suites when empty.
pub fn effective_cipher_suites(cfg Config) []CipherSuite {
	if cfg.cipher_suites.len > 0 {
		return cfg.cipher_suites.clone()
	}
	return default_cipher_suites()
}

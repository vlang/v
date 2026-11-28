// Copyright (c) 2026 blackshirt.
//
// aead.v adapts the TLS 1.3 AEAD suites (AES-GCM, ChaCha20-Poly1305) to a single
// seal/open interface shared by the TLS and QUIC protection styles. Nonce
// construction and size limits live in the TLS layer, not here.
module handshake

import crypto.aes
import crypto.cipher
import x.crypto.chacha20poly1305
import x.crypto.chacha20

// aead_nonce_len is the fixed 12-byte AEAD nonce size for every TLS 1.3 cipher suite
// (RFC 8446 §5.3 / RFC 8439 §2.8); the key schedule derives the nonce base to this length.
pub const aead_nonce_len = 12

// suite_hash_algo returns the HKDF hash for a cipher suite (RFC 8446 §7.1); pub for
// adapters expanding the RFC 9001 §5.1 material themselves.
pub fn suite_hash_algo(suite CipherSuite) HashAlgorithm {
	return match suite {
		.tls_aes_128_gcm_sha256 { .sha256 }
		.tls_aes_256_gcm_sha384 { .sha384 }
		.tls_chacha20_poly1305_sha256 { .sha256 }
	}
}

// suite_key_len returns the AEAD key length for a cipher suite, fixed by the cipher
// itself (RFC 8446 §7.1); pub for adapters sizing their own protection.
pub fn suite_key_len(suite CipherSuite) int {
	return match suite {
		.tls_aes_128_gcm_sha256 { 16 }
		.tls_aes_256_gcm_sha384 { 32 }
		.tls_chacha20_poly1305_sha256 { 32 }
	}
}

// new_aead_for_suite constructs the AEAD cipher for the negotiated suite; the 12-byte
// nonce matches TLS 1.3 (RFC 8446 §5.3 / RFC 8439 §2.8). It takes ownership of a
// distinct key: AES-GCM expands the key into its own round schedule, and ChaCha20-
// Poly1305 stores the slice it is handed, so that leg clones first. The callers in the
// record layer scrub their key copy once installed (§7.5), which would otherwise zero
// the shared buffer and silently corrupt every later ChaCha record (a tag mismatch that
// AES never shows). Cloning here keeps that scrub from reaching the live cipher.
pub fn new_aead_for_suite(suite CipherSuite, key []u8) !cipher.AEAD {
	return match suite {
		.tls_aes_128_gcm_sha256, .tls_aes_256_gcm_sha384 {
			aes.new_aes_gcm(key)!
		}
		.tls_chacha20_poly1305_sha256 {
			*chacha20poly1305.new(key.clone(), chacha20poly1305.nonce_size, chacha20.Options{})!
		}
	}
}

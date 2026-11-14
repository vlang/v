module tls

import crypto.hkdf
import crypto.hmac
import crypto.sha256
import crypto.sha512
import hash

// sha256_hash is the Hash constructor used throughout the TLS 1.3 key schedule.
fn sha256_hash() hash.Hash {
	return sha256.new()
}

// transcript_hash_for_suite computes Transcript-Hash with the suite hash.
pub fn transcript_hash_for_suite(data []u8, suite CipherSuite) []u8 {
	if suite == .tls_aes_256_gcm_sha384 {
		return sha512.sum384(data)
	}
	return sha256.sum256(data)
}

// hkdf_expand_label_for_suite implements TLS 1.3's HKDF-Expand-Label with the suite hash.
pub fn hkdf_expand_label_for_suite(secret []u8, label string, context []u8, length int, suite CipherSuite) ![]u8 {
	if suite == .tls_aes_256_gcm_sha384 {
		return hkdf_expand_label_with(sha512.new384, secret, label, context, length)
	}
	return hkdf_expand_label_with(sha256_hash, secret, label, context, length)
}

// hkdf_expand_label_with is the shared HKDF-Expand-Label core for one hash.
fn hkdf_expand_label_with[H](h fn () H, secret []u8, label string, context []u8, length int) ![]u8 {
	if length < 0 || length > 0xffff {
		return error('tls: hkdf_expand_label: length ${length} out of range')
	}
	full_label := 'tls13 ${label}'
	if full_label.len < 7 || full_label.len > 255 {
		return error('tls: hkdf_expand_label: label length ${full_label.len} out of range')
	}
	if context.len > 255 {
		return error('tls: hkdf_expand_label: context length ${context.len} out of range')
	}
	mut hkdf_label := []u8{cap: 2 + 1 + full_label.len + 1 + context.len}
	hkdf_label << u8(length >> 8)
	hkdf_label << u8(length)
	hkdf_label << u8(full_label.len)
	hkdf_label << full_label.bytes()
	hkdf_label << u8(context.len)
	hkdf_label << context
	return hkdf.expand(h, secret, hkdf_label.bytestr(), length)
}

// hkdf_expand_label implements TLS 1.3's HKDF-Expand-Label (RFC 8446 §7.1)
// with SHA-256 (suites 0x1301/0x1303). Use hkdf_expand_label_for_suite for
// suite agility.
pub fn hkdf_expand_label(secret []u8, label string, context []u8, length int) ![]u8 {
	return hkdf_expand_label_with(sha256_hash, secret, label, context, length)
}

// empty_transcript_hash is Transcript-Hash("") (RFC 8446 §7.1) for SHA-256 suites.
pub fn empty_transcript_hash() []u8 {
	return sha256.sum256([]u8{})
}

// empty_transcript_hash_for_suite is Transcript-Hash("") with the suite hash.
pub fn empty_transcript_hash_for_suite(suite CipherSuite) []u8 {
	if suite == .tls_aes_256_gcm_sha384 {
		return sha512.sum384([]u8{})
	}
	return empty_transcript_hash()
}

// derive_secret implements RFC 8446 §7.1 Derive-Secret (SHA-256 path).
pub fn derive_secret(secret []u8, label string, transcript_hash []u8) ![]u8 {
	return hkdf_expand_label(secret, label, transcript_hash, sha256.size)
}

// derive_secret_for_suite implements Derive-Secret with the suite hash.
pub fn derive_secret_for_suite(secret []u8, label string, transcript_hash []u8, suite CipherSuite) ![]u8 {
	return hkdf_expand_label_for_suite(secret, label, transcript_hash,
		cipher_suite_hash_len(suite), suite)
}

// derive_early_secret derives the early secret (no-PSK branch, SHA-256 path).
pub fn derive_early_secret() ![]u8 {
	zero_ikm := []u8{len: sha256.size}
	return hkdf.extract(sha256_hash, zero_ikm, []u8{})
}

// derive_early_secret_for_suite derives the early secret with the suite hash.
pub fn derive_early_secret_for_suite(suite CipherSuite) ![]u8 {
	if suite == .tls_aes_256_gcm_sha384 {
		zero_ikm := []u8{len: sha512.size384}
		return hkdf.extract(sha512.new384, zero_ikm, []u8{})
	}
	return derive_early_secret()
}

// HandshakeSecrets holds the handshake-phase traffic secrets.
pub struct HandshakeSecrets {
pub:
	handshake_secret []u8
	client_secret    []u8
	server_secret    []u8
}

// derive_handshake_secrets derives handshake traffic secrets from the (EC)DHE share.
pub fn derive_handshake_secrets(early_secret []u8, ecdhe_shared_secret []u8, transcript_hash_ch_sh []u8) !HandshakeSecrets {
	return derive_handshake_secrets_for_suite(early_secret, ecdhe_shared_secret,
		transcript_hash_ch_sh, .tls_aes_128_gcm_sha256)
}

// derive_handshake_secrets_for_suite derives handshake secrets with the suite hash.
pub fn derive_handshake_secrets_for_suite(early_secret []u8, ecdhe_shared_secret []u8, transcript_hash_ch_sh []u8, suite CipherSuite) !HandshakeSecrets {
	derived := derive_secret_for_suite(early_secret, 'derived', empty_transcript_hash_for_suite(suite),
		suite)!
	handshake_secret := if suite == .tls_aes_256_gcm_sha384 {
		hkdf.extract(sha512.new384, ecdhe_shared_secret, derived)!
	} else {
		hkdf.extract(sha256_hash, ecdhe_shared_secret, derived)!
	}
	client_secret := derive_secret_for_suite(handshake_secret, 'c hs traffic',
		transcript_hash_ch_sh, suite)!
	server_secret := derive_secret_for_suite(handshake_secret, 's hs traffic',
		transcript_hash_ch_sh, suite)!
	return HandshakeSecrets{
		handshake_secret: handshake_secret
		client_secret:    client_secret
		server_secret:    server_secret
	}
}

// ApplicationSecrets holds the master and application traffic secrets.
pub struct ApplicationSecrets {
pub:
	master_secret []u8
	client_secret []u8
	server_secret []u8
}

// derive_application_secrets derives application traffic secrets from the handshake secret.
pub fn derive_application_secrets(handshake_secret []u8, transcript_hash_ch_sfin []u8) !ApplicationSecrets {
	return derive_application_secrets_for_suite(handshake_secret, transcript_hash_ch_sfin,
		.tls_aes_128_gcm_sha256)
}

// derive_application_secrets_for_suite derives application secrets with the suite hash.
pub fn derive_application_secrets_for_suite(handshake_secret []u8, transcript_hash_ch_sfin []u8, suite CipherSuite) !ApplicationSecrets {
	derived := derive_secret_for_suite(handshake_secret, 'derived', empty_transcript_hash_for_suite(suite),
		suite)!
	zero_ikm := []u8{len: cipher_suite_hash_len(suite)}
	master_secret := if suite == .tls_aes_256_gcm_sha384 {
		hkdf.extract(sha512.new384, zero_ikm, derived)!
	} else {
		hkdf.extract(sha256_hash, zero_ikm, derived)!
	}
	client_secret := derive_secret_for_suite(master_secret, 'c ap traffic',
		transcript_hash_ch_sfin, suite)!
	server_secret := derive_secret_for_suite(master_secret, 's ap traffic',
		transcript_hash_ch_sfin, suite)!
	return ApplicationSecrets{
		master_secret: master_secret
		client_secret: client_secret
		server_secret: server_secret
	}
}

// TrafficKeys holds the AEAD key/iv and (for TLS 1.3, no separate header
// protection key is used for TCP record layer; QUIC reuses these via
// hkdf_expand_label with "quic ..." labels).
pub struct TrafficKeys {
pub:
	key []u8 // 16 bytes for AES-128-GCM
	iv  []u8 // 12 bytes
}

// derive_traffic_keys derives the AEAD key and IV from a traffic secret.
// The key length follows the negotiated suite (16 bytes for AES-128-GCM,
// 32 bytes for AES-256-GCM and ChaCha20-Poly1305); the IV is always 12.
// The HKDF hash follows the suite (SHA-384 for AES-256-GCM).
pub fn derive_traffic_keys(traffic_secret []u8, suite CipherSuite) !TrafficKeys {
	key := hkdf_expand_label_for_suite(traffic_secret, 'key', []u8{}, cipher_suite_key_len(suite),
		suite)!
	iv := hkdf_expand_label_for_suite(traffic_secret, 'iv', []u8{}, 12, suite)!
	return TrafficKeys{
		key: key
		iv:  iv
	}
}

// finished_verify_data computes the Finished verify_data (RFC 8446 §4.4.4):
// HMAC(finished_key, Transcript-Hash) truncated to Hash.length, where
// finished_key = HKDF-Expand-Label(base_key, "finished", "", Hash.length).
// SHA-256 path (suites 0x1301/0x1303).
pub fn finished_verify_data(base_secret []u8, transcript_hash []u8) ![]u8 {
	return finished_verify_data_for_suite(base_secret, transcript_hash,
		.tls_aes_128_gcm_sha256)
}

// finished_verify_data_for_suite computes verify_data with the suite hash.
pub fn finished_verify_data_for_suite(base_secret []u8, transcript_hash []u8, suite CipherSuite) ![]u8 {
	if suite == .tls_aes_256_gcm_sha384 {
		finished_key := hkdf_expand_label_for_suite(base_secret, 'finished', []u8{},
			sha512.size384, suite)!
		mut mac := hmac.new_hmac(sha512.new384, finished_key)
		mac.write(transcript_hash) or { return err }
		mut out := []u8{len: sha512.size384}
		mac.sum_into(mut out)
		return out
	}
	finished_key := hkdf_expand_label(base_secret, 'finished', []u8{}, sha256.size)!
	mut mac := hmac.new_hmac(sha256.new, finished_key)
	mac.write(transcript_hash) or { return err }
	mut out := []u8{len: sha256.size}
	mac.sum_into(mut out)
	return out
}

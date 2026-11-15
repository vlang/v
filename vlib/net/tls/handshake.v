module tls

import crypto.hkdf
import crypto.hmac
import crypto.rand
import crypto.sha256
import crypto.sha512
import hash
import math.big

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

// update_traffic_secret advances an application traffic secret (RFC 8446
// §7.2): HKDF-Expand-Label(old, "traffic upd", "", Hash.length).
pub fn update_traffic_secret(old []u8, suite CipherSuite) ![]u8 {
	return hkdf_expand_label_for_suite(old, 'traffic upd', []u8{}, cipher_suite_hash_len(suite),
		suite)
}

// derive_exporter_master derives the exporter master secret (RFC 8446 §7.5)
// from the application master secret and the full-handshake transcript hash.
pub fn derive_exporter_master(master_secret []u8, transcript_hash []u8, suite CipherSuite) ![]u8 {
	return derive_secret_for_suite(master_secret, 'exp master', transcript_hash, suite)
}

// tls_exporter computes an exporter value (RFC 8446 §7.5) from the exporter
// master secret: Derive-Secret(exp_master, label, "") then
// HKDF-Expand-Label(derived, "exporter", Hash(context), L).
pub fn tls_exporter(exporter_master []u8, label string, context []u8, length int, suite CipherSuite) ![]u8 {
	if length <= 0 || length > 0xffff {
		return error('tls: exporter length out of range')
	}
	empty := empty_transcript_hash_for_suite(suite)
	derived := derive_secret_for_suite(exporter_master, label, empty, suite)!
	ctx_hash := transcript_hash_for_suite(context, suite)
	return hkdf_expand_label_for_suite(derived, 'exporter', ctx_hash, length, suite)
}

// ---------- Named groups ----------

// ffdhe_prime_bytes returns the RFC 7919 prime for a group (generator 2).
// Source: RFC 7919 Appendix A byte-identical to the vendored mbedTLS
// MBEDTLS_DHM_RFC7919_FFDHE2048/3072_P_BIN tables.
pub fn ffdhe_prime_bytes(group u16) ![]u8 {
	if group == named_group_ffdhe2048 {
		return hex_to_bytes_ffdhe2048()
	}
	if group == named_group_ffdhe3072 {
		return hex_to_bytes_ffdhe3072()
	}
	return error('tls: unsupported FFDHE group')
}

// ffdhe_key_len returns the public-value length in bytes for a group.
pub fn ffdhe_key_len(group u16) !int {
	p := ffdhe_prime_bytes(group)!
	return p.len
}

fn hex_to_bytes_ffdhe2048() []u8 {
	return [u8(0xff), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xad, 0xf8, 0x54, 0x58, 0xa2, 0xbb,
		0x4a, 0x9a, 0xaf, 0xdc, 0x56, 0x20, 0x27, 0x3d, 0x3c, 0xf1, 0xd8, 0xb9, 0xc5, 0x83, 0xce,
		0x2d, 0x36, 0x95, 0xa9, 0xe1, 0x36, 0x41, 0x14, 0x64, 0x33, 0xfb, 0xcc, 0x93, 0x9d, 0xce,
		0x24, 0x9b, 0x3e, 0xf9, 0x7d, 0x2f, 0xe3, 0x63, 0x63, 0x0c, 0x75, 0xd8, 0xf6, 0x81, 0xb2,
		0x02, 0xae, 0xc4, 0x61, 0x7a, 0xd3, 0xdf, 0x1e, 0xd5, 0xd5, 0xfd, 0x65, 0x61, 0x24, 0x33,
		0xf5, 0x1f, 0x5f, 0x06, 0x6e, 0xd0, 0x85, 0x63, 0x65, 0x55, 0x3d, 0xed, 0x1a, 0xf3, 0xb5,
		0x57, 0x13, 0x5e, 0x7f, 0x57, 0xc9, 0x35, 0x98, 0x4f, 0x0c, 0x70, 0xe0, 0xe6, 0x8b, 0x77,
		0xe2, 0xa6, 0x89, 0xda, 0xf3, 0xef, 0xe8, 0x72, 0x1d, 0xf1, 0x58, 0xa1, 0x36, 0xad, 0xe7,
		0x35, 0x30, 0xac, 0xca, 0x4f, 0x48, 0x3a, 0x79, 0x7a, 0xbc, 0x0a, 0xb1, 0x82, 0xb3, 0x24,
		0xfb, 0x61, 0xd1, 0x08, 0xa9, 0x4b, 0xb2, 0xc8, 0xe3, 0xfb, 0xb9, 0x6a, 0xda, 0xb7, 0x60,
		0xd7, 0xf4, 0x68, 0x1d, 0x4f, 0x42, 0xa3, 0xde, 0x39, 0x4d, 0xf4, 0xae, 0x56, 0xed, 0xe7,
		0x63, 0x72, 0xbb, 0x19, 0x0b, 0x07, 0xa7, 0xc8, 0xee, 0x0a, 0x6d, 0x70, 0x9e, 0x02, 0xfc,
		0xe1, 0xcd, 0xf7, 0xe2, 0xec, 0xc0, 0x34, 0x04, 0xcd, 0x28, 0x34, 0x2f, 0x61, 0x91, 0x72,
		0xfe, 0x9c, 0xe9, 0x85, 0x83, 0xff, 0x8e, 0x4f, 0x12, 0x32, 0xee, 0xf2, 0x81, 0x83, 0xc3,
		0xfe, 0x3b, 0x1b, 0x4c, 0x6f, 0xad, 0x73, 0x3b, 0xb5, 0xfc, 0xbc, 0x2e, 0xc2, 0x20, 0x05,
		0xc5, 0x8e, 0xf1, 0x83, 0x7d, 0x16, 0x83, 0xb2, 0xc6, 0xf3, 0x4a, 0x26, 0xc1, 0xb2, 0xef,
		0xfa, 0x88, 0x6b, 0x42, 0x38, 0x61, 0x28, 0x5c, 0x97, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		0xff, 0xff]
}

fn hex_to_bytes_ffdhe3072() []u8 {
	return [u8(0xff), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xad, 0xf8, 0x54, 0x58, 0xa2, 0xbb,
		0x4a, 0x9a, 0xaf, 0xdc, 0x56, 0x20, 0x27, 0x3d, 0x3c, 0xf1, 0xd8, 0xb9, 0xc5, 0x83, 0xce,
		0x2d, 0x36, 0x95, 0xa9, 0xe1, 0x36, 0x41, 0x14, 0x64, 0x33, 0xfb, 0xcc, 0x93, 0x9d, 0xce,
		0x24, 0x9b, 0x3e, 0xf9, 0x7d, 0x2f, 0xe3, 0x63, 0x63, 0x0c, 0x75, 0xd8, 0xf6, 0x81, 0xb2,
		0x02, 0xae, 0xc4, 0x61, 0x7a, 0xd3, 0xdf, 0x1e, 0xd5, 0xd5, 0xfd, 0x65, 0x61, 0x24, 0x33,
		0xf5, 0x1f, 0x5f, 0x06, 0x6e, 0xd0, 0x85, 0x63, 0x65, 0x55, 0x3d, 0xed, 0x1a, 0xf3, 0xb5,
		0x57, 0x13, 0x5e, 0x7f, 0x57, 0xc9, 0x35, 0x98, 0x4f, 0x0c, 0x70, 0xe0, 0xe6, 0x8b, 0x77,
		0xe2, 0xa6, 0x89, 0xda, 0xf3, 0xef, 0xe8, 0x72, 0x1d, 0xf1, 0x58, 0xa1, 0x36, 0xad, 0xe7,
		0x35, 0x30, 0xac, 0xca, 0x4f, 0x48, 0x3a, 0x79, 0x7a, 0xbc, 0x0a, 0xb1, 0x82, 0xb3, 0x24,
		0xfb, 0x61, 0xd1, 0x08, 0xa9, 0x4b, 0xb2, 0xc8, 0xe3, 0xfb, 0xb9, 0x6a, 0xda, 0xb7, 0x60,
		0xd7, 0xf4, 0x68, 0x1d, 0x4f, 0x42, 0xa3, 0xde, 0x39, 0x4d, 0xf4, 0xae, 0x56, 0xed, 0xe7,
		0x63, 0x72, 0xbb, 0x19, 0x0b, 0x07, 0xa7, 0xc8, 0xee, 0x0a, 0x6d, 0x70, 0x9e, 0x02, 0xfc,
		0xe1, 0xcd, 0xf7, 0xe2, 0xec, 0xc0, 0x34, 0x04, 0xcd, 0x28, 0x34, 0x2f, 0x61, 0x91, 0x72,
		0xfe, 0x9c, 0xe9, 0x85, 0x83, 0xff, 0x8e, 0x4f, 0x12, 0x32, 0xee, 0xf2, 0x81, 0x83, 0xc3,
		0xfe, 0x3b, 0x1b, 0x4c, 0x6f, 0xad, 0x73, 0x3b, 0xb5, 0xfc, 0xbc, 0x2e, 0xc2, 0x20, 0x05,
		0xc5, 0x8e, 0xf1, 0x83, 0x7d, 0x16, 0x83, 0xb2, 0xc6, 0xf3, 0x4a, 0x26, 0xc1, 0xb2, 0xef,
		0xfa, 0x88, 0x6b, 0x42, 0x38, 0x61, 0x1f, 0xcf, 0xdc, 0xde, 0x35, 0x5b, 0x3b, 0x65, 0x19,
		0x03, 0x5b, 0xbc, 0x34, 0xf4, 0xde, 0xf9, 0x9c, 0x02, 0x38, 0x61, 0xb4, 0x6f, 0xc9, 0xd6,
		0xe6, 0xc9, 0x07, 0x7a, 0xd9, 0x1d, 0x26, 0x91, 0xf7, 0xf7, 0xee, 0x59, 0x8c, 0xb0, 0xfa,
		0xc1, 0x86, 0xd9, 0x1c, 0xae, 0xfe, 0x13, 0x09, 0x85, 0x13, 0x92, 0x70, 0xb4, 0x13, 0x0c,
		0x93, 0xbc, 0x43, 0x79, 0x44, 0xf4, 0xfd, 0x44, 0x52, 0xe2, 0xd7, 0x4d, 0xd3, 0x64, 0xf2,
		0xe2, 0x1e, 0x71, 0xf5, 0x4b, 0xff, 0x5c, 0xae, 0x82, 0xab, 0x9c, 0x9d, 0xf6, 0x9e, 0xe8,
		0x6d, 0x2b, 0xc5, 0x22, 0x36, 0x3a, 0x0d, 0xab, 0xc5, 0x21, 0x97, 0x9b, 0x0d, 0xea, 0xda,
		0x1d, 0xbf, 0x9a, 0x42, 0xd5, 0xc4, 0x48, 0x4e, 0x0a, 0xbc, 0xd0, 0x6b, 0xfa, 0x53, 0xdd,
		0xef, 0x3c, 0x1b, 0x20, 0xee, 0x3f, 0xd5, 0x9d, 0x7c, 0x25, 0xe4, 0x1d, 0x2b, 0x66, 0xc6,
		0x2e, 0x37, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
}

// ffdhe_pad_left left-pads b with zeros to length n.
fn ffdhe_pad_left(b []u8, n int) []u8 {
	if b.len >= n {
		return b[b.len - n..].clone()
	}
	mut out := []u8{len: n}
	for i in 0 .. b.len {
		out[n - b.len + i] = b[i]
	}
	return out
}

// ffdhe_generate creates an ephemeral FFDHE keypair for group.
pub fn ffdhe_generate(group u16) !([]u8, []u8) {
	p_bytes := ffdhe_prime_bytes(group)!
	p := big.integer_from_bytes(p_bytes, big.IntegerConfig{})
	g := big.integer_from_int(2)
	priv_len := if group == named_group_ffdhe3072 { 48 } else { 32 }
	priv_bytes := rand.bytes(priv_len)!
	if priv_bytes.len == 0 {
		return error('tls: bad FFDHE private key')
	}
	x := big.integer_from_bytes(priv_bytes, big.IntegerConfig{})
	pub_int := g.big_mod_pow(x, p)!
	pub_b, _ := pub_int.bytes()
	return priv_bytes, ffdhe_pad_left(pub_b, p_bytes.len)
}

// ffdhe_compute derives the FFDHE shared secret from our private and the
// peer's public value (both big-endian, peer padded to prime length).
pub fn ffdhe_compute(priv_bytes []u8, peer_bytes []u8, group u16) ![]u8 {
	p_bytes := ffdhe_prime_bytes(group)!
	if peer_bytes.len == 0 || peer_bytes.len > p_bytes.len {
		return error('tls: bad FFDHE peer key length')
	}
	if priv_bytes.len == 0 {
		return error('tls: bad FFDHE private key')
	}
	padded := ffdhe_pad_left(peer_bytes, p_bytes.len)
	p := big.integer_from_bytes(p_bytes, big.IntegerConfig{})
	y := big.integer_from_bytes(padded, big.IntegerConfig{})
	// Validate 1 < Y < P-1 (RFC 7919 §5.1).
	one := big.integer_from_int(1)
	pm1 := p - one
	if !(one < y) || !(y < pm1) {
		return error('tls: bad FFDHE peer key')
	}
	x := big.integer_from_bytes(priv_bytes, big.IntegerConfig{})
	ss := y.big_mod_pow(x, p)!
	ss_b, _ := ss.bytes()
	return ffdhe_pad_left(ss_b, p_bytes.len)
}

// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// TLS 1.2 cipher suite definitions (RFC 5246, RFC 5288, RFC 5289).
// This module is standalone: it does not import net.tls internals.
module tls12

// wires versions
pub const version_tls12 = u16(0x0303)

// Cipher suite IDs implemented by this module. The two GCM suites are the
// minimum required; the two CBC suites (HMAC-SHA256, RFC 5289-style SHA256
// variants) exercise the CBC record path.
pub const suite_ecdhe_rsa_aes128_gcm_sha256 = u16(0xC02F)
pub const suite_ecdhe_ecdsa_aes128_gcm_sha256 = u16(0xC02B)
pub const suite_ecdhe_rsa_aes128_cbc_sha256 = u16(0xC027)
pub const suite_ecdhe_ecdsa_aes128_cbc_sha256 = u16(0xC023)

// default_suites is the client offer order: GCM first, then CBC.
pub const default_suites = [u16(suite_ecdhe_rsa_aes128_gcm_sha256),
	suite_ecdhe_ecdsa_aes128_gcm_sha256, suite_ecdhe_rsa_aes128_cbc_sha256,
	suite_ecdhe_ecdsa_aes128_cbc_sha256]

// suite_is_rsa reports whether the suite needs an RSA server key.
pub fn suite_is_rsa(suite u16) bool {
	return suite == suite_ecdhe_rsa_aes128_gcm_sha256
		|| suite == suite_ecdhe_rsa_aes128_cbc_sha256
}

// suite_is_ecdsa reports whether the suite needs an ECDSA server key.
pub fn suite_is_ecdsa(suite u16) bool {
	return suite == suite_ecdhe_ecdsa_aes128_gcm_sha256
		|| suite == suite_ecdhe_ecdsa_aes128_cbc_sha256
}

// suite_is_gcm reports whether the suite uses AES-GCM (true) or
// AES-CBC with HMAC-SHA256 (false) for the record layer.
pub fn suite_is_gcm(suite u16) bool {
	return suite == suite_ecdhe_rsa_aes128_gcm_sha256
		|| suite == suite_ecdhe_ecdsa_aes128_gcm_sha256
}

// suite_is_supported reports whether this module implements the suite.
pub fn suite_is_supported(suite u16) bool {
	return suite == suite_ecdhe_rsa_aes128_gcm_sha256
		|| suite == suite_ecdhe_ecdsa_aes128_gcm_sha256
		|| suite == suite_ecdhe_rsa_aes128_cbc_sha256
		|| suite == suite_ecdhe_ecdsa_aes128_cbc_sha256
}

// suite_name returns the IANA name of a suite (or 'UNKNOWN').
pub fn suite_name(suite u16) string {
	return match suite {
		suite_ecdhe_rsa_aes128_gcm_sha256 { 'TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256' }
		suite_ecdhe_ecdsa_aes128_gcm_sha256 { 'TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256' }
		suite_ecdhe_rsa_aes128_cbc_sha256 { 'TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256' }
		suite_ecdhe_ecdsa_aes128_cbc_sha256 { 'TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256' }
		else {
			'UNKNOWN'
		}
	}
}

// key_lengths returns (mac_len, enc_key_len, fixed_iv_len) for the suite:
// GCM suites need no MAC key and use a 4-byte per-direction salt;
// CBC suites use HMAC-SHA256 (32-byte MAC keys) and 16-byte keys/IVs.
pub fn suite_key_lengths(suite u16) (int, int, int) {
	if suite_is_gcm(suite) {
		return 0, 16, 4
	}
	return 32, 16, 16
}

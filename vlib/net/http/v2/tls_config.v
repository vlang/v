module v2

// RFC 7540 §9.2.2 TLS cipher suite blacklist for HTTP/2 connections.
//
// Appendix A of RFC 7540 lists cipher suites that SHOULD NOT be used
// with HTTP/2 because they lack forward secrecy, use weak algorithms,
// or provide insufficient security.  mbedtls/OpenSSL default to modern
// TLS 1.2+ AEAD ciphers so blacklisted suites are unlikely to be
// negotiated; this module provides the list and a validator for use
// once V's TLS API exposes the negotiated cipher name.

// http2_forbidden_ciphers contains cipher suite name prefixes banned by RFC 7540 §9.2.2.
pub const http2_forbidden_ciphers = [
	'TLS_RSA_WITH_',
	'TLS_NULL_',
	'TLS_EXPORT_',
	'TLS_DES_',
	'TLS_3DES_',
	'TLS_RC4_',
]

// is_forbidden_cipher checks if a cipher name matches the HTTP/2 blacklist
// defined in RFC 7540 Appendix A.  The check is case-insensitive and
// inspects both prefixes and weak-algorithm substrings.
pub fn is_forbidden_cipher(cipher_name string) bool {
	upper := cipher_name.to_upper()
	for prefix in http2_forbidden_ciphers {
		if upper.starts_with(prefix) {
			return true
		}
	}
	return contains_weak_substring(upper)
}

// contains_weak_substring returns true when the uppercased cipher name
// contains a substring that identifies a weak algorithm forbidden by
// RFC 7540 §9.2.2.
fn contains_weak_substring(upper string) bool {
	if upper.contains('_NULL_') {
		return true
	}
	if upper.contains('_EXPORT_') {
		return true
	}
	if upper.contains('_DES_') {
		return true
	}
	if upper.contains('_RC4_') {
		return true
	}
	return false
}

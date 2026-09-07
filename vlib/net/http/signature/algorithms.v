// Algorithm identifiers from the IANA "HTTP Signature Algorithms" registry
// (RFC 9421 §6.2.2). Only those backed by `vlib/crypto` primitives are
// modelled here; RSA-based algorithms are deliberately omitted because
// `vlib/crypto` does not yet ship an RSA implementation.
module signature

// Algorithm names the signing or verification routine selected for a
// signature. The string form returned by `name()` is the exact token
// emitted on the wire as the value of the `alg` signature parameter.
pub enum Algorithm {
	hmac_sha256       // hmac-sha256       — RFC 9421 §3.3.3
	ecdsa_p256_sha256 // ecdsa-p256-sha256 — RFC 9421 §3.3.4
	ecdsa_p384_sha384 // ecdsa-p384-sha384 — RFC 9421 §3.3.5
	ed25519           // ed25519           — RFC 9421 §3.3.6
}

// name returns the IANA token for the algorithm.
pub fn (a Algorithm) name() string {
	return match a {
		.hmac_sha256 { 'hmac-sha256' }
		.ecdsa_p256_sha256 { 'ecdsa-p256-sha256' }
		.ecdsa_p384_sha384 { 'ecdsa-p384-sha384' }
		.ed25519 { 'ed25519' }
	}
}

// algorithm_from_name parses the IANA token. Returns `none` for
// algorithms outside this module's supported set so the caller can
// surface an `UnsupportedAlgorithm` error with the original token kept.
pub fn algorithm_from_name(s string) ?Algorithm {
	return match s {
		'hmac-sha256' { Algorithm.hmac_sha256 }
		'ecdsa-p256-sha256' { Algorithm.ecdsa_p256_sha256 }
		'ecdsa-p384-sha384' { Algorithm.ecdsa_p384_sha384 }
		'ed25519' { Algorithm.ed25519 }
		else { none }
	}
}

// is_mac reports whether the algorithm is symmetric (MAC) rather than
// asymmetric (signature). MACs are signed and verified with the same key.
pub fn (a Algorithm) is_mac() bool {
	return a == .hmac_sha256
}

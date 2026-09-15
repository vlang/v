// Algorithm identifiers from the IANA "COSE Algorithms" registry
// (https://www.iana.org/assignments/cose). Only the signature and MAC
// algorithms supported by the module are listed here; entries for
// AEAD encryption / RSA / key wrap will be added when those families
// land.
module cose

// Algorithm is a COSE algorithm identifier as registered in the IANA
// "COSE Algorithms" registry. Values match the integer codes defined by
// RFC 9053.
pub enum Algorithm {
	// Signature algorithms (RFC 9053 §2)
	es256 = -7  // ECDSA w/ SHA-256, curve P-256
	es384 = -35 // ECDSA w/ SHA-384, curve P-384
	es512 = -36 // ECDSA w/ SHA-512, curve P-521
	eddsa = -8  // EdDSA (Ed25519 in this module)

	// MAC algorithms (RFC 9053 §3)
	hmac_256_64  = 4 // HMAC w/ SHA-256, truncated to 64 bits
	hmac_256_256 = 5 // HMAC w/ SHA-256
	hmac_384_384 = 6 // HMAC w/ SHA-384
	hmac_512_512 = 7 // HMAC w/ SHA-512
}

// is_signature reports whether the algorithm is a signature algorithm
// (used with COSE_Sign and COSE_Sign1).
pub fn (a Algorithm) is_signature() bool {
	return a in [.es256, .es384, .es512, .eddsa]
}

// is_mac reports whether the algorithm is a MAC algorithm (used with
// COSE_Mac and COSE_Mac0).
pub fn (a Algorithm) is_mac() bool {
	return a in [.hmac_256_64, .hmac_256_256, .hmac_384_384, .hmac_512_512]
}

// algorithm_from_int converts an IANA algorithm code to a typed
// Algorithm. It returns an error if the code is not supported by this
// module.
pub fn algorithm_from_int(code i64) !Algorithm {
	return match code {
		-7 { Algorithm.es256 }
		-35 { Algorithm.es384 }
		-36 { Algorithm.es512 }
		-8 { Algorithm.eddsa }
		4 { Algorithm.hmac_256_64 }
		5 { Algorithm.hmac_256_256 }
		6 { Algorithm.hmac_384_384 }
		7 { Algorithm.hmac_512_512 }
		else { error('cose: unsupported algorithm code ${code}') }
	}
}

// name returns the IANA registered name of the algorithm (e.g. "ES256").
// Useful for error messages and logging; the wire format always uses the
// integer code.
pub fn (a Algorithm) name() string {
	return match a {
		.es256 { 'ES256' }
		.es384 { 'ES384' }
		.es512 { 'ES512' }
		.eddsa { 'EdDSA' }
		.hmac_256_64 { 'HMAC 256/64' }
		.hmac_256_256 { 'HMAC 256/256' }
		.hmac_384_384 { 'HMAC 384/384' }
		.hmac_512_512 { 'HMAC 512/512' }
	}
}

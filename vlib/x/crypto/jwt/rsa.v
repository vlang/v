module jwt

import crypto
import crypto.rsa
import crypto.sha256
import crypto.sha512
import encoding.base64
import json2

// rsa_hash returns the hash of an RSA algorithm.
fn rsa_hash(alg Algorithm) crypto.Hash {
	return match alg {
		.rs384, .ps384 { crypto.Hash.sha384 }
		.rs512, .ps512 { crypto.Hash.sha512 }
		else { crypto.Hash.sha256 }
	}
}

// rsa_digest returns the digest of `data` for an RSA algorithm.
fn rsa_digest(alg Algorithm, data []u8) []u8 {
	return match rsa_hash(alg) {
		.sha384 { sha512.sum384(data) }
		.sha512 { sha512.sum512(data) }
		else { sha256.sum256(data) }
	}
}

// sign_with_rsa_key signs `claims` like `sign`, with an RSA key that was parsed
// once, for RS256/384/512 and PS256/384/512. Prefer it to `sign` with DER bytes:
// `sign` parses and checks the key again for each token, with arithmetic that
// is not constant time.
pub fn sign_with_rsa_key[T](claims T, key rsa.PrivateKey, options SignOptions) !string {
	header := Header{
		alg: options.algorithm.str()
		typ: 'JWT'
		kid: options.kid
	}
	return sign_raw_with_rsa_key(header, json2.encode(claims, escape_unicode: true), key)
}

// sign_raw_with_rsa_key signs an arbitrary raw JSON payload like `sign_raw`, with
// an RSA key that was parsed once. The header algorithm must be RS256/384/512 or
// PS256/384/512.
pub fn sign_raw_with_rsa_key(header Header, payload_json string, key rsa.PrivateKey) !string {
	alg := parse_algorithm(header.alg)!
	if alg !in [.rs256, .rs384, .rs512, .ps256, .ps384, .ps512] {
		return error('${alg} is not an RSA algorithm')
	}
	signing_input := encode_signing_input(header, payload_json)
	sig := rsa_sign_with_key(alg, key, signing_input.bytes())!
	return '${signing_input}.${base64.url_encode(sig)}'
}

// rsa_sign signs `data` for RS256/384/512 (RSASSA-PKCS1-v1_5) or PS256/384/512
// (RSASSA-PSS with a salt as long as the digest, RFC 7518 §3.5). `key` is a DER
// private key, PKCS #8 or PKCS #1.
fn rsa_sign(alg Algorithm, key []u8, data []u8) ![]u8 {
	private_key := rsa.PrivateKey.from_der(key) or {
		return error('invalid RSA private key for ${alg}: ${err.msg()}')
	}
	return rsa_sign_with_key(alg, private_key, data)
}

// rsa_sign_with_key signs `data` for an RSA algorithm with a parsed key.
fn rsa_sign_with_key(alg Algorithm, key rsa.PrivateKey, data []u8) ![]u8 {
	digest := rsa_digest(alg, data)
	return match alg {
		.ps256, .ps384, .ps512 { key.sign_pss(rsa_hash(alg), digest)! }
		else { key.sign_pkcs1v15(rsa_hash(alg), digest)! }
	}
}

// rsa_verify reports whether `sig` is a valid RS256/384/512 or PS256/384/512
// signature of `data`. `key` is a DER public key, X.509 SubjectPublicKeyInfo
// or PKCS #1.
fn rsa_verify(alg Algorithm, key []u8, data []u8, sig []u8) !bool {
	public_key := rsa.PublicKey.from_der(key) or {
		return error('invalid RSA public key for ${alg}: ${err.msg()}')
	}
	digest := rsa_digest(alg, data)
	return match alg {
		.ps256, .ps384, .ps512 { public_key.verify_pss(rsa_hash(alg), digest, sig)! }
		else { public_key.verify_pkcs1v15(rsa_hash(alg), digest, sig)! }
	}
}

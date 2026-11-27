module jwt

import crypto.rsa
import encoding.base64
import json2

// Jwk is a public JSON Web Key (RFC 7517): an RSA key (RFC 7518 §6.3.1) or an
// Ed25519 key (RFC 8037). Other members, such as `x5c`, are ignored.
pub struct Jwk {
pub:
	kty string
	kid ?string
	use ?string
	alg ?string
	// RSA modulus and public exponent, base64url-encoded.
	n ?string
	e ?string
	// Curve and public key of an OKP key, base64url-encoded.
	crv ?string
	x   ?string
}

// Jwks is a JSON Web Key Set, as published at the `jwks_uri` of an OpenID
// provider such as Keycloak.
pub struct Jwks {
pub:
	keys []Jwk
}

// Jwks.parse parses a JSON Web Key Set.
pub fn Jwks.parse(data string) !Jwks {
	return json2.decode[Jwks](data) or { error('invalid JWKS: ${err.msg()}') }
}

// encode returns the key set as JSON.
pub fn (s Jwks) encode() string {
	return json2.encode(s)
}

// key returns the key of the set that can verify `alg` signatures (see
// `check_algorithm`) and whose `kid` is `kid`, or any `kid` when `kid` is none.
// Exactly one key must match: several keys may share a `kid` (RFC 7517 §4.5),
// as long as only one of them fits the algorithm.
pub fn (s Jwks) key(kid ?string, alg Algorithm) !Jwk {
	mut found := []Jwk{}
	for k in s.keys {
		if id := kid {
			if k.kid or { continue } != id {
				continue
			}
		}
		k.check_algorithm(alg) or { continue }
		found << k
	}
	label := if id := kid { 'with kid "${id}"' } else { 'without a kid' }
	if found.len == 0 {
		return error('no JWKS key ${label} for ${alg}')
	}
	if found.len > 1 {
		return error('${found.len} JWKS keys ${label} fit ${alg}')
	}
	return found[0]
}

// Jwk.from_rsa_public_key returns the JWK of an RSA public key, for the
// algorithm `alg` and with the key id `kid`.
pub fn Jwk.from_rsa_public_key(key rsa.PublicKey, kid string, alg Algorithm) Jwk {
	e := key.exponent()
	mut e_bytes := [u8(e >> 24), u8(e >> 16), u8(e >> 8), u8(e)]
	for e_bytes.len > 1 && e_bytes[0] == 0 {
		e_bytes.delete(0)
	}
	return Jwk{
		kty: 'RSA'
		kid: kid
		use: 'sig'
		alg: alg.str()
		n:   base64.url_encode(key.modulus())
		e:   base64.url_encode(e_bytes)
	}
}

// encode returns the key as JSON.
pub fn (k Jwk) encode() string {
	return json2.encode(k)
}

// verification_key returns the key in the form `verify` expects: a DER
// SubjectPublicKeyInfo for an RSA key, the 32 raw bytes for an Ed25519 key.
pub fn (k Jwk) verification_key() ![]u8 {
	match k.kty {
		'RSA' {
			n := decode_segment(k.n or { return error('RSA JWK is missing "n"') })!
			e := decode_segment(k.e or { return error('RSA JWK is missing "e"') })!
			if e.len > 4 || (e.len == 4 && e[0] & 0x80 != 0) {
				return error('RSA JWK exponent is too large')
			}
			mut exponent := u32(0)
			for b in e {
				exponent = (exponent << 8) | u32(b)
			}
			return rsa.PublicKey.new(n, int(exponent))!.to_der()
		}
		'OKP' {
			if k.crv or { '' } != 'Ed25519' {
				return error('unsupported OKP curve, expected Ed25519')
			}
			x := decode_segment(k.x or { return error('OKP JWK is missing "x"') })!
			if x.len != 32 {
				return error('invalid Ed25519 JWK public key length')
			}
			return x
		}
		else {
			return error('unsupported JWK key type "${k.kty}"')
		}
	}
}

// check_algorithm checks that the key can verify signatures of `alg`: its type
// matches the algorithm, and its `alg` and `use`, when present, allow it.
pub fn (k Jwk) check_algorithm(alg Algorithm) ! {
	family := match alg {
		.rs256, .rs384, .rs512, .ps256, .ps384, .ps512 { 'RSA' }
		.eddsa { 'OKP' }
		else { return error('${alg} keys cannot be taken from a JWKS') }
	}
	if k.kty != family {
		return error('JWK of type "${k.kty}" cannot verify ${alg}')
	}
	if key_alg := k.alg {
		if key_alg != alg.str() {
			return error('JWK is for ${key_alg}, not ${alg}')
		}
	}
	if key_use := k.use {
		if key_use != 'sig' {
			return error('JWK is for "${key_use}", not for signatures')
		}
	}
}

// verify_with_jwks verifies `token` with the key of `jwks` named by the `kid` of
// its header that fits the algorithm, or the only such key of the set when the
// token has no `kid`.
// options.algorithm must be the trusted algorithm of the issuer; the key must
// match it.
pub fn verify_with_jwks[T](token string, jwks Jwks, options VerifyOptions) !Token[T] {
	header := decode_header(token)!
	key := jwks.key(header.kid, options.algorithm)!
	return verify[T](token, key.verification_key()!, options)
}

// Copyright (c) 2026 V language contributors. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module jwt

import crypto.ed25519
import crypto.hmac
import crypto.sha256
import crypto.sha512
import encoding.base64
import json2
import net.mbedtls
import time

// Algorithm represents the cryptographic signing algorithm used in the JWS/JWT.
pub enum Algorithm {
	hs256
	hs384
	hs512
	eddsa
	rs256
}

// str returns the standard string identifier (e.g. 'HS256', 'EdDSA') for the algorithm.
pub fn (a Algorithm) str() string {
	return match a {
		.hs256 { 'HS256' }
		.hs384 { 'HS384' }
		.hs512 { 'HS512' }
		.eddsa { 'EdDSA' }
		.rs256 { 'RS256' }
	}
}

// parse_algorithm parses a string representation of an algorithm into an Algorithm enum.
pub fn parse_algorithm(name string) !Algorithm {
	return match name {
		'HS256' { Algorithm.hs256 }
		'HS384' { Algorithm.hs384 }
		'HS512' { Algorithm.hs512 }
		'EdDSA' { Algorithm.eddsa }
		'RS256' { Algorithm.rs256 }
		else { error('unsupported or invalid JWT algorithm: ${name}') }
	}
}

// Header represents the JOSE Header of a JSON Web Token.
pub struct Header {
pub mut:
	alg string = 'HS256'
	typ string = 'JWT'
	kid ?string
}

// StandardClaims contains the standard registered claims defined in RFC 7519 section 4.1.
pub struct StandardClaims {
pub mut:
	iss ?string // Issuer
	sub ?string // Subject
	aud ?string // Audience
	exp ?i64    // Expiration Time (Unix timestamp in seconds)
	nbf ?i64    // Not Before (Unix timestamp in seconds)
	iat ?i64    // Issued At (Unix timestamp in seconds)
	jti ?string // JWT ID
}

// is_expired returns true if `exp` is defined and `now` is at or past `exp`.
pub fn (c StandardClaims) is_expired(now i64) bool {
	if exp := c.exp {
		return now >= exp
	}
	return false
}

// is_valid_at checks if the claims are currently valid at timestamp `now`
// according to `exp` and `nbf` (with 0 leeway).
pub fn (c StandardClaims) is_valid_at(now i64) bool {
	if exp := c.exp {
		if now >= exp {
			return false
		}
	}
	if nbf := c.nbf {
		if now < nbf {
			return false
		}
	}
	return true
}

// Token represents a parsed or verified JSON Web Token.
pub struct Token[T] {
pub:
	header    Header
	claims    T
	signature []u8
	raw       string
}

// SignOptions provides configuration options for signing tokens.
pub struct SignOptions {
pub:
	algorithm Algorithm = .hs256
	kid       ?string
}

// VerifyOptions provides configuration options for validating and verifying tokens.
// algorithm must be explicitly set to the trusted algorithm associated with the verification key.
pub struct VerifyOptions {
pub:
	algorithm         Algorithm @[required]
	validate_exp      bool = true
	validate_nbf      bool = true
	leeway            i64
	current_time      ?i64
	expected_issuer   ?string
	expected_audience ?string
	expected_subject  ?string
}

// sign serializes `claims` to JSON, attaches a JOSE header, signs it using `key`,
// and returns the compact encoded JWT string (`<header>.<payload>.<signature>`).
pub fn sign[T](claims T, key []u8, options SignOptions) !string {
	header := Header{
		alg: options.algorithm.str()
		typ: 'JWT'
		kid: options.kid
	}
	payload_json := json2.encode(claims, escape_unicode: true)
	return sign_raw(header, payload_json, key)
}

// sign_with_secret is a convenience helper for signing with a string secret key.
pub fn sign_with_secret[T](claims T, secret string, options SignOptions) !string {
	return sign[T](claims, secret.bytes(), options)
}

// sign_raw signs an arbitrary raw JSON payload string using `header` and `key`.
pub fn sign_raw(header Header, payload_json string, key []u8) !string {
	alg := parse_algorithm(header.alg)!
	header_json := json2.encode(header, escape_unicode: true)
	encoded_header := base64.url_encode(header_json.bytes())
	encoded_payload := base64.url_encode(payload_json.bytes())
	signing_input := '${encoded_header}.${encoded_payload}'

	sig := match alg {
		.hs256 {
			hmac.new(key, signing_input.bytes(), sha256.sum, sha256.block_size)
		}
		.hs384 {
			hmac.new(key, signing_input.bytes(), sha512.sum384, sha512.block_size)
		}
		.hs512 {
			hmac.new(key, signing_input.bytes(), sha512.sum512, sha512.block_size)
		}
		.eddsa {
			if key.len != ed25519.private_key_size {
				return error('invalid private key length for EdDSA (expected 64 bytes)')
			}
			ed25519.sign(key, signing_input.bytes())!
		}
		.rs256 {
			rsa_sign_pkcs1v15_sha256(key, signing_input.bytes())!
		}
	}
	encoded_sig := base64.url_encode(sig)
	return '${signing_input}.${encoded_sig}'
}

// verify validates token signature, header, standard claims, and decodes claims into `T`.
// options.algorithm must specify the trusted algorithm associated with `key`.
pub fn verify[T](token string, key []u8, options VerifyOptions) !Token[T] {
	parts := token.split('.')
	if parts.len != 3 {
		return error('invalid token format: expected 3 dot-separated parts')
	}
	header, _ := decode_raw(token)!
	payload_json := verify_raw(token, key, options)!
	claims := json2.decode[T](payload_json, strict: true) or {
		return error('failed to decode claims into requested type: ${err.msg()}')
	}
	sig := decode_segment(parts[2])!
	return Token[T]{
		header:    header
		claims:    claims
		signature: sig
		raw:       token
	}
}

// verify_with_secret is a convenience helper for verifying a token with a string secret key.
// options.algorithm must specify the trusted algorithm associated with `secret`.
pub fn verify_with_secret[T](token string, secret string, options VerifyOptions) !Token[T] {
	return verify[T](token, secret.bytes(), options)
}

// verify_raw verifies token structure, algorithm, signature, and standard claims,
// returning the decoded payload JSON string.
// options.algorithm must specify the trusted algorithm associated with `key`.
pub fn verify_raw(token string, key []u8, options VerifyOptions) !string {
	parts := token.split('.')
	if parts.len != 3 {
		return error('invalid token format: expected 3 dot-separated parts')
	}

	header, payload_json := decode_raw(token)!
	alg := parse_algorithm(header.alg)!

	if alg != options.algorithm {
		return error('algorithm mismatch: expected ${options.algorithm}, got ${alg}')
	}

	signing_input := '${parts[0]}.${parts[1]}'
	expected_sig := decode_segment(parts[2])!

	match alg {
		.hs256 {
			computed_sig := hmac.new(key, signing_input.bytes(), sha256.sum, sha256.block_size)
			if !hmac.equal(computed_sig, expected_sig) {
				return error('invalid token signature')
			}
		}
		.hs384 {
			computed_sig := hmac.new(key, signing_input.bytes(), sha512.sum384, sha512.block_size)
			if !hmac.equal(computed_sig, expected_sig) {
				return error('invalid token signature')
			}
		}
		.hs512 {
			computed_sig := hmac.new(key, signing_input.bytes(), sha512.sum512, sha512.block_size)
			if !hmac.equal(computed_sig, expected_sig) {
				return error('invalid token signature')
			}
		}
		.eddsa {
			if key.len != ed25519.public_key_size {
				return error('invalid public key length for EdDSA (expected 32 bytes)')
			}
			valid := ed25519.verify(key, signing_input.bytes(), expected_sig) or { false }
			if !valid {
				return error('invalid token signature')
			}
		}
		.rs256 {
			valid := rsa_verify_pkcs1v15_sha256(key, signing_input.bytes(), expected_sig) or { false }
			if !valid {
				return error('invalid token signature')
			}
		}
	}

	// Validate standard claims without JSON coercions or ambiguous duplicate fields.
	claim_values := decode_object(payload_json, 'claims')!
	for name in ['iss', 'sub', 'aud', 'exp', 'nbf', 'iat', 'jti'] {
		if value := claim_values[name] {
			if value is json2.Null {
				return error('invalid JWT claim: ${name} cannot be null')
			}
			if name in ['iss', 'sub', 'aud', 'jti'] && value !is string {
				return error('invalid JWT claim: ${name} must be a string')
			}
		}
	}
	std_claims := json2.decode[StandardClaims](payload_json, strict: true) or {
		return error('invalid JWT claims: ${err.msg()}')
	}
	if options.leeway < 0 {
		return error('JWT leeway must be nonnegative')
	}
	now := options.current_time or { time.now().unix() }

	if options.validate_exp {
		if exp := std_claims.exp {
			if now >= exp && u64(now) - u64(exp) >= u64(options.leeway) {
				return error('token has expired (exp: ${exp}, now: ${now})')
			}
		}
	}

	if options.validate_nbf {
		if nbf := std_claims.nbf {
			if now < nbf && u64(nbf) - u64(now) > u64(options.leeway) {
				return error('token not valid yet (nbf: ${nbf}, now: ${now})')
			}
		}
	}

	if expected_iss := options.expected_issuer {
		if iss := std_claims.iss {
			if iss != expected_iss {
				return error('token issuer mismatch: expected "${expected_iss}", got "${iss}"')
			}
		} else {
			return error('token missing required issuer claim ("iss")')
		}
	}

	if expected_aud := options.expected_audience {
		if aud := std_claims.aud {
			if aud != expected_aud {
				return error('token audience mismatch: expected "${expected_aud}", got "${aud}"')
			}
		} else {
			return error('token missing required audience claim ("aud")')
		}
	}

	if expected_sub := options.expected_subject {
		if sub := std_claims.sub {
			if sub != expected_sub {
				return error('token subject mismatch: expected "${expected_sub}", got "${sub}"')
			}
		} else {
			return error('token missing required subject claim ("sub")')
		}
	}

	return payload_json
}

// decode_raw parses and base64url-decodes the header and payload JSON strings
// without verifying the signature.
pub fn decode_raw(token string) !(Header, string) {
	parts := token.split('.')
	if parts.len != 3 {
		return error('invalid token format: expected 3 dot-separated parts')
	}
	header_bytes := decode_segment(parts[0])!
	header_json := header_bytes.bytestr()
	header_values := decode_object(header_json, 'header')!
	algorithm_value := header_values['alg'] or {
		return error('JWT header is missing required algorithm ("alg")')
	}
	if algorithm_value !is string {
		return error('JWT header algorithm ("alg") must be a string')
	}
	if 'crit' in header_values || 'b64' in header_values {
		return error('unsupported JWT critical or unencoded-payload header extension')
	}
	header := json2.decode[Header](header_json, strict: true) or {
		return error('invalid jwt header: ${err.msg()}')
	}
	payload_bytes := decode_segment(parts[1])!
	return header, payload_bytes.bytestr()
}

// decode decodes a token into `Token[T]` without verifying its signature.
pub fn decode[T](token string) !Token[T] {
	parts := token.split('.')
	if parts.len != 3 {
		return error('invalid token format: expected 3 dot-separated parts')
	}
	header, payload_json := decode_raw(token)!
	claims := json2.decode[T](payload_json, strict: true) or {
		return error('failed to decode claims into requested type: ${err.msg()}')
	}
	sig := decode_segment(parts[2])!
	return Token[T]{
		header:    header
		claims:    claims
		signature: sig
		raw:       token
	}
}

// decode_header parses the Header from a token string without verifying its signature.
pub fn decode_header(token string) !Header {
	header, _ := decode_raw(token)!
	return header
}

// decode_segment rejects padding, non-URL alphabet characters, and noncanonical unused bits.
fn decode_segment(encoded string) ![]u8 {
	if encoded.len == 0 || encoded.len % 4 == 1 {
		return error('invalid JWT base64url segment')
	}
	for ch in encoded {
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `-` || ch == `_`) {
			return error('invalid JWT base64url character')
		}
	}
	decoded := base64.url_decode(encoded)
	if base64.url_encode(decoded) != encoded {
		return error('noncanonical JWT base64url segment')
	}
	return decoded
}

// C binding not exposed by net.mbedtls for public keys.
fn C.mbedtls_pk_parse_public_key(ctx &C.mbedtls_pk_context, key &u8, keylen uint) i32

// decode_object also rejects duplicate top-level members, including escaped spellings of a key.
fn decode_object(source string, label string) !map[string]json2.Any {
	value := json2.decode[map[string]json2.Any](source, strict: true) or {
		return error('invalid JWT ${label}: ${err.msg()}')
	}
	mut keys := map[string]bool{}
	mut depth := 0
	mut i := 0
	for i < source.len {
		if source[i] in [`{`, `[`] {
			depth++
		} else if source[i] in [`}`, `]`] {
			depth--
		} else if source[i] == `"` {
			start := i
			i++
			for i < source.len && source[i] != `"` {
				if source[i] == `\\` {
					i++
				}
				i++
			}
			i++
			end := i
			for i < source.len && source[i] in [` `, `\t`, `\r`, `\n`] {
				i++
			}
			if depth == 1 && i < source.len && source[i] == `:` {
				key := json2.decode[string](source[start..end], strict: true)!
				if key in keys {
					return error('duplicate JWT ${label} member: ${key}')
				}
				keys[key] = true
			}
			continue
		}
		i++
	}
	return value
}

// rsa_sign_pkcs1v15_sha256 signs `data` with an RSA private key (PEM or DER)
// using PKCS#1 v1.5 padding and SHA-256 (RS256).
fn rsa_sign_pkcs1v15_sha256(key []u8, data []u8) ![]u8 {
	mut pk := C.mbedtls_pk_context{}
	C.mbedtls_pk_init(&pk)
	defer {
		C.mbedtls_pk_free(&pk)
	}

	mut entropy := C.mbedtls_entropy_context{}
	mut ctr := C.mbedtls_ctr_drbg_context{}
	C.mbedtls_entropy_init(&entropy)
	C.mbedtls_ctr_drbg_init(&ctr)
	defer {
		C.mbedtls_ctr_drbg_free(&ctr)
		C.mbedtls_entropy_free(&entropy)
	}
	seed_ret := C.mbedtls_ctr_drbg_seed(&ctr, C.mbedtls_entropy_func, &entropy, unsafe { nil }, 0)
	if seed_ret != 0 {
		return error('mbedtls_ctr_drbg_seed failed: ${seed_ret}')
	}

	mut keybuf := key.clone()
	if key.bytestr().contains('-----BEGIN') {
		keybuf << 0
	}
	parse_ret := C.mbedtls_pk_parse_key(&pk, unsafe { &keybuf[0] }, keybuf.len, unsafe { nil }, 0,
		C.mbedtls_ctr_drbg_random, &ctr)
	if parse_ret != 0 {
		return error('mbedtls_pk_parse_key failed: ${parse_ret}')
	}

	hash := sha256.sum(data)
	mut sig := []u8{len: 1024}
	mut sig_len := uint(0)
	ret := C.mbedtls_pk_sign_ext(1, &pk, 9, unsafe { &hash[0] }, hash.len, unsafe { &sig[0] }, sig.len,
		&sig_len, C.mbedtls_ctr_drbg_random, &ctr)
	if ret != 0 {
		return error('mbedtls_pk_sign_ext failed: ${ret}')
	}
	return sig[..int(sig_len)].clone()
}

// rsa_verify_pkcs1v15_sha256 verifies an RS256 signature with an RSA public
// key (PEM or DER).
fn rsa_verify_pkcs1v15_sha256(key []u8, data []u8, sig []u8) !bool {
	mut pk := C.mbedtls_pk_context{}
	C.mbedtls_pk_init(&pk)
	defer {
		C.mbedtls_pk_free(&pk)
	}

	mut keybuf := key.clone()
	if key.bytestr().contains('-----BEGIN') {
		keybuf << 0
	}
	parse_ret := C.mbedtls_pk_parse_public_key(&pk, unsafe { &keybuf[0] }, keybuf.len)
	if parse_ret != 0 {
		return error('mbedtls_pk_parse_public_key failed: ${parse_ret}')
	}

	hash := sha256.sum(data)
	ret := C.mbedtls_pk_verify(&pk, 9, unsafe { &hash[0] }, hash.len, unsafe { &sig[0] }, sig.len)
	return ret == 0
}

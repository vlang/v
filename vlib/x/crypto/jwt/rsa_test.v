module jwt

import crypto.rsa
import encoding.base64
import json2
import os

// RFC 7515 Appendix A.2: a JWS signed with RS256, and its key.
const rfc7515_token = 'eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu3XOiZj5RZmh7AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4BAynRFdiuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K0GarZRmB_eSN9383LcOLn6_dO--xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqvhJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5AXLIhWkWywlVmtVrBp0igcN_IoypGlUPQGe77Rw'
const rfc7515_n = 'ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddxHmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMsD1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSHSXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdVMTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ'
const rfc7515_e = 'AQAB'
const rfc7515_d = 'Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97IjlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvXt4ehNYTCBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLhBOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ'
const rfc7515_p = '4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH_5IB3jw3bcxGn6QLvnEtfdUdiYrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcmnPGBY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc'
const rfc7515_q = 'uQPEfgmVtjL0Uyyx88GZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxaewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62wDmpe_HlB-TnBA-njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc'
const rfc7515_dp = 'BwKfV3Akq5_MFZDFZCnW-wzl-CCo83WoZvnLQwCTeDv8uzluRSnm71I3QCLdhrqE2e9YkxvuxdBfpT_PI7Yz-FOKnu1R6HsJeDCjn12Sk3vmAktV2zb34MCdy7cpdTh_YVr7tss2u6vneTwrA86rZtu5Mbr1C1XsmvkxHQAdYo0'
const rfc7515_dq = 'h_96-mK1R_7glhsum81dZxjTnYynPbZpHziZjeeHcXYsXaaMwkOlODsWa7I9xXDoRwbKgB719rrmI2oKr6N3Do9U0ajaHF-NKJnwgjMd2w9cjz3_-kyNlxAr2v4IKhGNpmM5iIgOS1VZnOZ68m6_pbLBSp3nssTdlqvd0tIiTHU'
const rfc7515_qi = 'IYd7DHOhrWvxkwPQsRM2tOgrjbcrfvtQJipd-DlcxyVuuM9sQLdgjVk2oy26F0EmpScGLq2MowX7fhd_QJQ3ydy5cY7YIBi87w93IKLEdfnbJtoOPLUW0ITrJReOgo1cq9SbsxYawBgfp_gh6A5603k2-ZQwVK0JKSHuLFkuQ3U'

const rsa_algorithms = [Algorithm.rs256, .rs384, .rs512, .ps256, .ps384, .ps512]

struct RsaTestClaims {
	sub string
	aud []string
	exp i64 = 2000000000
}

fn b64(s string) []u8 {
	return base64.url_decode(s)
}

fn rfc7515_key() rsa.PrivateKey {
	return rsa.PrivateKey.new(
		n:    b64(rfc7515_n)
		e:    65537
		d:    b64(rfc7515_d)
		p:    b64(rfc7515_p)
		q:    b64(rfc7515_q)
		dp:   b64(rfc7515_dp)
		dq:   b64(rfc7515_dq)
		qinv: b64(rfc7515_qi)
	) or { panic(err) }
}

fn fixture_key() rsa.PrivateKey {
	path := os.join_path(@VEXEROOT, 'vlib', 'crypto', 'rsa', 'testdata', 'key2048.pem')
	return rsa.PrivateKey.from_pem(os.read_file(path) or { panic(err) }) or { panic(err) }
}

fn test_rfc7515_rs256_example() {
	key := rfc7515_key()
	parts := rfc7515_token.split('.')
	// RS256 is deterministic: signing the example input gives the example signature.
	sig := rsa_sign(.rs256, key.to_der(), '${parts[0]}.${parts[1]}'.bytes())!
	assert base64.url_encode(sig) == parts[2]
	public_der := key.public_key().to_der()
	payload := verify_raw(rfc7515_token, public_der, algorithm: .rs256, validate_exp: false)!
	assert payload.contains('"iss":"joe"')
	// The example expired in 2011.
	if _ := verify_raw(rfc7515_token, public_der, algorithm: .rs256) {
		assert false, 'accepted an expired token'
	}
	// The key from its JWK verifies too.
	jwk := Jwk{
		kty: 'RSA'
		n:   rfc7515_n
		e:   rfc7515_e
	}
	assert jwk.verification_key()! == public_der
}

fn test_rsa_algorithms_round_trip() {
	key := fixture_key()
	private_der := key.to_der()
	public_der := key.public_key().to_der()
	pkcs1_der := key.public_key().to_pkcs1_der()
	claims := RsaTestClaims{
		sub: 'user-1'
		aud: ['account', 'api']
		exp: 2000000000
	}
	for alg in rsa_algorithms {
		token := sign(claims, private_der, algorithm: alg, kid: 'k1')!
		header := decode_header(token)!
		assert header.alg == alg.str()
		assert header.kid or { '' } == 'k1'
		verified := verify[RsaTestClaims](token, public_der,
			algorithm:         alg
			expected_audience: 'api'
		)!
		assert verified.claims == claims
		verify[RsaTestClaims](token, pkcs1_der, algorithm: alg)!
		// Signatures are as long as the modulus.
		assert verified.signature.len == 256
		// A PKCS #1 private key signs as well.
		sign(claims, key.to_pkcs1_der(), algorithm: alg)!
	}
	// A parsed key signs the same tokens.
	for alg in rsa_algorithms {
		token := sign_with_rsa_key(claims, key, algorithm: alg, kid: 'k1')!
		verify[RsaTestClaims](token, public_der, algorithm: alg)!
	}
	assert sign_with_rsa_key(claims, key, algorithm: .rs256)! == sign(claims, private_der,
		algorithm: .rs256
	)!
	if _ := sign_with_rsa_key(claims, key, algorithm: .hs256) {
		assert false, 'signed HS256 with an RSA key'
	}
	// RS256 is deterministic, PS256 is randomized.
	assert sign(claims, private_der, algorithm: .rs256)! == sign(claims, private_der,
		algorithm: .rs256
	)!
	assert sign(claims, private_der, algorithm: .ps256)! != sign(claims, private_der,
		algorithm: .ps256
	)!
}

fn test_ps256_uses_a_salt_as_long_as_the_digest() {
	key := fixture_key()
	token := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .ps256)!
	parts := token.split('.')
	digest := rsa_digest(.ps256, '${parts[0]}.${parts[1]}'.bytes())
	sig := base64.url_decode(parts[2])
	assert key.public_key().verify_pss(.sha256, digest, sig, salt_length: 32)!
	assert !key.public_key().verify_pss(.sha256, digest, sig, salt_length: 0)!
}

fn test_rsa_algorithm_confusion_is_rejected() {
	key := fixture_key()
	public_der := key.public_key().to_der()
	token := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .rs256)!
	for alg in [Algorithm.ps256, .rs384, .hs256, .eddsa] {
		if _ := verify_raw(token, public_der, algorithm: alg) {
			assert false, 'accepted an RS256 token as ${alg}'
		}
	}
	// A token whose header claims HS256, MACed with the public key bytes, is
	// refused when the trusted algorithm is RS256.
	forged := sign_raw(Header{ alg: 'HS256' }, '{"sub":"admin"}', public_der)!
	if _ := verify_raw(forged, public_der, algorithm: .rs256) {
		assert false, 'accepted an HS256 token signed with the public key'
	}
	// A tampered payload breaks the signature.
	parts := token.split('.')
	tampered := '${parts[0]}.${base64.url_encode('{"sub":"admin"}'.bytes())}.${parts[2]}'
	if _ := verify_raw(tampered, public_der, algorithm: .rs256) {
		assert false, 'accepted a tampered token'
	}
	// Keys of the wrong kind are refused with an error, not a panic.
	if _ := sign(RsaTestClaims{}, public_der, algorithm: .rs256) {
		assert false, 'signed with a public key'
	}
	if _ := verify_raw(token, 'not a key'.bytes(), algorithm: .rs256) {
		assert false, 'verified with garbage as key'
	}
}

fn test_jwks_round_trip_and_key_selection() {
	key := fixture_key()
	other := rfc7515_key()
	jwks := Jwks{
		keys: [
			Jwk.from_rsa_public_key(other.public_key(), 'old', .rs256),
			Jwk.from_rsa_public_key(key.public_key(), 'current', .ps256),
		]
	}
	parsed := Jwks.parse(jwks.encode())!
	assert parsed == jwks
	assert parsed.keys[1].e or { '' } == 'AQAB'
	token := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .ps256, kid: 'current')!
	verified := verify_with_jwks[RsaTestClaims](token, parsed, algorithm: .ps256)!
	assert verified.claims.sub == 'x'
	// The key published for PS256 does not verify RS256 tokens.
	rs := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .rs256, kid: 'current')!
	if _ := verify_with_jwks[RsaTestClaims](rs, parsed, algorithm: .rs256) {
		assert false, 'used a PS256 key for RS256'
	}
	// A token signed by one key, but naming another, fails.
	wrong := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .rs256, kid: 'old')!
	if _ := verify_with_jwks[RsaTestClaims](wrong, parsed, algorithm: .rs256) {
		assert false, 'accepted a token signed by another key'
	}
	for kid in ['missing', ''] {
		t := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .ps256, kid: kid)!
		if _ := verify_with_jwks[RsaTestClaims](t, parsed, algorithm: .ps256) {
			assert false, 'accepted an unknown kid'
		}
	}
	// Without a kid, the only key that fits the algorithm is used...
	no_kid := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .ps256)!
	verify_with_jwks[RsaTestClaims](no_kid, parsed, algorithm: .ps256)!
	// ...and two keys that fit are ambiguous.
	ambiguous := Jwks{
		keys: [
			Jwk.from_rsa_public_key(key.public_key(), 'a', .ps256),
			Jwk.from_rsa_public_key(other.public_key(), 'b', .ps256),
		]
	}
	if _ := verify_with_jwks[RsaTestClaims](no_kid, ambiguous, algorithm: .ps256) {
		assert false, 'picked one of two keys without a kid'
	}
	single := Jwks{
		keys: [Jwk.from_rsa_public_key(key.public_key(), 'only', .ps256)]
	}
	verify_with_jwks[RsaTestClaims](no_kid, single, algorithm: .ps256)!
	// An HMAC algorithm can never take its key from a JWKS.
	if _ := verify_with_jwks[RsaTestClaims](no_kid, single, algorithm: .hs256) {
		assert false, 'HS256 with a JWKS key'
	}
}

// A JWKS as Keycloak publishes it: certificate members, an encryption key, and
// key ids, which are ignored or used to select the key.
fn test_keycloak_style_jwks() {
	key := fixture_key()
	n := base64.url_encode(key.public_key().modulus())
	data := '{"keys":[{"kid":"enc-1","kty":"RSA","alg":"RSA-OAEP","use":"enc","n":"${n}","e":"AQAB","x5c":["MIIC"],"x5t":"abc","x5t#S256":"def"},{"kid":"sig-1","kty":"RSA","alg":"RS256","use":"sig","n":"${n}","e":"AQAB","x5c":["MIIC"],"x5t":"abc","x5t#S256":"def"}]}'
	jwks := Jwks.parse(data)!
	assert jwks.keys.len == 2
	token := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .rs256, kid: 'sig-1')!
	verify_with_jwks[RsaTestClaims](token, jwks, algorithm: .rs256)!
	// The encryption key is not used for signatures.
	enc := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .rs256, kid: 'enc-1')!
	if _ := verify_with_jwks[RsaTestClaims](enc, jwks, algorithm: .rs256) {
		assert false, 'used an encryption key'
	}
	// Without a kid, the signature key is the only one that fits RS256.
	no_kid := sign(RsaTestClaims{ sub: 'x' }, key.to_der(), algorithm: .rs256)!
	verify_with_jwks[RsaTestClaims](no_kid, jwks, algorithm: .rs256)!
	// Keys of different uses may share a kid (RFC 7517 §4.5).
	shared := Jwks.parse(data.replace('"enc-1"', '"sig-1"'))!
	verify_with_jwks[RsaTestClaims](token, shared, algorithm: .rs256)!
	assert shared.key('sig-1', .rs256)!.use or { '' } == 'sig'
	// Two keys that both fit are ambiguous.
	twice := Jwks.parse(data.replace('"enc-1"', '"sig-1"').replace('"RSA-OAEP","use":"enc"',
		'"RS256","use":"sig"'))!
	if _ := verify_with_jwks[RsaTestClaims](token, twice, algorithm: .rs256) {
		assert false, 'picked one of two matching keys'
	}
	ec := Jwk{
		kty: 'EC'
		crv: 'P-256'
	}
	if _ := ec.verification_key() {
		assert false, 'EC keys are not supported'
	}
	short := Jwk{
		kty: 'RSA'
		n:   base64.url_encode(key.public_key().modulus()[..128])
		e:   'AQAB'
	}
	if _ := short.verification_key() {
		assert false, 'accepted a 1024-bit key'
	}
	_ := json2.encode(jwks)
}

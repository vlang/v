// Tests for the http.Request / http.Response wrappers. The
// roundtrip cases exercise the full sign-then-verify pipeline through
// the public API; the negative paths cover the rejection branches
// callers depend on for security.
module signature

import crypto.ecdsa
import crypto.ed25519
import encoding.base64
import net.http

const test_secret = 'shh-this-is-a-secret-shared-with-the-server'

fn build_request(url string) http.Request {
	mut req := http.Request{
		method: .post
		url:    url
	}
	req.header.add_custom('Date', 'Tue, 20 Apr 2021 02:07:55 GMT') or {}
	req.header.add_custom('Content-Type', 'application/json') or {}
	req.header.add_custom('Host', 'example.com') or {}
	return req
}

fn test_sign_and_verify_request_hmac_roundtrip() {
	mut req := build_request('https://example.com/foo?bar=1')
	key := Key.hmac_sha256(test_secret.bytes()).with_keyid('shared-key')
	sign_request(mut req, key,
		components: ['@method', '@target-uri', '@authority', 'date', 'content-type']
		created:    1618884473
	)!
	verify_request(req, key)!
}

fn test_sign_and_verify_request_ed25519_roundtrip() {
	seed := []u8{len: 32, init: u8(index)}
	priv_obj := ed25519.new_key_from_seed(seed)
	pub_bytes := []u8(priv_obj.public_key())

	priv_key := Key.ed25519_private(seed).with_keyid('alice-ed25519')
	pub_key := Key.ed25519_public(pub_bytes)

	mut req := build_request('https://example.com/foo')
	sign_request(mut req, priv_key,
		components: ['@method', '@target-uri', 'date']
		created:    1618884473
	)!
	verify_request(req, pub_key)!
}

fn test_sign_and_verify_request_ecdsa_p256_roundtrip() {
	x, y, d := p256_test_key()
	priv_key := Key.ecdsa_p256_private(x, y, d).with_keyid('p256-key')
	pub_key := Key.ecdsa_p256_public(x, y)
	mut req := build_request('https://example.com/api?id=42')
	sign_request(mut req, priv_key,
		components: ['@method', '@target-uri', '@path', '@query', 'content-type']
		created:    1618884473
	)!
	verify_request(req, pub_key)!
}

fn test_sign_and_verify_request_ecdsa_p384_roundtrip() {
	// P-384 has no RFC 9421 vector; generate a fresh keypair via the
	// V ecdsa module so the test is self-contained.
	pub_obj, priv_obj := ecdsa.generate_key(nid: .secp384r1)!
	defer {
		priv_obj.free()
		pub_obj.free()
	}
	pub_bytes := pub_obj.bytes()!
	// pub_bytes is the SEC1 uncompressed point: 0x04 || x || y. The
	// public-key constructor wants raw (x, y) — strip the prefix.
	x := pub_bytes[1..49]
	y := pub_bytes[49..97]
	d := priv_obj.bytes()!
	priv_key := Key.ecdsa_p384_private(x, y, d).with_keyid('p384-key')
	pub_key := Key.ecdsa_p384_public(x, y)
	mut req := build_request('https://example.com/foo')
	sign_request(mut req, priv_key,
		components: ['@method', '@target-uri']
		created:    1618884473
	)!
	verify_request(req, pub_key)!
}

fn test_verify_request_rejects_wrong_key() {
	mut req := build_request('https://example.com/foo')
	good := Key.hmac_sha256('secret-A'.bytes())
	bad := Key.hmac_sha256('secret-B'.bytes())
	sign_request(mut req, good, components: ['@method', '@target-uri'], created: 1)!
	if _ := verify_request(req, bad) {
		assert false, 'wrong key must not verify'
	} else {
		assert err is VerificationFailed
	}
}

fn test_verify_request_rejects_tampered_target_uri() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())
	sign_request(mut req, key, components: ['@method', '@target-uri'], created: 1)!
	// Mutate the URL after signing - the verifier rebuilds the
	// signature base from the (now-tampered) request and must fail.
	req.url = 'https://example.com/bar'
	if _ := verify_request(req, key) {
		assert false, 'tampered @target-uri must not verify'
	} else {
		assert err is VerificationFailed
	}
}

fn test_verify_request_rejects_missing_signature_header() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())
	sign_request(mut req, key, components: ['@method'], created: 1)!
	req.header.delete_custom('Signature')
	if _ := verify_request(req, key) {
		assert false, 'missing Signature header must error'
	} else {
		assert err is MalformedMessage
		assert err.msg().contains('no Signature header')
	}
}

fn test_verify_request_rejects_expired_signature() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())
	sign_request(mut req, key,
		components: ['@method']
		created:    1000
		expires:    2000
	)!
	if _ := verify_request(req, key, now_unix: 5000) {
		assert false, 'expired signature must be rejected when now_unix > expires'
	} else {
		assert err is SignatureExpired
	}
	// Unchecked when now_unix is left at the default zero.
	verify_request(req, key)!
}

fn test_sign_two_signatures_coexist() {
	mut req := build_request('https://example.com/foo')
	k1 := Key.hmac_sha256('one'.bytes())
	k2 := Key.hmac_sha256('two'.bytes())
	sign_request(mut req, k1, components: ['@method'], label: 'sig-a', created: 1)!
	sign_request(mut req, k2, components: ['@target-uri'], label: 'sig-b', created: 2)!
	verify_request(req, k1, label: 'sig-a')!
	verify_request(req, k2, label: 'sig-b')!
	if _ := verify_request(req, k1, label: 'sig-b') {
		assert false, 'sig-b must not verify under k1'
	} else {
		assert err is VerificationFailed
	}
}

fn test_sign_response_and_verify() {
	mut resp := http.Response{
		status_code: 200
	}
	resp.header.add_custom('Content-Type', 'application/json')!
	resp.header.add_custom('Content-Length', '23')!
	key := Key.hmac_sha256(test_secret.bytes())
	sign_response(mut resp, key,
		components: ['@status', 'content-type', 'content-length']
		created:    1
	)!
	verify_response(resp, key)!
}

fn test_alg_param_must_match_key_algorithm() {
	x, y, d := p256_test_key()
	priv := Key.ecdsa_p256_private(x, y, d)
	c := Components{
		method:     'GET'
		target_uri: 'https://example.com/'
	}
	p := SignatureParams{
		components: ['@method']
		alg:        'ed25519'
	}
	if _ := sign(c, p, priv, 'sig1') {
		assert false, 'alg mismatch must fail'
	} else {
		assert err is MalformedMessage
	}
}

fn test_label_validation_rejects_empty_and_uppercase() {
	p := SignatureParams{
		components: ['@method']
	}
	key := Key.hmac_sha256('k'.bytes())
	c := Components{
		method: 'GET'
	}
	if _ := sign(c, p, key, '') {
		assert false, 'empty label must fail'
	} else {
		assert err is MalformedMessage
	}
	if _ := sign(c, p, key, 'Sig1') {
		assert false, 'uppercase label must fail (Structured Field key grammar)'
	} else {
		assert err is MalformedMessage
	}
}

// p256_test_key returns the (x, y, d) coordinates from RFC 9421
// Appendix B.1.3.
fn p256_test_key() ([]u8, []u8, []u8) {
	x := pad_b64u('qIVYZVLCrPZHGHjP17CTW0_-D9Lfw0EkjqF7xB4FivA', 32)
	y := pad_b64u('Mc4nN9LTDOBhfoUeg8Ye9WedFRhnZXZJA12Qp0zZ6F0', 32)
	d := pad_b64u('UpuF81l-kOxbjf7T4mNSv0r5tN67Gim7rnf6EFpcYDs', 32)
	return x, y, d
}

fn pad_b64u(s string, want int) []u8 {
	mut padded := s
	for padded.len % 4 != 0 {
		padded += '='
	}
	mut b := base64.url_decode(padded)
	for b.len < want {
		b.prepend(u8(0))
	}
	return b
}

// vtest build: present_openssl? && !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
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
	priv_key := Key.ecdsa_p256_private(x, y, d)!.with_keyid('p256-key')
	pub_key := Key.ecdsa_p256_public(x, y)!
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
	priv_key := Key.ecdsa_p384_private(x, y, d)!.with_keyid('p384-key')
	pub_key := Key.ecdsa_p384_public(x, y)!
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

fn test_origin_form_request_target_uri_reconstructed() {
	// Inbound HTTP/1.1 requests parsed by `http.parse_request*` carry
	// the request target verbatim (`/foo?bar=1`), not an absolute URI.
	// `request_components` must rebuild `<scheme>://<authority><url>`
	// so the verifier sees the same `@target-uri` as a peer that signs
	// with an absolute URL.
	mut signing_req := http.Request{
		method: .post
		url:    'https://example.com/foo?bar=1'
	}
	signing_req.header.add_custom('Host', 'example.com')!
	key := Key.hmac_sha256(test_secret.bytes())
	sign_request(mut signing_req, key,
		components: ['@method', '@target-uri']
		created:    1
	)!

	// Receiver side: same request as it would land after parsing.
	mut received := http.Request{
		method: .post
		url:    '/foo?bar=1'
		host:   'example.com'
	}
	for k in signing_req.header.keys() {
		for v in signing_req.header.custom_values(k) {
			received.header.add_custom(k, v)!
		}
	}
	verify_request(received, key)!
}

fn test_origin_form_uses_explicit_scheme() {
	// HTTP-only deployment: caller must pass `scheme: 'http'` so both
	// sides agree on the reconstructed target URI.
	mut signing_req := http.Request{
		method: .get
		url:    'http://api.example.com/v1/items'
	}
	signing_req.header.add_custom('Host', 'api.example.com')!
	key := Key.hmac_sha256(test_secret.bytes())
	sign_request(mut signing_req, key,
		components: ['@method', '@target-uri']
		created:    1
		scheme:     'http'
	)!

	mut received := http.Request{
		method: .get
		url:    '/v1/items'
		host:   'api.example.com'
	}
	for k in signing_req.header.keys() {
		for v in signing_req.header.custom_values(k) {
			received.header.add_custom(k, v)!
		}
	}
	verify_request(received, key, scheme: 'http')!
}

fn test_request_components_preserve_escaped_path() {
	req := build_request('https://example.com/files/a%2Fb?download=1')
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('@path')! == '/files/a%2Fb'
	assert c.component_value('@request-target')! == '/files/a%2Fb?download=1'
}

fn test_request_components_normalize_empty_absolute_path() {
	req := build_request('https://example.com')
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('@target-uri')! == 'https://example.com/'
	assert c.component_value('@request-target')! == '/'
}

fn test_outgoing_request_components_match_transmitted_query() {
	req := build_request('https://example.com/search?q=a%20b&flag=&bare')
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('@target-uri')! == 'https://example.com/search?q=a+b&flag&bare'
	assert c.component_value('@request-target')! == '/search?q=a+b&flag&bare'
	assert c.component_value('@query')! == '?q=a+b&flag&bare'
}

fn test_incoming_request_components_preserve_absolute_form() {
	req := build_request('http://example.com/search?q=a+b')
	c := request_components(req, 'http', .incoming)!
	assert c.component_value('@request-target')! == 'http://example.com/search?q=a+b'
}

fn test_outgoing_proxy_request_components_use_absolute_form() {
	proxy := http.new_http_proxy('http://localhost:8080')!
	mut req := build_request('http://example.com/search?q=a%20b')
	req.proxy = proxy
	c := request_components(req, 'http', .outgoing)!
	assert c.component_value('@request-target')! == 'http://example.com/search?q=a+b'
}

fn test_outgoing_request_components_use_host_override() {
	mut req := build_request('https://origin.example/path')
	req.header.set(.host, 'virtual.example')
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('@authority')! == 'virtual.example'
	assert c.component_value('@target-uri')! == 'https://virtual.example/path'
}

fn test_outgoing_request_components_match_repeated_header_serialization() {
	mut req := build_request('https://example.com/')
	req.header.add_custom('Accept', 'text/html')!
	req.header.add_custom('Accept', 'application/json')!
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('accept')! == 'text/html, application/json'
}

fn test_outgoing_request_components_include_generated_fields() {
	mut req := http.Request{
		method:     .post
		url:        'https://example.com/upload'
		data:       'body'
		user_agent: 'signature-test'
	}
	req.add_cookie(http.Cookie{
		name:  'sid'
		value: 'abc'
	})
	req.header.add_custom('cookie', 'theme=dark')!
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('host')! == 'example.com'
	assert c.component_value('user-agent')! == 'signature-test'
	assert c.component_value('content-length')! == '4'
	assert c.component_value('cookie')! == 'sid=abc; theme=dark'
}

fn test_sign_request_rejects_control_bytes_in_parameters() {
	mut req := build_request('https://example.com/')
	key := Key.hmac_sha256(test_secret.bytes())
	if _ := sign_request(mut req, key,
		components: ['@method']
		keyid:      'safe\r\nInjected: true'
	)
	{
		assert false, 'control bytes in signature parameters must be rejected'
	} else {
		assert err is MalformedMessage
	}
	assert !req.header.contains_custom('Signature-Input')
}

fn test_append_dict_header_preserves_all_existing_field_lines() {
	mut header := http.Header{}
	header.add_custom('Signature-Input', 'sig-a=("@method")')!
	header.add_custom('Signature-Input', 'sig-b=("@path")')!
	append_dict_header(mut header, 'Signature-Input', 'sig-c=("@authority")')!
	assert header.custom_values('Signature-Input') == [
		'sig-a=("@method"), sig-b=("@path"), sig-c=("@authority")',
	]
	response := http.Response{
		status_code:  200
		status_msg:   'OK'
		http_version: '1.1'
		header:       header
	}
	wire := response.bytestr()
	assert wire.count('Signature-Input:') == 1
	assert !wire.contains('Signature-Input: \r\n')
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

fn test_sign_response_normalizes_zero_status_to_wire_ok() {
	mut resp := http.Response{}
	key := Key.hmac_sha256(test_secret.bytes())
	sign_response(mut resp, key, components: ['@status'], created: 1)!
	received := http.Response{
		...resp
		status_code: 200
	}
	verify_response(received, key)!
}

fn test_alg_param_must_match_key_algorithm() {
	x, y, d := p256_test_key()
	priv := Key.ecdsa_p256_private(x, y, d)!
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

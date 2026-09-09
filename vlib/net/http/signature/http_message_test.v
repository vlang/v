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
	key := Key.hmac_sha256(test_secret.bytes())!.with_keyid('shared-key')
	sign_request(mut req, key,
		components: ['@method', '@target-uri', '@authority', 'date', 'content-type']
		created:    1618884473
	)!
	assert !req.allow_redirect
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
	verify_request(req, pub_key, required_components: ['@method', '@target-uri'])!
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
	verify_request(req, pub_key,
		required_components: ['@method', '@target-uri', '@path', '@query', 'content-type']
	)!
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
	verify_request(req, pub_key, required_components: ['@method', '@target-uri'])!
}

fn test_verify_request_rejects_wrong_key() {
	mut req := build_request('https://example.com/foo')
	good := Key.hmac_sha256('secret-A'.bytes())!
	bad := Key.hmac_sha256('secret-B'.bytes())!
	sign_request(mut req, good, components: ['@method', '@target-uri'], created: 1)!
	if _ := verify_request(req, bad, required_components: ['@method', '@target-uri']) {
		assert false, 'wrong key must not verify'
	} else {
		assert err is VerificationFailed
	}
}

fn test_verify_request_rejects_tampered_target_uri() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_request(mut req, key, components: ['@method', '@target-uri'], created: 1)!
	// Mutate the URL after signing - the verifier rebuilds the
	// signature base from the (now-tampered) request and must fail.
	req.url = 'https://example.com/bar'
	if _ := verify_request(req, key, required_components: ['@method', '@target-uri']) {
		assert false, 'tampered @target-uri must not verify'
	} else {
		assert err is VerificationFailed
	}
}

fn test_verify_request_rejects_missing_signature_header() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())!
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
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_request(mut req, key,
		components: ['@method']
		created:    1000
		expires:    2000
	)!
	if _ := verify_request(req, key, now_unix: 5000, required_components: ['@method']) {
		assert false, 'expired signature must be rejected when now_unix > expires'
	} else {
		assert err is SignatureExpired
	}
	// Unchecked when now_unix is left at the default zero.
	verify_request(req, key, required_components: ['@method'])!
}

fn test_verify_request_rejects_signature_created_in_future() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_request(mut req, key, components: ['@method'], created: 2000)!
	if _ := verify_request(req, key, now_unix: 1000, required_components: ['@method']) {
		assert false, 'a signature created after now_unix must be rejected'
	} else {
		assert err is SignatureNotYetValid
	}
	// Time validation remains opt-in.
	verify_request(req, key, required_components: ['@method'])!
}

fn test_verify_request_requires_safe_default_component_coverage() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_request(mut req, key, components: ['date'], created: 1)!
	if _ := verify_request(req, key) {
		assert false, 'request verification must require method, target URI, and authority by default'
	} else {
		assert err is MalformedMessage
		assert err.msg().contains('@method')
	}
	// Applications with a different authorization profile can opt into its
	// required component set explicitly.
	verify_request(req, key, required_components: ['date'])!
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
	key := Key.hmac_sha256(test_secret.bytes())!
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
	verify_request(received, key, required_components: ['@method', '@target-uri'])!
}

fn test_origin_form_uses_explicit_scheme() {
	// HTTP-only deployment: caller must pass `scheme: 'http'` so both
	// sides agree on the reconstructed target URI.
	mut signing_req := http.Request{
		method: .get
		url:    'http://api.example.com/v1/items'
	}
	signing_req.header.add_custom('Host', 'api.example.com')!
	key := Key.hmac_sha256(test_secret.bytes())!
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
	verify_request(received, key,
		scheme:              'http'
		required_components: ['@method', '@target-uri']
	)!
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

fn test_outgoing_request_components_trim_explicit_host_for_derived_values() {
	mut req := build_request('https://url.example/path')
	req.enable_http2 = false
	req.header.set(.host, ' example.com ')
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('@authority')! == 'example.com'
	assert c.component_value('@target-uri')! == 'https://example.com/path'
}

fn test_incoming_request_components_trim_host_for_derived_values() {
	req := http.parse_request_str('GET /path HTTP/1.1\r\nHost: example.com \r\n\r\n')!
	c := request_components(req, 'https', .incoming)!
	assert c.component_value('@authority')! == 'example.com'
	assert c.component_value('@target-uri')! == 'https://example.com/path'
}

fn test_outgoing_request_components_preserve_ipv6_authority_brackets() {
	req := http.Request{
		method: .get
		url:    'https://[2001:db8::1]/path'
	}
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('@authority')! == '[2001:db8::1]'
	assert c.component_value('@target-uri')! == 'https://[2001:db8::1]/path'

	req_nondefault := http.Request{
		method: .get
		url:    'https://[2001:db8::1]:8443/path'
	}
	c_nondefault := request_components(req_nondefault, 'https', .outgoing)!
	assert c_nondefault.component_value('@authority')! == '[2001:db8::1]:8443'
	assert c_nondefault.component_value('@target-uri')! == 'https://[2001:db8::1]:8443/path'
}

fn test_incoming_request_components_preserve_absolute_form() {
	req := build_request('http://example.com/search?q=a+b')
	c := request_components(req, 'http', .incoming)!
	assert c.component_value('@request-target')! == 'http://example.com/search?q=a+b'
}

fn test_incoming_connect_reconstructs_target_uri_from_authority_form() {
	req :=
		http.parse_request_str('CONNECT tunnel.example:443 HTTP/1.1\r\nHost: ignored.example\r\n\r\n')!
	c := request_components(req, 'https', .incoming)!
	assert c.component_value('@target-uri')! == 'https://tunnel.example:443'
	assert c.component_value('@authority')! == 'tunnel.example'
	assert c.component_value('@path')! == '/'
	assert c.component_value('@request-target')! == 'tunnel.example:443'
}

fn test_incoming_options_asterisk_reconstructs_target_uri_without_path() {
	req := http.parse_request_str('OPTIONS * HTTP/1.1\r\nHost: example.com:8443\r\n\r\n')!
	c := request_components(req, 'https', .incoming)!
	assert c.component_value('@target-uri')! == 'https://example.com:8443'
	assert c.component_value('@authority')! == 'example.com:8443'
	assert c.component_value('@path')! == '/'
	assert c.component_value('@request-target')! == '*'
}

fn test_outgoing_proxy_request_components_use_absolute_form() {
	proxy := http.new_http_proxy('http://localhost:8080')!
	mut req := build_request('http://example.com/search?q=a%20b')
	req.proxy = proxy
	c := request_components(req, 'http', .outgoing)!
	assert c.component_value('@request-target')! == 'http://example.com/search?q=a+b'
}

fn test_outgoing_proxy_request_components_preserve_url_authority() {
	proxy := http.new_http_proxy('http://localhost:8080')!
	mut req := build_request('http://origin.example:80/search')
	req.proxy = proxy
	req.header.set(.host, 'virtual.example')
	c := request_components(req, 'http', .outgoing)!
	assert c.component_value('@authority')! == 'origin.example'
	assert c.component_value('@target-uri')! == 'http://origin.example/search'
	assert c.component_value('@request-target')! == 'http://origin.example/search'
	assert c.component_value('host')! == 'virtual.example'
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

fn test_outgoing_request_components_deduplicate_header_name_casing() {
	mut req := build_request('https://example.com/')
	req.header.add_custom('X-Foo', 'a')!
	req.header.add_custom('x-foo', 'b')!
	c := request_components(req, 'https', .outgoing)!
	assert c.component_value('x-foo')! == 'a, b'
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

fn test_incoming_http2_request_components_semicolon_combine_cookie_fields() {
	mut req := http.Request{
		method:  .get
		url:     '/'
		host:    'example.com'
		version: .v2_0
	}
	req.header.add_custom('cookie', 'a=1')!
	req.header.add_custom('cookie', 'b=2')!
	c := request_components(req, 'https', .incoming)!
	assert c.component_value('cookie')! == 'a=1; b=2'
}

fn test_incoming_http2_request_components_exclude_synthesized_host() {
	mut req := http.Request{
		method:  .get
		url:     '/'
		host:    'example.com'
		version: .v2_0
	}
	// H2ServerConn adds this compatibility field from :authority even though
	// no Host field was present in the HTTP/2 message.
	req.header.set(.host, 'example.com')
	c := request_components(req, 'https', .incoming)!
	assert c.component_value('@authority')! == 'example.com'
	if _ := c.component_value('host') {
		assert false, 'a synthesized HTTP/2 Host field must not be coverable'
	} else {
		assert err is MalformedMessage
	}
}

fn test_sign_request_rejects_control_bytes_in_parameters() {
	mut req := build_request('https://example.com/')
	key := Key.hmac_sha256(test_secret.bytes())!
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
	k1 := Key.hmac_sha256('one'.bytes())!
	k2 := Key.hmac_sha256('two'.bytes())!
	sign_request(mut req, k1, components: ['@method'], label: 'sig-a', created: 1)!
	sign_request(mut req, k2, components: ['@target-uri'], label: 'sig-b', created: 2)!
	verify_request(req, k1, label: 'sig-a', required_components: ['@method'])!
	verify_request(req, k2, label: 'sig-b', required_components: ['@target-uri'])!
	if _ := verify_request(req, k1,
		label:               'sig-b'
		required_components: [
			'@target-uri',
		]
	)
	{
		assert false, 'sig-b must not verify under k1'
	} else {
		assert err is VerificationFailed
	}
}

fn test_sign_request_rejects_existing_label_without_mutation() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_request(mut req, key, components: ['@method'], created: 1)!
	input_before := req.header.custom_values('Signature-Input')
	signature_before := req.header.custom_values('Signature')
	if _ := sign_request(mut req, key, components: ['@path'], created: 2) {
		assert false, 'an existing signature label must not be appended again'
	} else {
		assert err is MalformedMessage
	}
	assert req.header.custom_values('Signature-Input') == input_before
	assert req.header.custom_values('Signature') == signature_before
}

fn test_sign_request_rejects_empty_existing_signature_input() {
	mut req := build_request('https://example.com/foo')
	req.header.add_custom('Signature-Input', '')!
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_request(mut req, key, components: ['@method'], created: 1) {
		assert false, 'an empty existing Signature-Input dictionary must be rejected'
	} else {
		assert err is MalformedMessage
	}
	assert req.header.custom_values('Signature-Input') == ['']
	assert !req.header.contains_custom('Signature')
}

fn test_sign_request_rejects_http2_removed_covered_field() {
	mut req := build_request('https://example.com/foo')
	req.header.set(.connection, 'keep-alive')
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_request(mut req, key, components: ['connection'], created: 1) {
		assert false, 'fields removed by possible HTTP/2 negotiation cannot be covered'
	} else {
		assert err is MalformedMessage
	}
	assert !req.header.contains_custom('Signature')
}

fn test_sign_request_rejects_http2_replaced_host_field() {
	mut req := build_request('https://example.com/foo')
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_request(mut req, key, components: ['host'], created: 1) {
		assert false, 'Host is replaced by @authority during possible HTTP/2 negotiation'
	} else {
		assert err is MalformedMessage
	}
	assert !req.header.contains_custom('Signature')
}

fn test_sign_request_preflights_both_signature_header_slots() {
	mut req := http.Request{
		method: .get
		url:    'https://example.com/'
	}
	for i in 0 .. http.max_headers - 1 {
		req.header.add_custom('X-Fill-${i}', 'value')!
	}
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_request(mut req, key, components: ['@method'], created: 1) {
		assert false, 'signing must reject insufficient capacity before changing either header'
	} else {
		assert err is MalformedMessage
	}
	assert !req.header.contains_custom('Signature-Input')
	assert !req.header.contains_custom('Signature')
}

fn test_sign_request_rejects_generated_connection_close_coverage() {
	mut req := build_request('http://example.com/foo')
	req.disable_connection_reuse = true
	req.header.set(.connection, 'keep-alive')
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_request(mut req, key, components: ['connection'], created: 1) {
		assert false, 'a transport-generated Connection: close value cannot be omitted from coverage'
	} else {
		assert err is MalformedMessage
	}
	assert !req.header.contains_custom('Signature')
}

fn test_sign_request_rejects_http2_filtered_te_value() {
	mut req := build_request('https://example.com/foo')
	req.header.add_custom('TE', 'gzip')!
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_request(mut req, key, components: ['te'], created: 1) {
		assert false, 'TE values filtered during possible HTTP/2 negotiation cannot be covered'
	} else {
		assert err is MalformedMessage
	}
	assert !req.header.contains_custom('Signature')
}

fn test_sign_request_allows_http2_te_trailers() {
	mut req := build_request('https://example.com/foo')
	req.header.add_custom('TE', 'trailers')!
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_request(mut req, key, components: ['te'], created: 1)!
	verify_request(req, key, required_components: ['te'])!
}

fn test_sign_request_rejects_self_referential_signature_field() {
	mut req := build_request('https://example.com/foo')
	req.header.add_custom('Signature-Input', 'old=()')!
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_request(mut req, key,
		components: ['signature-input']
		label:      'new'
		created:    1
	)
	{
		assert false, 'a field changed by signing cannot be covered'
	} else {
		assert err is MalformedMessage
	}
	assert req.header.custom_values('Signature-Input') == ['old=()']
	assert !req.header.contains_custom('Signature')
}

fn test_sign_response_and_verify() {
	mut resp := http.Response{
		status_code: 200
	}
	resp.header.add_custom('Content-Type', 'application/json')!
	resp.header.add_custom('Content-Length', '23')!
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_response(mut resp, key,
		components: ['@status', 'content-type', 'content-length']
		created:    1
	)!
	verify_response(resp, key)!
}

fn test_verify_response_requires_status_coverage_by_default() {
	mut resp := http.Response{
		status_code: 200
	}
	resp.header.add_custom('Content-Type', 'application/json')!
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_response(mut resp, key, components: ['content-type'], created: 1)!
	if _ := verify_response(resp, key) {
		assert false, 'response verification must require status coverage by default'
	} else {
		assert err is MalformedMessage
		assert err.msg().contains('@status')
	}
	verify_response(resp, key, required_components: ['content-type'])!
}

fn test_sign_response_normalizes_zero_status_to_wire_ok() {
	mut resp := http.Response{
		status_msg: 'custom reason without a status'
	}
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_response(mut resp, key, components: ['@status'], created: 1)!
	received := http.Response{
		...resp
		status_code: 200
	}
	verify_response(received, key)!
}

fn test_sign_response_preserves_unassigned_three_digit_status() {
	mut resp := http.Response{
		status_code: 299
	}
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_response(mut resp, key, components: ['@status'], created: 1)!
	verify_response(resp, key)!
	tampered := http.Response{
		...resp
		status_code: 250
	}
	if _ := verify_response(tampered, key) {
		assert false, 'the signature must bind the actual unassigned status code'
	} else {
		assert err is VerificationFailed
	}
}

fn test_sign_response_includes_generated_content_length() {
	mut resp := http.Response{
		body: 'hello'
	}
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_response(mut resp, key, components: ['@status', 'content-length'], created: 1)!
	assert (resp.header.get(.content_length) or { '' }) == '5'
	mut received := resp
	received.status_code = 200
	verify_response(received, key)!
}

fn test_sign_response_failure_does_not_add_generated_content_length() {
	mut resp := http.Response{
		body: 'hello'
	}
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_response(mut resp, key,
		components: ['@status', 'content-length']
		created:    2
		expires:    1
	)
	{
		assert false, 'invalid signature parameters must fail'
	} else {
		assert err is MalformedMessage
	}
	assert !resp.header.contains(.content_length)
	assert !resp.header.contains_custom('Signature-Input')
	assert !resp.header.contains_custom('Signature')
}

fn test_outgoing_trace_components_do_not_synthesize_content_length() {
	req := http.Request{
		method: .trace
		url:    'https://example.com/'
	}
	c := request_components(req, 'https', .outgoing)!
	if _ := c.component_value('content-length') {
		assert false, 'TRACE must not synthesize a Content-Length field'
	} else {
		assert err is MalformedMessage
	}
}

fn test_sign_response_rejects_existing_label() {
	mut resp := http.Response{
		status_code: 200
	}
	key := Key.hmac_sha256(test_secret.bytes())!
	sign_response(mut resp, key, created: 1)!
	if _ := sign_response(mut resp, key, created: 2) {
		assert false, 'an existing response signature label must not be appended again'
	} else {
		assert err is MalformedMessage
	}
}

fn test_sign_response_rejects_empty_existing_signature() {
	mut resp := http.Response{
		status_code: 200
	}
	resp.header.add_custom('Signature', '')!
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_response(mut resp, key, created: 1) {
		assert false, 'an empty existing Signature dictionary must be rejected'
	} else {
		assert err is MalformedMessage
	}
	assert resp.header.custom_values('Signature') == ['']
	assert !resp.header.contains_custom('Signature-Input')
}

fn test_sign_response_rejects_self_referential_signature_field_without_mutation() {
	mut resp := http.Response{
		status_code: 200
		body:        'hello'
	}
	resp.header.add_custom('Signature', 'old=:b2xk:')!
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_response(mut resp, key,
		components: ['signature', 'content-length']
		label:      'new'
		created:    1
	)
	{
		assert false, 'a field changed by signing cannot be covered'
	} else {
		assert err is MalformedMessage
	}
	assert resp.header.custom_values('Signature') == ['old=:b2xk:']
	assert !resp.header.contains(.content_length)
}

fn test_sign_response_rejects_http2_removed_covered_field() {
	mut resp := http.Response{
		status_code: 200
	}
	resp.header.set(.connection, 'keep-alive')
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_response(mut resp, key, components: ['connection'], created: 1) {
		assert false, 'fields removed by an HTTP/2 response transport cannot be covered'
	} else {
		assert err is MalformedMessage
	}
	assert !resp.header.contains_custom('Signature')
}

fn test_sign_response_preflights_both_signature_header_slots() {
	mut resp := http.Response{
		status_code: 200
	}
	for i in 0 .. http.max_headers - 1 {
		resp.header.add_custom('X-Fill-${i}', 'value')!
	}
	key := Key.hmac_sha256(test_secret.bytes())!
	if _ := sign_response(mut resp, key, components: ['@status'], created: 1) {
		assert false, 'signing must reject insufficient capacity before changing either header'
	} else {
		assert err is MalformedMessage
	}
	assert !resp.header.contains_custom('Signature-Input')
	assert !resp.header.contains_custom('Signature')
}

fn test_response_components_do_not_invent_content_length() {
	c := response_components(http.Response{
		body: 'hello'
	})
	if _ := c.component_value('content-length') {
		assert false, 'an absent response Content-Length must remain absent'
	} else {
		assert err is MalformedMessage
	}
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
	key := Key.hmac_sha256('k'.bytes())!
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

// Byte-exact test cases from RFC 9421 Appendix B. Deterministic
// algorithms (Ed25519, HMAC) reproduce the reference signature
// exactly; ECDSA can only verify the reference (RFC 9421 §B.2.4
// notes its non-determinism).
module signature

import encoding.base64
import encoding.hex

const sample_request_date = 'Tue, 20 Apr 2021 02:07:55 GMT'

// b64u_decode is base64.url_decode with manual padding restoration —
// JWK encodes raw key coordinates without trailing '=' padding.
fn b64u_decode(s string) []u8 {
	mut padded := s
	for padded.len % 4 != 0 {
		padded += '='
	}
	return base64.url_decode(padded)
}

// RFC 9421 §B.2.6: Ed25519 over the standard test request, byte-exact
// reproduction of the reference signature.
fn test_b26_ed25519_signature_base_matches_rfc() {
	c := Components{
		method:    'POST'
		path:      '/foo'
		authority: 'example.com'
		fields:    {
			'date':           ['Tue, 20 Apr 2021 02:07:55 GMT']
			'content-type':   ['application/json']
			'content-length': ['18']
		}
	}
	p := SignatureParams{
		components: ['date', '@method', '@path', '@authority', 'content-type', 'content-length']
		created:    1618884473
		keyid:      'test-key-ed25519'
	}
	got := signature_base_string(c, p)!
	want := '"date": Tue, 20 Apr 2021 02:07:55 GMT\n' + '"@method": POST\n' + '"@path": /foo\n' +
		'"@authority": example.com\n' + '"content-type": application/json\n' +
		'"content-length": 18\n' +
		'"@signature-params": ("date" "@method" "@path" "@authority" "content-type" "content-length");created=1618884473;keyid="test-key-ed25519"'
	assert got == want
}

fn test_b26_ed25519_signature_matches_rfc() {
	seed := b64u_decode('n4Ni-HpISpVObnQMW0wOhCKROaIKqKtW_2ZYb2p9KcU')
	priv := Key.ed25519_private(seed)
	c := b26_components()
	p := b26_params()
	out := sign(c, p, priv, 'sig-b26')!
	want_input := 'sig-b26=("date" "@method" "@path" "@authority" "content-type" "content-length");created=1618884473;keyid="test-key-ed25519"'
	want_signature := 'sig-b26=:wqcAqbmYJ2ji2glfAMaRy4gruYYnx2nEFN2HN6jrnDnQCK1u02Gb04v9EDgwUPiu4A0w6vuQv5lIp5WPpBKRCw==:'
	assert out.signature_input == want_input
	assert out.signature == want_signature
}

fn test_b26_ed25519_verify_roundtrip() {
	x := b64u_decode('JrQLj5P_89iXES9-vFgrIy29clF9CC_oPPsw3c5D0bs')
	pub_key := Key.ed25519_public(x)
	verify(b26_components(),
		'sig-b26=("date" "@method" "@path" "@authority" "content-type" "content-length");created=1618884473;keyid="test-key-ed25519"',
		'sig-b26=:wqcAqbmYJ2ji2glfAMaRy4gruYYnx2nEFN2HN6jrnDnQCK1u02Gb04v9EDgwUPiu4A0w6vuQv5lIp5WPpBKRCw==:',
		'sig-b26', pub_key)!
}

fn b26_components() Components {
	return Components{
		method:    'POST'
		path:      '/foo'
		authority: 'example.com'
		fields:    {
			'date':           [sample_request_date]
			'content-type':   ['application/json']
			'content-length': ['18']
		}
	}
}

fn b26_params() SignatureParams {
	return SignatureParams{
		components: ['date', '@method', '@path', '@authority', 'content-type', 'content-length']
		created:    1618884473
		keyid:      'test-key-ed25519'
	}
}

// RFC 9421 §B.2.5: HMAC-SHA256 — deterministic, byte-exact.
fn test_b25_hmac_sha256_matches_rfc() {
	secret :=
		base64.decode('uzvJfB4u3N0Jy4T7NZ75MDVcr8zSTInedJtkgcu46YW4XByzNJjxBdtjUkdJPBtbmHhIDi6pcl8jsasjlTMtDQ==')
	key := Key.hmac_sha256(secret)
	c := Components{
		authority: 'example.com'
		fields:    {
			'date':         [sample_request_date]
			'content-type': ['application/json']
		}
	}
	p := SignatureParams{
		components: ['date', '@authority', 'content-type']
		created:    1618884473
		keyid:      'test-shared-secret'
	}
	out := sign(c, p, key, 'sig-b25')!
	assert out.signature_input == 'sig-b25=("date" "@authority" "content-type");created=1618884473;keyid="test-shared-secret"'
	assert out.signature == 'sig-b25=:pxcQw6G3AjtMBQjwo8XzkZf/bws5LelbaMk5rGIGtE8=:'
	verify(c, out.signature_input, out.signature, 'sig-b25', key)!
}

// RFC 9421 §B.2.4: ECDSA P-256 / SHA-256 over the test response.
// ECDSA is non-deterministic — only verification of the reference
// signature is checked here. A separate test does sign+verify roundtrip.
fn test_b24_ecdsa_p256_verify_rfc_signature() {
	x := b64u_decode('qIVYZVLCrPZHGHjP17CTW0_-D9Lfw0EkjqF7xB4FivA')
	y := b64u_decode('Mc4nN9LTDOBhfoUeg8Ye9WedFRhnZXZJA12Qp0zZ6F0')
	pub_key := Key.ecdsa_p256_public(x, y)
	c := b24_components()
	verify(c,
		'sig-b24=("@status" "content-type" "content-digest" "content-length");created=1618884473;keyid="test-key-ecc-p256"',
		'sig-b24=:wNmSUAhwb5LxtOtOpNa6W5xj067m5hFrj0XQ4fvpaCLx0NKocgPquLgyahnzDnDAUy5eCdlYUEkLIj+32oiasw==:',
		'sig-b24', pub_key)!
}

fn test_b24_ecdsa_p256_sign_verify_roundtrip() {
	x := b64u_decode('qIVYZVLCrPZHGHjP17CTW0_-D9Lfw0EkjqF7xB4FivA')
	y := b64u_decode('Mc4nN9LTDOBhfoUeg8Ye9WedFRhnZXZJA12Qp0zZ6F0')
	d := b64u_decode('UpuF81l-kOxbjf7T4mNSv0r5tN67Gim7rnf6EFpcYDs')
	priv := Key.ecdsa_p256_private(x, y, d)
	pub_key := Key.ecdsa_p256_public(x, y)
	c := b24_components()
	p := SignatureParams{
		components: ['@status', 'content-type', 'content-digest', 'content-length']
		created:    1618884473
		keyid:      'test-key-ecc-p256'
	}
	out := sign(c, p, priv, 'sig-b24')!
	verify(c, out.signature_input, out.signature, 'sig-b24', pub_key)!
}

fn b24_components() Components {
	return Components{
		status: 200
		fields: {
			'content-type':   ['application/json']
			'content-digest': [
				'sha-512=:mEWXIS7MaLRuGgxOBdODa3xqM1XdEvxoYhvlCFJ41QJgJc4GTsPp29l5oGX69wWdXymyU0rjJuahq4l5aGgfLQ==:',
			]
			'content-length': ['23']
		}
	}
}

// Sanity check: the raw Ed25519 signature is exactly 64 bytes (RFC 8032).
fn test_ed25519_signature_is_64_bytes() {
	seed := b64u_decode('n4Ni-HpISpVObnQMW0wOhCKROaIKqKtW_2ZYb2p9KcU')
	priv := Key.ed25519_private(seed)
	base := signature_base_string(b26_components(), b26_params())!
	raw := sign_base(base.bytes(), priv)!
	assert raw.len == 64
	_ = hex.encode(raw)
}

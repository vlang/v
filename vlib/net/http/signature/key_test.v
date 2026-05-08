// Tests for Key.from_pem - the PEM blocks come from RFC 9421
// Appendix B.1.3 (ECDSA P-256) and B.1.4 (Ed25519). The verification
// roundtrip checks that the parsed key behaves identically to one
// built from raw coordinates.
module signature

const rfc_ed25519_public_pem = '-----BEGIN PUBLIC KEY-----
MCowBQYDK2VwAyEAJrQLj5P/89iXES9+vFgrIy29clF9CC/oPPsw3c5D0bs=
-----END PUBLIC KEY-----'

const rfc_ed25519_private_pem = '-----BEGIN PRIVATE KEY-----
MC4CAQAwBQYDK2VwBCIEIJ+DYvh6SEqVTm50DFtMDoQikTmiCqirVv9mWG9qfSnF
-----END PRIVATE KEY-----'

const rfc_ec_p256_public_pem = '-----BEGIN PUBLIC KEY-----
MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEqIVYZVLCrPZHGHjP17CTW0/+D9Lf
w0EkjqF7xB4FivAxzic30tMM4GF+hR6Dxh71Z50VGGdldkkDXZCnTNnoXQ==
-----END PUBLIC KEY-----'

const rfc_ec_p256_private_pem = '-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIFKbhfNZfpDsW43+0+JjUr9K+bTeuxopu653+hBaXGA7oAoGCCqGSM49
AwEHoUQDQgAEqIVYZVLCrPZHGHjP17CTW0/+D9Lfw0EkjqF7xB4FivAxzic30tMM
4GF+hR6Dxh71Z50VGGdldkkDXZCnTNnoXQ==
-----END EC PRIVATE KEY-----'

fn key_test_b26_components() Components {
	return Components{
		method:    'POST'
		path:      '/foo'
		authority: 'example.com'
		fields:    {
			'date':           ['Tue, 20 Apr 2021 02:07:55 GMT']
			'content-type':   ['application/json']
			'content-length': ['18']
		}
	}
}

fn key_test_b26_params() SignatureParams {
	return SignatureParams{
		components: ['date', '@method', '@path', '@authority', 'content-type', 'content-length']
		created:    1618884473
		keyid:      'test-key-ed25519'
	}
}

fn key_test_b24_components() Components {
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

fn test_from_pem_ed25519_private_reproduces_rfc_signature() {
	priv := Key.from_pem(rfc_ed25519_private_pem)!
	assert priv.algorithm == .ed25519
	assert priv.is_private
	c := key_test_b26_components()
	out := sign(c, key_test_b26_params(), priv, 'sig-b26')!
	// RFC 9421 §B.2.6 reference value, byte-exact.
	assert out.signature == 'sig-b26=:wqcAqbmYJ2ji2glfAMaRy4gruYYnx2nEFN2HN6jrnDnQCK1u02Gb04v9EDgwUPiu4A0w6vuQv5lIp5WPpBKRCw==:'
}

fn test_from_pem_ed25519_public_verifies_rfc_signature() {
	pub_key := Key.from_pem(rfc_ed25519_public_pem)!
	assert pub_key.algorithm == .ed25519
	assert !pub_key.is_private
	verify(key_test_b26_components(),
		'sig-b26=("date" "@method" "@path" "@authority" "content-type" "content-length");created=1618884473;keyid="test-key-ed25519"',
		'sig-b26=:wqcAqbmYJ2ji2glfAMaRy4gruYYnx2nEFN2HN6jrnDnQCK1u02Gb04v9EDgwUPiu4A0w6vuQv5lIp5WPpBKRCw==:',
		'sig-b26', pub_key)!
}

fn test_from_pem_ecdsa_p256_private_signs_and_verifies() {
	priv := Key.from_pem(rfc_ec_p256_private_pem)!
	pub_key := Key.from_pem(rfc_ec_p256_public_pem)!
	assert priv.algorithm == .ecdsa_p256_sha256
	assert pub_key.algorithm == .ecdsa_p256_sha256
	assert priv.is_private
	c := Components{
		method:     'POST'
		target_uri: 'https://example.com/'
	}
	p := SignatureParams{
		components: ['@method', '@target-uri']
		created:    1
	}
	out := sign(c, p, priv, 'sig1')!
	verify(c, out.signature_input, out.signature, 'sig1', pub_key)!
}

fn test_from_pem_ecdsa_p256_public_verifies_rfc_b24_reference() {
	pub_key := Key.from_pem(rfc_ec_p256_public_pem)!
	verify(key_test_b24_components(),
		'sig-b24=("@status" "content-type" "content-digest" "content-length");created=1618884473;keyid="test-key-ecc-p256"',
		'sig-b24=:wNmSUAhwb5LxtOtOpNa6W5xj067m5hFrj0XQ4fvpaCLx0NKocgPquLgyahnzDnDAUy5eCdlYUEkLIj+32oiasw==:',
		'sig-b24', pub_key)!
}

fn test_from_pem_rejects_garbage() {
	if _ := Key.from_pem('not a PEM block') {
		assert false, 'must reject non-PEM input'
	} else {
		assert err is MalformedMessage
	}
}

fn test_from_pem_rejects_unsupported_block_type() {
	src := '-----BEGIN RSA PRIVATE KEY-----\nMIIEvg==\n-----END RSA PRIVATE KEY-----'
	if _ := Key.from_pem(src) {
		assert false, 'must reject RSA PEM (no RSA support in this module)'
	} else {
		assert err is MalformedMessage
	}
}

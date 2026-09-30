// vtest build: !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
// Tests for Key.from_pem - the PEM blocks come from RFC 9421
// Appendix B.1.3 (ECDSA P-256) and B.1.4 (Ed25519). The verification
// roundtrip checks that the parsed key behaves identically to one
// built from raw coordinates.
module signature

import crypto.ed25519

const rfc_ed25519_public_pem = '-----BEGIN PUBLIC KEY-----
MCowBQYDK2VwAyEAJrQLj5P/89iXES9+vFgrIy29clF9CC/oPPsw3c5D0bs=
-----END PUBLIC KEY-----'

const rfc_ed25519_private_pem = '-----BEGIN PRIVATE KEY-----
MC4CAQAwBQYDK2VwBCIEIJ+DYvh6SEqVTm50DFtMDoQikTmiCqirVv9mWG9qfSnF
-----END PRIVATE KEY-----'

const secp256k1_private_pem = '-----BEGIN EC PRIVATE KEY-----
MHQCAQEEIOPU7WKsHTyVIXG8dgC7rRnWHs5aB4Ltm4evimpk/i3woAcGBSuBBAAK
oUQDQgAECSuyyabDUU2w1p22h1AGyfD7At+Cvb63E//kTmcXA55d1xZZH3WX6msE
9u0eNEe/4nVyHbTKoW+DKBFEGtHNpw==
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

fn test_from_pem_rejects_unsupported_same_width_curve() {
	if _ := Key.from_pem(secp256k1_private_pem) {
		assert false, 'secp256k1 must not be interpreted as NIST P-256'
	} else {
		assert err is UnsupportedAlgorithm
	}
}

fn test_pad_left_pads_to_width_and_rejects_overflow() {
	assert pad_left([u8(0x01)], 4)! == [u8(0x00), 0x00, 0x00, 0x01]
	assert pad_left([u8(0x01), 0x02, 0x03, 0x04], 4)! == [u8(0x01), 0x02, 0x03, 0x04]
	if _ := pad_left([u8(0x01), 0x02, 0x03, 0x04, 0x05], 4) {
		assert false, 'must reject scalars wider than the curve'
	} else {
		assert err is MalformedMessage
	}
}

fn test_raw_ecdsa_constructors_pad_coordinates() {
	p256 := Key.ecdsa_p256_private([u8(1)], [u8(2)], [u8(3)])!
	assert p256.bytes.len == 96
	assert p256.bytes[31] == 1
	assert p256.bytes[63] == 2
	assert p256.bytes[95] == 3
	p384 := Key.ecdsa_p384_public([u8(1)], [u8(2)])!
	assert p384.bytes.len == 96
	assert p384.bytes[47] == 1
	assert p384.bytes[95] == 2
	if _ := Key.ecdsa_p256_public([]u8{len: 33}, [u8(1)]) {
		assert false, 'raw constructors must reject oversized coordinates'
	} else {
		assert err is MalformedMessage
	}
}

fn test_hmac_rejects_empty_secret() {
	if _ := Key.hmac_sha256([]u8{}) {
		assert false, 'empty HMAC secrets must be rejected'
	} else {
		assert err is MalformedMessage
	}
	key := Key{
		algorithm: .hmac_sha256
	}
	if _ := sign_base('base'.bytes(), key) {
		assert false, 'directly constructed empty HMAC keys must not sign'
	} else {
		assert err is MalformedMessage
	}
}

fn test_verify_rejects_invalid_ed25519_private_seed_length() {
	key := Key.ed25519_private([u8(1)])
	if _ := verify_base('base'.bytes(), []u8{len: 64}, key, 'sig1') {
		assert false, 'verification must reject an invalid private seed without panicking'
	} else {
		assert err is MalformedMessage
	}
}

fn test_verify_rejects_invalid_ed25519_signature_length() {
	key := Key.ed25519_public([]u8{len: ed25519.public_key_size})
	if _ := verify_base('base'.bytes(), []u8{}, key, 'sig1') {
		assert false, 'verification must reject an invalid Ed25519 signature length without panicking'
	} else {
		assert err is VerificationFailed
	}
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

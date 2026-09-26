// vtest build: present_openssl? && !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
// vtest vflags: -d use_openssl
//
// ECDSA PEM/DER decoding lives in `crypto.ecdsa`, whose loaders are only
// implemented for the OpenSSL backend; the rest of this module - Ed25519 PEM
// included - runs on the default build, so those tests stay in key_test.v.
module signature

const rfc_ec_p256_public_pem = '-----BEGIN PUBLIC KEY-----
MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEqIVYZVLCrPZHGHjP17CTW0/+D9Lf
w0EkjqF7xB4FivAxzic30tMM4GF+hR6Dxh71Z50VGGdldkkDXZCnTNnoXQ==
-----END PUBLIC KEY-----'

const rfc_ec_p256_private_pem = '-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIFKbhfNZfpDsW43+0+JjUr9K+bTeuxopu653+hBaXGA7oAoGCCqGSM49
AwEHoUQDQgAEqIVYZVLCrPZHGHjP17CTW0/+D9Lfw0EkjqF7xB4FivAxzic30tMM
4GF+hR6Dxh71Z50VGGdldkkDXZCnTNnoXQ==
-----END EC PRIVATE KEY-----'

// P-256 key whose private scalar starts with 0x00, so OpenSSL's
// `BN_bn2binpad(num_bytes)` returns 31 bytes instead of 32. This
// exercises the leading-zero padding in `ecdsa_key_from_xy_d`.
const short_d_p256_private_pem = '-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIACZmEw0q8iipb0amaNiobX/wwn6PoIKUatErMY2Dd4+oAoGCCqGSM49
AwEHoUQDQgAE/z/OBheMT6mCKDapfETr56tkYLOrnQh+ZL293+IqXsJ+iMZgYe0/
WHaZhZfCu1OKUWayaVEkvb7j0o3uUfw+OQ==
-----END EC PRIVATE KEY-----'

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

fn test_from_pem_ecdsa_p256_pads_short_private_scalar() {
	// Regression: a P-256 PEM whose `d` has a leading zero byte must
	// still produce a 96-byte (x||y||d) key and sign successfully.
	priv := Key.from_pem(short_d_p256_private_pem)!
	assert priv.algorithm == .ecdsa_p256_sha256
	assert priv.is_private
	assert priv.bytes.len == 96
	c := Components{
		method:     'POST'
		target_uri: 'https://example.com/'
	}
	p := SignatureParams{
		components: ['@method', '@target-uri']
		created:    1
	}
	out := sign(c, p, priv, 'sig1')!
	verify(c, out.signature_input, out.signature, 'sig1', priv)!
}

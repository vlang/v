// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

import crypto.pem
import encoding.hex

// Tests of the PEM/DER key loaders on the default (mbedTLS) backend.
// Under -d use_openssl, util_test.v covers the OpenSSL backend's loaders.

// One P-256 key, generated with `openssl ecparam -name prime256v1 -genkey -noout`,
// in its SEC1 and PKCS#8 forms, with its SubjectPublicKeyInfo.
const p256_sec1 = '-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIM1+TGZA3bQ1j66kfXnJl00/rZ8cixAShRS+7+se2u1MoAoGCCqGSM49
AwEHoUQDQgAEuyKg9MnOWtJ9NzCxgeGfIREFJZ7tzJ0gXO9qY6GatVRfuZGGR8di
U6M7WWN0zrXjSNmQF8gPaIXy4sGFgS+wpQ==
-----END EC PRIVATE KEY-----'

const p256_pkcs8 = '-----BEGIN PRIVATE KEY-----
MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgzX5MZkDdtDWPrqR9
ecmXTT+tnxyLEBKFFL7v6x7a7UyhRANCAAS7IqD0yc5a0n03MLGB4Z8hEQUlnu3M
nSBc72pjoZq1VF+5kYZHx2JToztZY3TOteNI2ZAXyA9ohfLiwYWBL7Cl
-----END PRIVATE KEY-----'

const p256_spki = '-----BEGIN PUBLIC KEY-----
MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEuyKg9MnOWtJ9NzCxgeGfIREFJZ7t
zJ0gXO9qY6GatVRfuZGGR8diU6M7WWN0zrXjSNmQF8gPaIXy4sGFgS+wpQ==
-----END PUBLIC KEY-----'

const p256_scalar = 'cd7e4c6640ddb4358faea47d79c9974d3fad9f1c8b10128514beefeb1edaed4c'

const p256_point = '04bb22a0f4c9ce5ad27d3730b181e19f211105259eedcc9d205cef6a63a19ab5545fb9918647c76253a33b596374ceb5e348d99017c80f6885f2e2c185812fb0a5'

// A signature over p256_message by the key above, made by OpenSSL:
// `openssl dgst -sha256 -sign sec1.pem`.
const p256_message = 'crypto.ecdsa default backend'.bytes()

const p256_openssl_signature = '3045022100e55e9ab2581473e6d89f6252a4407fc7445c2fdb3db3292e14bd977c21ed9c0a022024770d3e8d56afa5bd035ea4bac5129d2afe3741b3fed850a4f1038d51e49b5a'

// The P-384 key of util_test.v, in PKCS#8, and its SubjectPublicKeyInfo.
const p384_pkcs8 = '-----BEGIN PRIVATE KEY-----
MIG2AgEAMBAGByqGSM49AgEGBSuBBAAiBIGeMIGbAgEBBDAwzj2iiJZaxgk/C6mp
oVskdr6j7akl4bPB8JRnT1J5XNbLPK/iNd/BW+xUJEj/pxWhZANiAAT4/euEWRPV
9cdhtjcKlwF2HrFMLvgxAXFx+01UPfMQ9XOj/85qUhVq1jXraSyDy5FYF28UW4dn
04xVeRuPBbCFxc/uqYj2s5ItHcAZSV3L5sGlXadPfTqoIjCBQAx44k8=
-----END PRIVATE KEY-----'

const p384_spki = '-----BEGIN PUBLIC KEY-----
MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAE+P3rhFkT1fXHYbY3CpcBdh6xTC74MQFx
cftNVD3zEPVzo//OalIVatY162ksg8uRWBdvFFuHZ9OMVXkbjwWwhcXP7qmI9rOS
LR3AGUldy+bBpV2nT306qCIwgUAMeOJP
-----END PUBLIC KEY-----'

// util_test.v's signature over its message with the P-384 key, SHA-384.
const p384_message = 'Example of ECDSA with P-384'.bytes()

const p384_signature = '3066023100b08f6ec77bb319fdb7bce55a2714d7e79cc645d834ee539d8903cfcc88c6fa90df1558856cb840b2dd82e82cd89d7046023100d9d482ca8a6545a3b081fbdd4bb9643a2b4eda4e21fd624833216596032471faae646891f8d2f0bbb86b796c36d3c390'

fn test_privkey_from_string_reads_sec1_and_pkcs8() ! {
	for text in [p256_sec1, p256_pkcs8] {
		key := privkey_from_string(text)!
		defer {
			key.free()
		}
		assert key.bytes()!.hex() == p256_scalar
		public := key.public_key()!
		defer {
			public.free()
		}
		assert public.bytes()!.hex() == p256_point
	}
}

fn test_loaded_private_key_signs_for_the_loaded_public_key() ! {
	key := privkey_from_string(p256_pkcs8)!
	defer {
		key.free()
	}
	public := pubkey_from_string(p256_spki)!
	defer {
		public.free()
	}
	signature := key.sign(p256_message)!
	assert public.verify(p256_message, signature)!
	assert !public.verify('another message'.bytes(), signature)!
}

fn test_pubkey_from_string_verifies_an_openssl_signature() ! {
	public := pubkey_from_string(p256_spki)!
	defer {
		public.free()
	}
	assert public.bytes()!.hex() == p256_point
	assert public.verify(p256_message, hex.decode(p256_openssl_signature)!)!
}

fn test_pubkey_from_bytes_reads_der() ! {
	block, _ := pem.decode(p256_spki) or { return error('invalid PEM block') }
	public := pubkey_from_bytes(block.data)!
	defer {
		public.free()
	}
	assert public.bytes()!.hex() == p256_point
	assert public.verify(p256_message, hex.decode(p256_openssl_signature)!)!
}

fn test_p384_keys_load_with_their_curve() ! {
	key := privkey_from_string(p384_pkcs8)!
	defer {
		key.free()
	}
	assert key.bytes()!.hex() == '30ce3da288965ac6093f0ba9a9a15b2476bea3eda925e1b3c1f094674f52795cd6cb3cafe235dfc15bec542448ffa715'
	public := pubkey_from_string(p384_spki)!
	defer {
		public.free()
	}
	assert public.verify(p384_message, hex.decode(p384_signature)!)!
	derived := key.public_key()!
	defer {
		derived.free()
	}
	assert derived.equal(public)
}

fn test_unsupported_curve_is_refused() {
	// secp192k1: mbedTLS parses it, but Nid does not name it.
	key := '-----BEGIN EC PRIVATE KEY-----
MFwCAQEEGDHV+WhJL2UjUhgMLh52k0RJjRebtu4HvqAHBgUrgQQAH6E0AzIABFyF
UHhnmmVRraSwrVkPdYIeXhH/Ob4+8OLcwrQBMv4RXsD1GVFsgkvEYDTEb/vnMA==
-----END EC PRIVATE KEY-----'
	privkey_from_string(key) or {
		assert err.msg().contains('unsupported curve')
		return
	}
	assert false, 'a secp192k1 key should be refused'
}

fn test_a_non_ec_key_is_refused() {
	rsa := '-----BEGIN PUBLIC KEY-----
MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDhzJEEIweJWf/KeEDvFDN6CZD/
n8L6OI13JlEo2BE5hk5wcpYP/9s7PcQxMgevU7jR/zY4CaYCicKlLA4YlgTNdNLw
4lSgpsG9vaVsJx4/lEflvn2DIMhbr7wPYLYkq5UO0/wqpOzgQIZ65pI9eAG+F5J6
RGIGUXI57hgeVveBVQIDAQAB
-----END PUBLIC KEY-----'
	pubkey_from_string(rsa) or {
		assert err.msg().contains('not an EC key')
		return
	}
	assert false, 'an RSA key should be refused'
}

fn test_malformed_input_is_refused() {
	if _ := pubkey_from_bytes([u8(4), 1, 2, 3]) {
		assert false, 'garbage DER should be refused'
	}
	if _ := pubkey_from_string('-----BEGIN PUBLIC KEY-----\n-----END PUBLIC KEY-----') {
		assert false, 'an empty PEM body should be refused'
	}
	if _ := privkey_from_string('-----BEGIN PRIVATE KEY-----\n-----END PRIVATE KEY-----') {
		assert false, 'an empty PEM body should be refused'
	}
	if _ := pubkey_from_bytes([]u8{}) {
		assert false, 'empty bytes should be refused'
	}
}

fn test_pubkey_bytes_delegates_to_uncompressed_bytes() ! {
	pub_key, priv_key := generate_key()!
	defer {
		priv_key.free()
		pub_key.free()
	}
	assert pub_key.bytes()! == pub_key.uncompressed_bytes()!
}

module ecdsa

import encoding.hex
import crypto.pem

// This material wss generated with https://emn178.github.io/online-tools/ecdsa/key-generator
// with curve SECG secp384r1 aka NIST P-384
const privatekey_sample = '-----BEGIN PRIVATE KEY-----
MIG2AgEAMBAGByqGSM49AgEGBSuBBAAiBIGeMIGbAgEBBDAwzj2iiJZaxgk/C6mp
oVskdr6j7akl4bPB8JRnT1J5XNbLPK/iNd/BW+xUJEj/pxWhZANiAAT4/euEWRPV
9cdhtjcKlwF2HrFMLvgxAXFx+01UPfMQ9XOj/85qUhVq1jXraSyDy5FYF28UW4dn
04xVeRuPBbCFxc/uqYj2s5ItHcAZSV3L5sGlXadPfTqoIjCBQAx44k8=
-----END PRIVATE KEY-----'

const public_key_sample = '-----BEGIN PUBLIC KEY-----
MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAE+P3rhFkT1fXHYbY3CpcBdh6xTC74MQFx
cftNVD3zEPVzo//OalIVatY162ksg8uRWBdvFFuHZ9OMVXkbjwWwhcXP7qmI9rOS
LR3AGUldy+bBpV2nT306qCIwgUAMeOJP
-----END PUBLIC KEY-----'

// Message tobe signed and verified
const message_tobe_signed = 'Example of ECDSA with P-384'.bytes()
// Message signature created with SHA384 digest with associated above key
const expected_signature = hex.decode('3066023100b08f6ec77bb319fdb7bce55a2714d7e79cc645d834ee539d8903cfcc88c6fa90df1558856cb840b2dd82e82cd89d7046023100d9d482ca8a6545a3b081fbdd4bb9643a2b4eda4e21fd624833216596032471faae646891f8d2f0bbb86b796c36d3c390')!

fn test_load_pubkey_from_der_serialized_bytes() ! {
	block, _ := pem.decode(public_key_sample) or { panic(err) }
	pbkey := pubkey_from_bytes(block.data)!

	status_without_hashed := pbkey.verify(message_tobe_signed, expected_signature,
		hash_config: .with_no_hash
	)!
	assert status_without_hashed == false

	// expected signature was comes from hashed message with sha384
	status_with_hashed := pbkey.verify(message_tobe_signed, expected_signature)!
	assert status_with_hashed == true
	key_free(pbkey.key)
}

fn test_for_pubkey_bytes() ! {
	// material generated with online ecdsa generator https://emn178.github.io/online-tools/ecdsa/key-generator/
	pv := '62e998bea8a15f52ff0b76cf3fe281cfcd8042ce4479b6e652ca7b5a36f6fb40'
	pb := '0421af184ac64c8a13e66c65d4f1ad31677edeaa97af791aef73b66ea26d1623a411f67b6c4d842ba22fa39d1216bd64acef00a1b924ac11a10af679ac3a7eb2fd'
	pvkey := new_key_from_seed(hex.decode(pv)!)!

	assert pvkey.seed()!.hex() == pv
	pbkey := pvkey.public_key()!
	assert pbkey.bytes()!.hex() == pb
	key_free(pbkey.key)
	key_free(pvkey.key)
}

// above pem-formatted private key read with
// `$openssl ec -in vlib/crypto/ecdsa/example.pem -text -param_out -check`
// produces following result:
// ```codeblock
// read EC key
// Private-Key: (384 bit)
// priv:
//    30:ce:3d:a2:88:96:5a:c6:09:3f:0b:a9:a9:a1:5b:
//    24:76:be:a3:ed:a9:25:e1:b3:c1:f0:94:67:4f:52:
//    79:5c:d6:cb:3c:af:e2:35:df:c1:5b:ec:54:24:48:
//    ff:a7:15
// pub:
//    04:f8:fd:eb:84:59:13:d5:f5:c7:61:b6:37:0a:97:
//    01:76:1e:b1:4c:2e:f8:31:01:71:71:fb:4d:54:3d:
//    f3:10:f5:73:a3:ff:ce:6a:52:15:6a:d6:35:eb:69:
//    2c:83:cb:91:58:17:6f:14:5b:87:67:d3:8c:55:79:
//    1b:8f:05:b0:85:c5:cf:ee:a9:88:f6:b3:92:2d:1d:
//    c0:19:49:5d:cb:e6:c1:a5:5d:a7:4f:7d:3a:a8:22:
//    30:81:40:0c:78:e2:4f
// ASN1 OID: secp384r1
// NIST CURVE: P-384
// EC Key valid.
// writing EC key
// -----BEGIN EC PRIVATE KEY-----
// MIGkAgEBBDAwzj2iiJZaxgk/C6mpoVskdr6j7akl4bPB8JRnT1J5XNbLPK/iNd/B
// W+xUJEj/pxWgBwYFK4EEACKhZANiAAT4/euEWRPV9cdhtjcKlwF2HrFMLvgxAXFx
// +01UPfMQ9XOj/85qUhVq1jXraSyDy5FYF28UW4dn04xVeRuPBbCFxc/uqYj2s5It
// HcAZSV3L5sGlXadPfTqoIjCBQAx44k8=
// -----END EC PRIVATE KEY-----
// ```
fn test_load_privkey_from_string_sign_and_verify() ! {
	pvkey := privkey_from_string(privatekey_sample)!
	expected_pvkey_bytes := '30ce3da288965ac6093f0ba9a9a15b2476bea3eda925e1b3c1f094674f52795cd6cb3cafe235dfc15bec542448ffa715'
	assert pvkey.seed()!.hex() == expected_pvkey_bytes

	// public key part	
	pbkey := pvkey.public_key()!
	pbkey_bytes := pbkey.bytes()!
	expected_pubkey_bytes := '04f8fdeb845913d5f5c761b6370a9701761eb14c2ef831017171fb4d543df310f573a3ffce6a52156ad635eb692c83cb9158176f145b8767d38c55791b8f05b085c5cfeea988f6b3922d1dc019495dcbe6c1a55da74f7d3aa8223081400c78e24f'
	assert pbkey_bytes.hex() == expected_pubkey_bytes

	// lets sign the message with default hash, ie, sha384
	signature := pvkey.sign(message_tobe_signed)!

	verified := pbkey.verify(message_tobe_signed, signature)!
	assert verified == true
	pvkey.free()
	pbkey.free()
}

fn test_load_pubkey_from_string_and_used_for_verifying() ! {
	pbkey := pubkey_from_string(public_key_sample)!
	pbkey_bytes := pbkey.bytes()!
	expected_pubkey_bytes := '04f8fdeb845913d5f5c761b6370a9701761eb14c2ef831017171fb4d543df310f573a3ffce6a52156ad635eb692c83cb9158176f145b8767d38c55791b8f05b085c5cfeea988f6b3922d1dc019495dcbe6c1a55da74f7d3aa8223081400c78e24f'
	assert pbkey_bytes.hex() == expected_pubkey_bytes

	// expected signature was comes from hashed message with sha384
	status_with_hashed := pbkey.verify(message_tobe_signed, expected_signature)!
	assert status_with_hashed == true
	pbkey.free()
}

// test for loading privat key from unsupported curve should fail.
fn test_load_privkey_from_string_with_unsupported_curve() ! {
	// generated with openssl ecparam -name secp192k1 -genkey -noout -out key.pem
	key := '-----BEGIN EC PRIVATE KEY-----
MFwCAQEEGDHV+WhJL2UjUhgMLh52k0RJjRebtu4HvqAHBgUrgQQAH6E0AzIABFyF
UHhnmmVRraSwrVkPdYIeXhH/Ob4+8OLcwrQBMv4RXsD1GVFsgkvEYDTEb/vnMA==
-----END EC PRIVATE KEY-----'
	_ := privkey_from_string(key) or {
		assert err == error('Unsupported group')
		return
	}
}

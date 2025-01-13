module ecdsa

import encoding.hex
import crypto.pem
import crypto.sha512

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
const message_tobe_signed = 'Example of ECDSA with P-384'
// Message signature created with SHA384 digest with associated above key
const expected_signature = hex.decode('3066023100b08f6ec77bb319fdb7bce55a2714d7e79cc645d834ee539d8903cfcc88c6fa90df1558856cb840b2dd82e82cd89d7046023100d9d482ca8a6545a3b081fbdd4bb9643a2b4eda4e21fd624833216596032471faae646891f8d2f0bbb86b796c36d3c390')!

fn test_load_pubkey_from_der_serialized_bytes() ! {
	block, _ := pem.decode(public_key_sample) or { panic(err) }
	pbkey := pubkey_from_bytes(block.data)!

	status_without_hashed := pbkey.verify(message_tobe_signed.bytes(), expected_signature)!
	assert status_without_hashed == false

	hashed_msg := sha512.sum384(message_tobe_signed.bytes())
	status_with_hashed := pbkey.verify(hashed_msg, expected_signature)!
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

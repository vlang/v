// vtest build: present_openssl? && !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
import rand
import crypto.ecdsa
import encoding.hex
// The test file placed on its own directory. Its for workaround for
// module lookup problem, because there are two rand module availables,
// between `crypto.rand` and `rand` module.
// See [the talk](https://discord.com/channels/592103645835821068/592294828432424960/1328198034806407311) on discord.

fn test_new_key_from_seed_with_random_size_and_data() ! {
	num_iters := 100
	// default prime256v1 curve key size was 32 bytes.
	max_key_size := i32(48)
	for i := 0; i <= num_iters; i++ {
		m := rand.i32n(max_key_size)!
		random_bytes := rand.bytes(m)!
		pvkey := ecdsa.new_key_from_seed(random_bytes) or {
			// With default size, would error on m > 32 or m == 0
			// dump(m)
			if m == 0 {
				assert err == error('Seed with null-length was not allowed')
			} else if m > 32 {
				assert err == error('Seed length exceeds key size')
			} else {
				assert err == error('EC_KEY_check_key failed')
			}
			continue
		}
		ret_seed := pvkey.bytes()!
		assert random_bytes == ret_seed
		pvkey.free()
	}
}

fn test_private_and_public_key_from_string() ! {
	// See [this](https://github.com/vlang/v/blob/master/vlib/crypto/ecdsa/util_test.v) for detail
	// of material used as a sample.
	privkey_sample := '-----BEGIN PRIVATE KEY-----
MIG2AgEAMBAGByqGSM49AgEGBSuBBAAiBIGeMIGbAgEBBDAwzj2iiJZaxgk/C6mp
oVskdr6j7akl4bPB8JRnT1J5XNbLPK/iNd/BW+xUJEj/pxWhZANiAAT4/euEWRPV
9cdhtjcKlwF2HrFMLvgxAXFx+01UPfMQ9XOj/85qUhVq1jXraSyDy5FYF28UW4dn
04xVeRuPBbCFxc/uqYj2s5ItHcAZSV3L5sGlXadPfTqoIjCBQAx44k8=
-----END PRIVATE KEY-----'

	pubkey_sample := '-----BEGIN PUBLIC KEY-----
MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAE+P3rhFkT1fXHYbY3CpcBdh6xTC74MQFx
cftNVD3zEPVzo//OalIVatY162ksg8uRWBdvFFuHZ9OMVXkbjwWwhcXP7qmI9rOS
LR3AGUldy+bBpV2nT306qCIwgUAMeOJP
-----END PUBLIC KEY-----'

	// Message tobe signed and verified
	message_tobe_signed := 'Example of ECDSA with P-384'.bytes()
	// Message signature generated with SHA384 digest with associated key previously.
	signature := hex.decode('3066023100b08f6ec77bb319fdb7bce55a2714d7e79cc645d834ee539d8903cfcc88c6fa90df1558856cb840b2dd82e82cd89d7046023100d9d482ca8a6545a3b081fbdd4bb9643a2b4eda4e21fd624833216596032471faae646891f8d2f0bbb86b796c36d3c390')!

	// loads a Privatekey and PublicKey from above sample
	privkey := ecdsa.privkey_from_string(privkey_sample)!
	pubkey := ecdsa.pubkey_from_string(pubkey_sample)!
	// get a public key from private key
	pbkey_from_privkey := privkey.public_key()!

	// two public key should be equal, its comes from the same source.
	assert pubkey.equal(pbkey_from_privkey)

	// lets create the signature
	created_signature := privkey.sign(message_tobe_signed)!

	verified1 := pubkey.verify(message_tobe_signed, signature)!
	verified2 := pubkey.verify(message_tobe_signed, created_signature)!

	assert verified1 == true
	assert verified2 == true

	// Its also should be verified with pbkey_from_privkey opaque
	verified3 := pbkey_from_privkey.verify(message_tobe_signed, signature)!
	verified4 := pbkey_from_privkey.verify(message_tobe_signed, created_signature)!
	assert verified3 == true
	assert verified4 == true

	// release the key
	privkey.free()
	pubkey.free()
	pbkey_from_privkey.free()
}

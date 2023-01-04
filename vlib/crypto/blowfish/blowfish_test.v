import crypto.blowfish
import encoding.base64

fn test_crypto_blowfish() {
	key := 'password'.bytes()
	csalt := base64.decode('an2da3dn')
	bf := blowfish.new_salted_cipher(key, csalt) or { panic(err) }
}

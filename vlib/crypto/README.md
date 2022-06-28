## Description:

`crypto` is a module that exposes cryptographic algorithms to V programs.

Each submodule implements things differently, so be sure to consider the documentation
of the specific algorithm you need, but in general, the method is to create a `cipher`
struct using one of the module functions, and then to call the `encrypt` or `decrypt`
method on that struct to actually encrypt or decrypt your data.

This module is a work-in-progress. For example, the AES implementation currently requires you
to create a destination buffer of the correct size to receive the decrypted data, and the AesCipher
`encrypt` and `decrypt` functions only operate on the first block of the `src`.

The implementations here are loosely based on [Go's crypto package](https://pkg.go.dev/crypto).

## Examples:

### AES:
```v
import crypto.aes
import crypto.rand

fn main() {
	// remember to save this key somewhere if you ever want to decrypt your data
	key := rand.bytes(32)?
	println('KEY: $key')

	// this data is one block (16 bytes) big
	mut data := 'THIS IS THE DATA'.bytes()

	println('generating cipher')
	cipher := aes.new_cipher(key)

	println('performing encryption')
	mut encrypted := []u8{len: aes.block_size}
	cipher.encrypt(mut encrypted, data)
	println(encrypted)

	println('performing decryption')
	mut decrypted := []u8{len: aes.block_size}
	cipher.decrypt(mut decrypted, encrypted)
	println(decrypted)

	assert decrypted == data
}
```

### JWT:
```v
import crypto.hmac
import crypto.sha256
import encoding.base64
import json

struct JwtHeader {
	alg string
	typ string
}

struct JwtPayload {
	sub  string
	name string
	iat  int
}

fn main() {
	token := make_token()
	println(token)
}

fn make_token() string {
	secret := 'your-256-bit-secret'

	header := base64.url_encode(json.encode(JwtHeader{'HS256', 'JWT'}).bytes())
	payload := base64.url_encode(json.encode(JwtPayload{'1234567890', 'John Doe', 1516239022}).bytes())
	signature := base64.url_encode(hmac.new(secret.bytes(), '${header}.$payload'.bytes(),
		sha256.sum, sha256.block_size).bytestr().bytes())

	jwt := '${header}.${payload}.$signature'

	return jwt
}
```

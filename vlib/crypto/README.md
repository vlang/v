## Description

`crypto` is a module that exposes cryptographic algorithms to V programs.

Each submodule implements things differently, so be sure to consider the documentation
of the specific algorithm you need, but in general, the method is to create a `cipher`
struct using one of the module functions, and then to call the `encrypt` or `decrypt`
method on that struct to actually encrypt or decrypt your data.

This module is a work-in-progress. For example, the AES implementation currently requires you
to create a destination buffer of the correct size to receive the decrypted data, and the AesCipher
`encrypt` and `decrypt` functions only operate on the first block of the `src`.

The implementations here are loosely based on [Go's crypto package](https://pkg.go.dev/crypto).

## Examples

### AES

```v
import crypto.aes
import crypto.rand

fn main() {
	// remember to save this key somewhere if you ever want to decrypt your data
	key := rand.bytes(32)!
	println('KEY: ${key}')

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

### JWT

```v
import crypto.hmac
import crypto.sha256
import encoding.base64
import json
import time

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
	sw := time.new_stopwatch()
	secret := 'your-256-bit-secret'
	token := make_token(secret)
	ok := auth_verify(secret, token)
	pl := decode_payload(token) or { panic(err) }
	dt := sw.elapsed().microseconds()
	println('token: ${token}')
	println('auth_verify(secret, token): ${ok}')
	println('decode_payload(token): ${pl}')
	println('Elapsed time: ${dt} uS')
}

fn make_token(secret string) string {
	header := base64.url_encode(json.encode(JwtHeader{'HS256', 'JWT'}).bytes())
	payload :=
		base64.url_encode(json.encode(JwtPayload{'1234567890', 'John Doe', 1516239022}).bytes())
	signature := base64.url_encode(hmac.new(secret.bytes(), '${header}.${payload}'.bytes(),
		sha256.sum, sha256.block_size))
	jwt := '${header}.${payload}.${signature}'
	return jwt
}

fn auth_verify(secret string, token string) bool {
	token_split := token.split('.')
	signature_mirror := hmac.new(secret.bytes(), '${token_split[0]}.${token_split[1]}'.bytes(),
		sha256.sum, sha256.block_size)
	signature_from_token := base64.url_decode(token_split[2])
	return hmac.equal(signature_from_token, signature_mirror)
}

fn decode_payload(token string) !JwtPayload {
	token_split := token.split('.')
	payload := json.decode(JwtPayload, base64.url_decode_str(token_split[1]))!
	return payload
}
```

### HKDF

```v
import crypto.hkdf
import crypto.sha256

fn main() {
	secret := 'shared secret'.bytes()
	salt := 'salt'.bytes()
	info := 'session keys'
	key := hkdf.key(sha256.new, secret, salt, info, 32)!
	assert key.len == 32
}
```

### Argon2 Password Hashing

```v
import crypto.argon2

fn main() {
	hash := argon2.generate_from_password('correct horse battery staple'.bytes())!
	println(hash)

	argon2.compare_hash_and_password('correct horse battery staple'.bytes(), hash.bytes())!
}
```

### bcrypt Password Hashing

```v
import crypto.bcrypt

fn main() {
	password := 'correct horse battery staple'.bytes()
	hash := bcrypt.generate_from_password(password, bcrypt.default_cost)!
	println(hash)

	bcrypt.compare_hash_and_password(password, hash.bytes())!
}
```

### scrypt

`scrypt` is a memory-hard key derivation function (RFC 7914). `n` must be a power of two.
For password storage OWASP recommends at least `(n: 2^17, r: 8, p: 1)` (or `(n: 2^14, r: 8, p: 5)`);
tune the parameters for your hardware. Smaller values such as `(n: 16384, r: 8, p: 1)` are only a
low-cost interactive/demo profile, not a password-storage profile.

```v
import crypto.rand
import crypto.scrypt

fn main() {
	password := 'correct horse battery staple'.bytes()
	// generate and persist a unique random salt per password
	salt := rand.bytes(16)!
	// tune the work factor for your deployment (OWASP suggests n >= 2^17 for password storage)
	key := scrypt.scrypt(password, salt, 131072, 8, 1, 32)!
	assert key.len == 32
}
```

### PBKDF2

`pbkdf2` derives a key from a password (RFC 8018). For password storage prefer a
memory-hard function such as `argon2` or `scrypt`; if you must use PBKDF2, use a high
iteration count and tune it for your hardware. OWASP currently recommends at least
600_000 iterations for PBKDF2-HMAC-SHA256.

```v
import crypto.pbkdf2
import crypto.rand
import crypto.sha256

fn main() {
	password := 'correct horse battery staple'.bytes()
	// generate and persist a unique random salt per password
	salt := rand.bytes(16)!
	// tune the iteration count for your deployment (OWASP suggests 600_000+ for SHA-256)
	key := pbkdf2.key(password, salt, 600_000, 32, sha256.new())!
	assert key.len == 32
}
```

# chacha20poly1305

Chacha20Poly1305 Authenticated Encryption with Additional Data (AEAD) module for V Language

This module provides authenticated encryption with additional data (AEAD) algorithm in V Language.
Its backed by experimental `x.crypto.chacha20` symmetric key stream cipher encryption
module and `x.crypto.poly1305` message authentication code (MAC) module.

> [!Warning]
> This is an absolutely experimental module, which is subject to change.
> Please use it carefully, thoroughly and wisely.

## Examples

```v
import encoding.hex
import x.crypto.chacha20poly1305

fn main() {
	// plaintext message to be encrypted and authenticated
	message := "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
		.bytes()

	// sets your secure random key
	key := hex.decode('808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f')!
	// give yours nonce
	nonce := hex.decode('070000004041424344454647')!
	// yours additional data
	aad := hex.decode('50515253c0c1c2c3c4c5c6c7')!

	// lets doing authenticated encryption
	ciphertext := chacha20poly1305.encrypt(message, key, nonce, aad)!

	// lets perform decryption back
	plaintext := chacha20poly1305.decrypt(ciphertext, key, nonce, aad)!

	assert plaintext == message
}
```

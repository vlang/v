# module chacha20

## chacha20

Chacha20 (and XChacha20) stream cipher encryption algorithm in pure V.
Its mostly based on [RFC 8439](https://datatracker.ietf.org/doc/html/rfc8439) and inspired by Go version of the same library.

## Examples

```v
module main

import encoding.hex
import x.crypto.chacha20

fn main() {
	// example of random key
	// you should make sure the key (and nonce) are random enough.
	// The security guarantees of the ChaCha20 require that the same nonce
	// value is never used twice with the same key.
	key := hex.decode('bf32a829ebf86d23f6a32a74ef0333401e54a6b2900d35bfadef82c5d49da15f')!
	nonce := hex.decode('a7d7cf3405631f25cc1054bd')!

	input := 'Good of gambler'.bytes()

	// encrypt and the decrypt back
	output := chacha20.encrypt(key, nonce, input)!
	input_back := chacha20.decrypt(key, nonce, output)!

	// should true
	assert input == input_back
}
```

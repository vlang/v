## ecdsa

`ecdsa` module for V language. Its a wrapper on top of openssl ecdsa functionality.
Its currently (expanded) to support the following curves:

- NIST P-256 curve, commonly referred as prime256v1 or secp256r1
- NIST P-384 curve, commonly referred as secp384r1 
- NIST P-521 curve, commonly referred as secp521r1
- A famous Bitcoin curve, commonly referred as secp256k1

# Warning
---------
This module defines symbol `OPENSSL_API_COMPAT` as 0x10100000L (or 1.1.0) for 
compatibility reason with rest of underlying low level OpenSSL API used here.
Its mostly has been deprecated in OpenSSL 3.0.
Unfortonately, standard `net.openssl` module also defines `OPENSSL_API_COMPAT` as `0x30000000L`
So, it would fail to build if you use both of the modules together, ie, `net.openssl` and `crypto.ecdsa`
under the same circumstance.

# Example
```v
import crypto.ecdsa

fn main() {
	// create default NIST P-256 secp256r1 curve key pair
	// If you wish to generate another curve, use: `pbkey, pvkey := ecdsa.generate_key(nid: .secp521r1)!`
	pbkey, pvkey := ecdsa.generate_key()!

	message_tobe_signed := 'Hello ecdsa'.bytes()
	// create a signature with the recommended hash
	signature := pvkey.sign(message_tobe_signed)!

	// verify the message with the signature
	verified := pbkey.verify(message_tobe_signed, signature)!
	dump(verified) // should be true

	// free allocated keys when you have done with your work.
	pbkey.free()
	pvkey.free()
}
```
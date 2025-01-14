## ecdsa

`ecdsa` module for V language. Its a wrapper on top of openssl ecdsa functionality.
Its currently (expanded) to support the following curves:

- NIST P-256 curve, commonly referred as prime256v1 or secp256r1
- NIST P-384 curve, commonly referred as secp384r1 
- NIST P-521 curve, commonly referred as secp521r1
- A famous Bitcoin curve, commonly referred as secp256k1

# Example
```codeblock
import crypto.ecdsa

fn main() {
	// create default NIST P-256 secp256r1 curve key pair
    // If you wish generate another curve, use following manner (or similar): `pbkey, pvkey := ecdsa.generate_key(nid: .secp521r1)!`
	pbkey, pvkey := ecdsa.generate_key()!

	message_tobe_signed := 'Hello ecdsa'.bytes()
	// create signature with recommended hash
	signature := pvkey.sign(message_tobe_signed, hash_config: .with_recommended_hash)!

	// verified the message with signature
	verified := pbkey.verify(message_tobe_signed, signature, hash_config: .with_recommended_hash)!
	dump(verified) // should be true
}
```
# mldsa

Pure V implementation of [ML-DSA](https://csrc.nist.gov/pubs/fips/204/final) (FIPS 204), a post-quantum digital signature algorithm. Supports all three parameter sets (ML-DSA-44, ML-DSA-65, ML-DSA-87).

> **This is still experimental**
> It is verified against NIST ACVP test vectors for [keygen](./nist_keygen_test.v), [signing](./nist_siggen_test.v), and [verification](./nist_sigver_test.v), but not yet production-ready.

## Example

```v
import x.crypto.mldsa

fn main() {
	// generate a new ML-DSA-65 key pair
	sk := mldsa.PrivateKey.generate(.ml_dsa_65)!
	pk := sk.public_key()

	// sign a message (with an optional context string)
	msg := 'Hello ML-DSA'.bytes()
	sig := sk.sign(msg, context: '')!

	// verify the signature with the same context
	verified := pk.verify(msg, sig, context: 'my-app')!
	assert verified // true

	// deterministic signing is also available
	sig2 := sk.sign(msg, context: 'my-app', deterministic: true)!
	verified2 := pk.verify(msg, sig2, context: 'my-app')!
	assert verified2 // true
}
```

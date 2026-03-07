# mldsa

`mldsa` is an implementation of NIST FIPS 204 Module-Lattice-Based Digital Signature
Standard (ML-DSA) in pure V. ML-DSA is a post-quantum digital signature scheme based
on module lattices. See the [NIST FIPS 204 Standard](https://csrc.nist.gov/pubs/fips/204/final)
for more detail.

It currently implements:
- Key generation from random seed or OS randomness
- Deterministic and randomized signing
- Signature verification
- Public key and private key serialization
- Context strings (up to 255 bytes)
- Three parameter sets: ML-DSA-44, ML-DSA-65, ML-DSA-87 (NIST security levels 2, 3, 5)

Even though this is completely experimental and **non-production ready**, the implmentation is verified against NIST ACVP test vectors for both keygen and signature verification. See [`mldsa_keygen_test.v`](./mldsa_keygen_test.v) and [`mldsa_sigver_test.v`](./mldsa_sigver_test.v)

## Example

```v
import x.crypto.mldsa

fn main() {
	// generate a new ML-DSA-65 key pair
	sk := mldsa.generate_key_65()!
	pk := sk.public_key()

	// sign a message (last param is an optional context string)
	msg := 'Hello ML-DSA'.bytes()
	sig := mldsa.sign(&sk, msg, 'my-app')!

	// verify the signature with the same context
	verified := mldsa.verify(pk, msg, sig, 'my-app')!
	assert verified // true

	// deterministic signing is also available
	sig2 := mldsa.sign_deterministic(&sk, msg, 'my-app')!
	verified2 := mldsa.verify(pk, msg, sig2, 'my-app')!
	assert verified2 // true
}
```

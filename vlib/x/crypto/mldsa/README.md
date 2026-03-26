# mldsa

`x.crypto.mldsa` wraps the OpenSSL 3.5 ML-DSA implementation defined by FIPS 204.

It supports:

- `ML-DSA-44`, `ML-DSA-65`, and `ML-DSA-87`
- random or seed-based key generation
- raw public/private key import and export
- one-shot sign and verify operations

## Requirement

This module requires OpenSSL 3.5 or newer.

## Example

```v ignore
import x.crypto.mldsa

fn main() {
	mut pv := mldsa.PrivateKey.new()!
	msg := 'ML-DSA example'.bytes()
	sig := pv.sign(msg)!

	mut pb := pv.public_key()!
	assert pb.verify(sig, msg)!

	pv.free()
	pb.free()
}
```

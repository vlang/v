## Description

`crypto.rsa` implements RSA signatures, as specified by
[RFC 8017](https://www.rfc-editor.org/rfc/rfc8017):

- RSASSA-PKCS1-v1_5, the scheme of the `RS256`, `RS384` and `RS512` JSON Web
  Signatures;
- RSASSA-PSS, with MGF1 over the same hash, the scheme of `PS256`, `PS384` and
  `PS512`.

Both work with SHA-256, SHA-384 and SHA-512. The module is written in V, with no
C dependency. Signing uses the Chinese remainder theorem, and checks each
signature with the public key before returning it.

Signing is written to run in constant time: it never branches on, or indexes
memory with, secret values. On amd64 and arm64 with GCC or Clang, an assembly
barrier keeps the C compiler from turning its masked selections back into
branches; with other compilers and architectures, this relies on the compiler.
Key generation and the consistency checks of `PrivateKey.new`, which also run
when a key is parsed, use `math.big`, which is not constant time: parse a key
once, and keep the `PrivateKey`.

Keys are read and written in the usual formats: PKCS #8 (`PRIVATE KEY`) and
PKCS #1 (`RSA PRIVATE KEY`) for private keys, X.509 SubjectPublicKeyInfo
(`PUBLIC KEY`) and PKCS #1 (`RSA PUBLIC KEY`) for public keys, in PEM or DER.
Moduli range from 2048 bits, the minimum RFC 7518 sets for JSON Web Signatures,
to 16384 bits.

For JSON Web Tokens signed with these algorithms, see `x.crypto.jwt`.

## Example

```v
import crypto.rsa
import crypto.sha256

fn main() {
	private_key := rsa.generate_key(2048)!
	public_key := private_key.public_key()

	digest := sha256.sum256('hello'.bytes())
	signature := private_key.sign_pss(.sha256, digest)!
	assert public_key.verify_pss(.sha256, digest, signature)!

	// Keys round-trip through PEM.
	pem := public_key.to_pem()
	assert rsa.PublicKey.from_pem(pem)!.equal(public_key)
}
```

## Signing and verifying

Signatures are computed over a digest, together with the hash that produced
it, which must be `.sha256`, `.sha384` or `.sha512`:

- `sign_pkcs1v15(hash, digest)` and `verify_pkcs1v15(hash, digest, signature)`;
  PKCS #1 v1.5 signatures are deterministic.
- `sign_pss(hash, digest, opts)` and `verify_pss(hash, digest, signature, opts)`;
  PSS signatures are salted with random bytes.

`verify_*` return `false` for a signature that does not match, and an error when
the hash is not supported or the digest has the wrong length.

`PSSOptions.salt_length` defaults to `rsa.pss_salt_length_equals_hash`, a salt
as long as the digest, which is what `PS256`, `PS384` and `PS512` require. A
positive value sets the length. `rsa.pss_salt_length_auto` signs with the
longest salt the key allows, and verifies signatures with any salt length.

## Keys

- `rsa.generate_key(bits)` creates a key with the public exponent 65537.
- `PrivateKey.from_pem`, `PrivateKey.from_der`, `PublicKey.from_pem` and
  `PublicKey.from_der` accept both formats of their kind. Encrypted PEM keys are
  not supported.
- `to_pem` and `to_der` write PKCS #8 and SubjectPublicKeyInfo; `to_pkcs1_der`
  writes PKCS #1.
- `PublicKey.new(modulus, exponent)` and `PrivateKey.new(components)` build keys
  from their numbers, as big-endian bytes, for instance from a JSON Web Key.
  `PrivateKey.new` checks that the numbers are consistent, and computes `dp`,
  `dq` and `qinv` when they are missing.

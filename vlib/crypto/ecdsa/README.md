## ecdsa

`ecdsa` module for V language. Its currently (expanded) to support the
following curves:

- NIST P-256 curve, commonly referred as prime256v1 or secp256r1
- NIST P-384 curve, commonly referred as secp384r1 
- NIST P-521 curve, commonly referred as secp521r1
- A famous Bitcoin curve, commonly referred as secp256k1

## Backends

`crypto.ecdsa` has two independent, interchangeable backends -- one is
compiled in, never both, selected by the presence of the `-d use_openssl`
build flag:

- **mbedTLS (default, `-d use_openssl` absent)**: uses the mbedTLS library
  already vendored under `thirdparty/mbedtls` for this repo's own TLS 1.3/
  QUIC stack, so it needs no external dependency to build or run. This is
  the backend most callers get automatically.
- **OpenSSL (opt-in, `-d use_openssl` present)**: uses the system's OpenSSL
  development headers/libraries. Requires OpenSSL to be installed and
  discoverable by the C compiler.

Both backends implement the same public API (`generate_key`,
`new_key_from_seed`, `PrivateKey.new`, `.sign`, `.bytes`, `.public_key`,
`.derive_shared_secret`, `.equal`, `.free`, `PublicKey.verify`, `.equal`,
`.free`, `.uncompressed_bytes`, `PublicKey.from_uncompressed_bytes`, and the
PEM/DER loaders `pubkey_from_bytes`, `pubkey_from_string`,
`privkey_from_string`), so
calling code compiles unchanged regardless of which backend is selected.

One documented difference: `new_key_from_seed` on the mbedTLS backend
rejects a seed whose big-endian value is outside `[1, curve_order-1]`
(mbedTLS's `mbedtls_ecp_read_key` range-checks the scalar), while the
OpenSSL backend performs no such check. Every seed that `PrivateKey.bytes()`
produces is in range, so a key saved with either backend reloads with both.

On the mbedTLS backend the PEM/DER loaders accept an unencrypted SEC1
(`EC PRIVATE KEY`) or PKCS#8 (`PRIVATE KEY`) private key, and a
SubjectPublicKeyInfo public key, on the curves `Nid` names; any other key
type or curve is refused with an error.

**The default mbedTLS backend does not (yet) implement everything the
OpenSSL backend does** -- this returns a clear, informative error instead of
silently behaving differently, and is only available under
`-d use_openssl` today:

- `PrivateKey.sign()`/`sign_with_options()` with
  `hash_config: .with_custom_hash` -- mbedTLS's deterministic-nonce signing
  (RFC 6979, compiled in by default) needs a real digest-algorithm
  identifier to seed its internal HMAC-DRBG, and an arbitrary
  caller-supplied `hash.Hash` carries no such identifier. Verifying a
  signature with a custom hash is unaffected (mbedTLS's own verify call
  takes no digest-algorithm parameter at all).

# Example
```v
import crypto.ecdsa

fn main() {
	// create default NIST P-256 secp256r1 curve key pair. If you wish to generate another curve,
	// use: `pbkey, pvkey := ecdsa.generate_key(nid: .secp521r1)!` instead.
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
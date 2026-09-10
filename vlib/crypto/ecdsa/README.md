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
`PrivateKey.new`, `.sign`, `.public_key`, `.derive_shared_secret`, `.equal`,
`.free`, `PublicKey.verify`, `.equal`, `.free`, `.uncompressed_bytes`,
`PublicKey.from_uncompressed_bytes`), so calling code compiles unchanged
regardless of which backend is selected.

**The default mbedTLS backend does not (yet) implement everything the
OpenSSL backend does** -- these return a clear, informative error instead of
silently behaving differently, and are only available under
`-d use_openssl` today:

- `new_key_from_seed` -- OpenSSL's current implementation performs no range
  check against the curve order; mbedTLS's closest equivalent validates the
  resulting scalar is in `[1, curve_order-1]` and rejects it otherwise. A
  seed that "works" under `-d use_openssl` could be rejected here, so this
  is a genuine behavioral divergence, not just missing plumbing, and needs
  its own design pass before porting.
- `PrivateKey.sign()`/`sign_with_options()` with
  `hash_config: .with_custom_hash` -- mbedTLS's deterministic-nonce signing
  (RFC 6979, compiled in by default) needs a real digest-algorithm
  identifier to seed its internal HMAC-DRBG, and an arbitrary
  caller-supplied `hash.Hash` carries no such identifier. Verifying a
  signature with a custom hash is unaffected (mbedTLS's own verify call
  takes no digest-algorithm parameter at all).
- `pubkey_from_bytes`, `pubkey_from_string`, `privkey_from_string` (PEM/DER
  loading) -- format-parsing work not yet ported; no caller in this repo
  currently needs them on the default backend.

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
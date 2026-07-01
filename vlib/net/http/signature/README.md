# `net.http.signature` — HTTP Message Signatures (RFC 9421)

Sign and verify HTTP requests and responses per [RFC 9421][rfc9421] —
the standard that replaces the long-running `Signature` /
`Signature-Input` drafts and underpins production deployments at major
CDNs, mTLS proxies, mutual API authentication, and the upcoming
[Web Bot Auth][web-bot-auth] work.

[rfc9421]: https://www.rfc-editor.org/rfc/rfc9421.html
[web-bot-auth]: https://datatracker.ietf.org/doc/draft-meunier-web-bot-auth-architecture/

## Quick start

```v ignore
import net.http
import net.http.signature

// Sign an outbound request. `created` defaults to time.now().unix().
mut req := http.new_request(.post, 'https://example.com/items', '{}')!
req.header.add_custom('Date', 'Tue, 20 Apr 2021 02:07:55 GMT')!
req.header.add_custom('Content-Type', 'application/json')!

priv := signature.Key.from_pem(alice_private_pem)!.with_keyid('alice')
signature.sign_request(mut req, priv,
    components: ['@method', '@target-uri', '@authority', 'date', 'content-type']
)!
// req now carries Signature-Input and Signature header fields.

// On the receiving side, verify with the public key resolved from `keyid`:
pub_key := signature.Key.from_pem(alice_public_pem)!
signature.verify_request(req, pub_key, now_unix: time.now().unix())!
```

`Key.from_pem` accepts the canonical PKCS#8 / SPKI / SEC1 PEM blocks
that `openssl genpkey` and friends produce. The raw-coordinate
constructors (`Key.ed25519_private(seed)`, `Key.ecdsa_p256_public(x,
y)`, …) are still available when you have JWK-style key material.

The `now_unix` option enforces the optional `expires` parameter; pass
`0` (the default) to skip the expiry check.

## Algorithms

| IANA name           | Status | Backed by |
| ------------------- | ------ | --------- |
| `hmac-sha256`       | ✅ | `crypto.hmac` + `crypto.sha256` |
| `ecdsa-p256-sha256` | ✅ | `crypto.ecdsa` (P-256) |
| `ecdsa-p384-sha384` | ✅ | `crypto.ecdsa` (P-384) |
| `ed25519`           | ✅ | `crypto.ed25519` |

`rsa-pss-sha512` and `rsa-v1_5-sha256` are intentionally out of scope —
`vlib/crypto` does not yet ship an RSA implementation. Adding them is
mechanical once it does.

## Covered components

All derived components from RFC 9421 §2.2 are implemented:

`@method`, `@target-uri`, `@authority`, `@scheme`, `@request-target`,
`@path`, `@query`, `@status`.

Plain HTTP fields are matched by *lowercased* field name, with
multi-value fields joined by `", "` and OWS trimmed at the boundaries
(RFC 9421 §2.1).

`@query-param` (RFC 9421 §2.2.8), structured-field re-serialisation
(`sf`, `key`, `bs` parameters from §2.1.x), and binary-wrapped fields
are deferred to a follow-up PR.

## Two API layers

```v ignore
// Components-level - works on any HTTP-shaped data, no http.Request
// dependency. Use this offline (signing fixtures, building tests).
base := signature.signature_base_string(components, params)!
out  := signature.sign(components, params, key, 'sig1')!
signature.verify(components, sig_input, sig_value, 'sig1', key)!

// http.Request / http.Response wrappers - sugar over the above.
signature.sign_request(mut req, key, components: [...], created: now)!
signature.verify_request(req, key, now_unix: now)!
```

## Design notes

* **No silent algorithm fallbacks.** If you set the `alg` parameter
  and it doesn't match the key's algorithm, `sign` errors out with
  `MalformedMessage`. `verify` does the same on the inbound side.
  RFC 9421 §3.1 step 3 makes this a correctness requirement.

* **Empty `keyid` is allowed.** RFC 9421 doesn't make `keyid`
  mandatory; some out-of-band channel (mTLS cert, JWT bearer)
  identifies the signer instead. `sign` still emits a usable
  signature; the verifier picks the key by other means.

* **Multiple signatures coexist.** Calling `sign_request` twice with
  different labels merges the labelled entries into a single
  `Signature-Input` / `Signature` field per RFC 8941 §3.2 (comma-
  separated dictionary) — TLS-terminating proxies and federated
  signing scenarios both rely on this layout.

* **No clock dependency.** Both `created` and the expiry check are
  driven by the caller (`opts.created`, `opts.now_unix`). Signing in
  bulk over historical data, deterministic test runs, and replay
  protection are all the caller's concern.

## Test vectors

RFC 9421 Appendix B vectors are vendored under
`tests/rfc9421/` and exercised by `rfc9421_test.v`:

| Section | Algorithm | Mode |
| --- | --- | --- |
| B.2.5 | `hmac-sha256` | **bytes-exact** |
| B.2.6 | `ed25519` | **bytes-exact** |
| B.2.4 | `ecdsa-p256-sha256` | verify (ECDSA non-deterministic) |

`http_message_test.v` covers sign/verify roundtrips across all four
supported algorithms (including a freshly-generated P-384 key),
tampered URL rejection, missing-header rejection, expiry enforcement,
multi-signature coexistence, and `alg` / label validation.
`structured_field_test.v` pins the Inner List + parameter
serialisation, multi-value field joining, OWS trimming, and the
`@query` empty-vs-present semantics.

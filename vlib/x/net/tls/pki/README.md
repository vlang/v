# tls.pki

X.509 / ASN.1 decoding and RFC 5280 certificate-chain validation for the `tls` client. This
module answers one question — *can this certificate list be trusted for this hostname?* — and
nothing else in the stack re-decides it. It is built from V's `crypto` packages and this module's
own DER reader; there are no OpenSSL bindings.

This README summarizes the module's public entry points, layout, and security posture.

## Public entry points

```v
import x.net.tls.pki

fn verify_server(chain []pki.Certificate, hostname string) ! {
	store := pki.system_root_cas()!
	validator := pki.new_validator(store)
	validator.verify_certificate_chain(chain, hostname)!
}

fn main() {}
```

- `parse_certificate(der []u8) !Certificate` — decode one certificate.
- `Validator` + `new_validator(store)` / `new_insecure_validator()` / `default_validator()` and
  `verify_certificate_chain(chain, hostname)`.
- `TrustStore` + `new_trust_store()` / `add_der` / `add_pem` / `load_system_ca_bundle` /
  `system_root_cas()` (process-wide cached roots).
- `is_ip_literal(host)` — the IPv4/IPv6 classifier the matcher uses.

`new_insecure_validator()` is the only way to turn verification off, and it is a separate
constructor so the dangerous intent is spelled out at the call site rather than hidden in a flag.

## File map

```
asn1.v         bounds-checked ASN.1/DER element reader (tags, lengths, SEQUENCE)
x509.v         X.509 v3 certificate + extension decoding (SAN, basicConstraints,
               keyUsage, EKU, nameConstraints)
validator.v    chain building, path validation, trust anchoring, signature dispatch
hostname.v     RFC 6125 presentation-identifier matching (dNSName/CN/IP)
rsa.v          RSASSA-PKCS1-v1_5 and RSASSA-PSS chain-signature verification
trust_store.v  system trust bundle loading + process-wide cached roots
```

Each `*_test.v` sits beside its source. `pki_test.v` holds validator- and helper-level tests;
`x509_test.v`, `asn1_test.v`, and `rsa_test.v` cover the parsers.

## Validation rules enforced

- **Path building** to a trusted root (byte-identical anchor or a signature that verifies under
  a stored root); every issuer name must match the child's subject; the anchor must be a CA
  (`basicConstraints cA=TRUE`).
- **Validity windows** on the leaf and every intermediate (RFC 5280 §4.1.2.5).
- **Key usage**: leaf must permit `digitalSignature`; every CA in the path must permit
  `keyCertSign` when it asserts a `keyUsage` (RFC 5280 §4.2.1.3).
- **Extended key usage**: when present on the leaf or an issuer in the peer chain,
  it must allow `serverAuth`; absence is unconstrained (RFC 5280 §4.2.1.12).
- **pathLenConstraint** (RFC 5280 §4.2.1.9) counted against intermediates below each constrained
  CA; a malformed INTEGER never maps to the unlimited sentinel.
- **Name constraints** (RFC 5280 §4.2.1.10): each CA binds every subordinate beneath it; implicit
  subtree tagging is decoded. Permitted subtrees are one namespace across dNSName and iPAddress.
- **Hostname matching** (RFC 6125 §6.4.3/§6.4.4): IP literals match iPAddress SANs only; a present
  SAN disables the CN; single-label wildcards; CN fallback only when no SAN list exists.
- **Signature schemes**: RSA-PSS (SHA-256/384/512, RFC 4055 §3.1 refuses any other hash rather
  than guessing), RSA-PKCS1-v1_5 (SHA-256/384/512), ECDSA, and Ed25519. SHA-1 and unknown OIDs
  are rejected, never downgraded to.
- **RSA key floors**: leaf ≥ 2048 bits (adjustable via `Validator.set_min_rsa_bits`, clamped to
  `min_rsa_bits_floor`), CA keys ≥ 2048 regardless of store contents.

## Fail-closed posture

This module treats parsing as the attack surface it is:

- `parse_certificate` bounds the accepted DER size (`max_certificate_der_bytes`) before touching
  the ASN.1 walker, so no entry point (handshake, `add_der`, `add_pem`) needs its own bound.
- Malformed or unsupported extensions fail the whole decode rather than being silently dropped —
  an empty constraint set must never exempt every name.
- `verify_cert_signature` returns `!` (no `bool`), so a caller cannot silently discard a
  "verification failed" result as success.
- `trust_store.v` loads PEM bundles through `crypto.pem`, isolating each block on the
  `-----BEGIN ` marker so one corrupt envelope costs only itself, and fails closed on bundle-size
  / block-count caps so a partially-loaded trust store never looks healthy.

## Revocation

CRL and OCSP are **not** implemented: a revoked-but-unexpired certificate is accepted. A caller
needing revocation must validate out of band before trusting the connection.

## Testing

```sh
./v -silent test vlib/x/net/tls/pki/
```

Covers OID edge inputs, hand-built v3 certificate fixtures, the element-count truncation guards,
duplicate/critical extension rules, name-constraint subtree enforcement, hostname/IP matching
(including the SAN-vs-CN precedence rule), the RSA exponent policy and key floors, trust-anchor
placement, and the fail-closed caps above.

# crypto.x509

`parse_pem(text)!` skips other PEM blocks and reads the first `CERTIFICATE` block.
`parse_der(bytes)!` reads one
complete DER certificate and returns its version, serial, issuer and subject attribute
maps, validity times, DNS and IP subject alternative names, and original bytes.
Attribute map
keys are dotted OIDs; `common_name()` and `organization()` retrieve common subject values.
Repeated attributes use the last value. Unsupported directory
string types are skipped. Common UTF-8, ASCII, and BMP directory strings are decoded;
TeletexString is skipped.

The parser checks certificate structure, bounded DER lengths, duplicate extensions, and
calendar/time encodings. Both version 2 unique identifiers and version 3 extensions are
handled. Parsing does not validate a certificate's signature, trust chain, validity period
or key usage. Unknown **non-critical** extensions are skipped; unknown **critical**
extensions are rejected (fail-closed).

`certificate.check_host(name)!` checks an ASCII DNS hostname, ignoring case and accepting
a final dot. A wildcard matches exactly one leftmost label. SAN identities take precedence;
if a SAN extension is present without DNS names, matching fails. When SAN is absent, the
subject common name is accepted for legacy compatibility. This CN fallback is not the
modern SAN-only TLS policy. IP reference identities are rejected; IP SANs are parsed and
available as `ip_addresses` (raw 4/16-byte addresses) but never satisfy `check_host`.
Use IDNA ASCII names for international domains.

## Chain building and validation

`build_chain(leaf, intermediates, roots)!` assembles the longest valid path from the leaf
through intermediates to a trust anchor. The direct-root fast path (leaf IS a root, or is
issued directly by one) is tried before the depth-first search, so single-level deployments
pay nothing. Every edge enforces issuer/subject DER equality, CA status (`CA:TRUE` on
non-anchor intermediates), `keyCertSign` key usage, validity windows, signature
verification (ECDSA/RSA/Ed25519/ML-DSA/SLH-DSA-SHAKE; SHA2 sets fail closed), name
constraints and `pathLenConstraint`. Cross-signed intermediates are explored; the longest
valid path wins. Trust anchors themselves are exempt from the CA flag only.

`verify_chain(chain, opts)!` validates an assembled chain (leaf first): validity at
`opts.at` (zero means now), leaf EKU per usage (`server_auth` needs
`1.3.6.1.5.5.7.3.1`, `client_auth` needs `1.3.6.1.5.5.7.3.2`; absent EKU or
`anyExtendedKeyUsage` passes either), leaf `digitalSignature` key usage when the
extension is present, hostname via `check_host` for server auth, CRL revocation for every
non-anchor certificate, and an optional OCSP staple (`opts.ocsp_resp`).

## Revocation (CRL)

`parse_crl_der` / `parse_crl_pem` read CRLs (unknown critical extensions fail closed);
`verify_crl(crl, issuer)!` checks issuer match, the validity window, `cRLSign` key usage
and the signature; `crl.is_revoked(serial)` does serial lookup (sign-byte tolerant).
`make_crl(issuer, opts, signer)!` builds and signs a CRL carrying CRLNumber and
authorityKeyId with any CA key type (`CertSigner`: ECDSA, RSA PKCS#1 v1.5, Ed25519,
ML-DSA, SLH-DSA-SHAKE).

No `Config` fields are added for revocation inputs. CRLs are read from the environment,
mirroring the `SSL_CERT_FILE` / `SSL_CERT_DIR` pattern used for trust roots
(see `net.tls`): `SSL_CRL_FILE` (explicit bundle of `X509 CRL` PEM blocks or DER),
`SSL_CRL_DIR` (directory scan of `*.crl`/`*.pem`/`*.der`), plus the directory containing
`SSL_CERT_FILE` (same-dir convention). Unparsable files are skipped; an empty result
means "no revocation data", not an error. Use `load_system_crls()` and pass the result
as `VerifyOptions.crls`.

## Revocation (OCSP stapling)

The TLS transport supplies the staple bytes (status_request is owned elsewhere); this
module only verifies bytes. `verify_ocsp_staple(resp_der, leaf_der, issuer_der)!` parses
the OCSPResponse, requires `successful` status and the BasicOCSPResponse type, checks the
CA-as-responder identity (`responderID` must name the issuer), the response signature
under the issuer key, `producedAt`/`thisUpdate`/`nextUpdate` freshness (±15 minutes
skew), certID hash binding (SHA-1/SHA-256) and a `good` certStatus for the leaf serial.
`make_ocsp_response(leaf, issuer, opts, signer)!` builds signed fixtures for tests.

## Name constraints, policies, issuance

Name constraints enforce permitted/excluded subtrees for `dNSName` (RFC 5280 §4.2.1.10
matching) and `iP` (address++mask ranges) against SAN identities. Other GeneralName bases
in constraints fail closed. `certificatePolicies` and `policyConstraints` are parsed as
known extensions but not enforced (anyPolicy passthrough).

Issued certificates always carry `subjectKeyIdentifier` and `authorityKeyIdentifier`
(self for self-signed), and CAs additionally carry critical `basicConstraints`.
`CertGenOptions` accepts `key_usage`/`ext_key_usage` (keyUsage is critical, EKU is not),
name-constraint subtrees and raw IP SANs. `crlDistributionPoints` and
`authorityInfoAccess` (OCSP URLs) are parsed into URI lists but never fetched.

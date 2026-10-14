# crypto.x509

`parse_pem(text)!` skips other PEM blocks and reads the first `CERTIFICATE` block.
`parse_der(bytes)!` reads one
complete DER certificate and returns its version, serial, issuer and subject attribute
maps, validity times, DNS subject alternative names, and original bytes. Attribute map
keys are dotted OIDs; `common_name()` and `organization()` retrieve common subject values.
Repeated attributes use the last value. Unknown extensions and unsupported directory
string types are skipped. Common UTF-8, ASCII, and BMP directory strings are decoded;
TeletexString is skipped.

The parser checks certificate structure, bounded DER lengths, duplicate extensions, and
calendar/time encodings. Both version 2 unique identifiers and version 3 extensions are
handled. Parsing does not validate a certificate's signature, trust chain, validity period,
key usage, or critical extensions.

`certificate.check_host(name)!` checks an ASCII DNS hostname, ignoring case and accepting
a final dot. A wildcard matches exactly one leftmost label. SAN identities take precedence;
if a SAN extension is present without DNS names, matching fails. When SAN is absent, the
subject common name is accepted for legacy compatibility. This CN fallback is not the
modern SAN-only TLS policy. IP reference identities are rejected; IP SAN support and full
certificate verification are not provided. Use IDNA ASCII names for international domains.

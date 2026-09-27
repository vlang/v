# QUIC-scoped TLS 1.3 test vectors — real reference-implementation capture

RFC 8448 provides a full worked TLS 1.3 example, but it predates QUIC and
has no `quic_transport_parameters` extension, no QUIC framing, and no QUIC
key-schedule labels — it isn't directly reusable for this module (it *is*
still used, separately, as the source for the key-schedule math itself,
cross-checked in `tls13_keyschedule_test.v`). This directory instead
captures a **real handshake between two independent reference
implementations**, to validate this module's message *parsing* and its
`compute_finished_verify_data`/`verify_finished`/CertificateVerify-signature
functions against real, independently-produced wire bytes — not just
hand-constructed or self-round-tripped ones.

## Provenance

- **Client + server implementation**: [Cloudflare quiche](https://github.com/cloudflare/quiche),
  via the `quic-interop-runner` project's published Docker image
  `cloudflare/quiche-qns:latest`
  (`sha256:7ac69e8f413e5c913421686754d7c632c1c15dc787f1f1e3b1986b8fb93aafd1`,
  image build timestamp `2026-07-19T12:33:28Z`). Image reference sourced
  from quic-interop-runner's own
  [`implementations_quic.json`](https://github.com/quic-interop/quic-interop-runner/blob/master/implementations_quic.json)
  (accessed 2026-07-20), not guessed.
- **Capture date**: 2026-07-20.
- **Capture tool**: `tcpdump` (bundled in the quiche-qns image) writing a
  pcap on the loopback/bridge interface; `SSLKEYLOGFILE` (an undocumented
  but functional env var quiche honors — confirmed empirically, not found
  in `quiche-server --help`/`quiche-client --help`) exported the standard
  NSS/QUIC keylog lines.
- **Extraction tool**: [Wireshark](https://www.wireshark.org/) `tshark`
  4.6.6 (via the `nicolaka/netshoot:latest` Docker image), decrypting the
  capture using the keylog and dumping the dissected tree as PDML.
  `extract_handshake.py` (this directory) parses the PDML and reconstructs
  each TLS handshake message's exact raw bytes, verified against tshark's
  own independently-computed `tls.handshake` `size` attribute for every
  single message (see "Extraction correctness" below).
- **Server certificate/key**: freshly generated for this capture only
  (`openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1
  ...`, ECDSA P-256, self-signed, 30-day validity from the capture date).
  Not a real-world CA-issued certificate; not meant to validate chain
  trust (this repo has no CA-flagged test certificate at all — see
  `tls13_certificate_chain_test.v`'s own note for that separate, pre-existing
  limitation). `quiche_p256_handshake_server_cert.pem` is the exact PEM
  used; the matching private key was not retained (only the CA-independent
  public-key material embedded in the leaf certificate is needed by
  anything downstream).

## What this validates, and what it doesn't

The keylog format (NSS Key Log Format, extended for QUIC) exports the
*already-derived* per-level traffic secrets
(`CLIENT_HANDSHAKE_TRAFFIC_SECRET`, `SERVER_HANDSHAKE_TRAFFIC_SECRET`,
`CLIENT_TRAFFIC_SECRET_0`, `SERVER_TRAFFIC_SECRET_0`) — not the raw ECDHE
shared secret, nor the intermediate Early/Handshake Secret HKDF-Extract
outputs. This is a deliberate NSS/TLS convention, not a gap in this
capture. As a result:

- **Message parsing** (`parse_handshake_message`, `parse_server_hello`,
  `parse_encrypted_extensions`, `parse_certificate`,
  `parse_certificate_verify`, `decode_transport_parameters`) is exercised
  against real bytes from an independent implementation — a genuinely new
  form of validation this module didn't have before, since every other
  existing test either hand-constructs wire bytes or round-trips this
  module's own encoder against its own decoder.
- **CertificateVerify signature verification** is validated against a
  REAL ECDSA P-256 signature (`sig_scheme_ecdsa_secp256r1_sha256`) —
  this closes a real, previously-documented gap: no EC private key exists
  anywhere in this repo, so `net.mbedtls/x509_standalone_signature_test.v`
  could only test the ECDSA code path via a key-type-mismatch rejection,
  never a genuine accepted ECDSA signature. This capture provides that
  missing positive case.
- **Finished MAC verification** (`compute_finished_verify_data`/
  `verify_finished`) is validated in both directions, using the keylog's
  real `CLIENT_HANDSHAKE_TRAFFIC_SECRET`/`SERVER_HANDSHAKE_TRAFFIC_SECRET`
  against the real captured Finished message bytes.
- **NOT validated by this capture**: `derive_early_secret`,
  `derive_handshake_secrets`, and `derive_application_secrets`'s own
  HKDF-Extract/HKDF-Expand-Label math (the Early Secret → Handshake
  Secret → traffic-secret chain) — that would need the raw ECDHE shared
  secret, which this capture method cannot recover (recovering it would
  need instrumenting quiche's own source, not just observing its wire
  traffic and keylog). That chain is already independently
  cross-validated against RFC 8448's own official worked values in
  `tls13_keyschedule_test.v`, via a different, complementary form of
  independent validation (a published spec worked example, not a live
  capture) — this is not a coverage gap, just two different tools each
  covering the part they're suited for.

## Extraction correctness

`extract_handshake.py`'s reconstruction is checked against tshark's own
independently-reported `tls.handshake` `size` attribute for every single
message (see the script's own printed `OK`/`MISMATCH` output) — this
caught two real bugs in the extraction script itself before it was
trusted (Wireshark's dissection tree shows some field spans twice, once
as a raw value and once under a friendly/decoded alias field covering the
identical byte range; and a field that has BOTH its own raw `value=` and a
child `_ws.expert` annotation node needs its own value captured, not
skipped in favor of recursing into the annotation). Every message in this
capture reconstructs to exactly tshark's own reported size. The final
proof is `tls13_quiche_vector_test.v` (in this module's parent directory)
successfully parsing every message and passing every cross-check listed
above — a byte-count match alone does not prove the bytes are in the
right order, only the full parse + signature/MAC verification does that.

## Reproducing this capture

```
docker pull cloudflare/quiche-qns:latest
# generate a fresh self-signed P-256 cert (see openssl command above)
# start quiche-server with tcpdump + SSLKEYLOGFILE, on a docker network
# run quiche-client against it requesting a small file over h3
# tshark -r capture.pcap -o "tls.keylog_file:keys.log" -Y quic -T pdml > out.pdml
python extract_handshake.py   # reconstructs handshake_messages.txt from out.pdml
```

## Files

- `quiche_p256_handshake.pcap` — raw tcpdump capture (loopback/bridge,
  UDP port 4433 only).
- `quiche_p256_handshake.keylog` — NSS/QUIC keylog (client and server
  sides independently confirmed identical before trusting either).
- `quiche_p256_handshake_server_cert.pem` — the server's self-signed
  leaf certificate used in the capture.
- `extract_handshake.py` — PDML → raw handshake-message-bytes extractor,
  kept for reproducibility (mirrors `crypto/blake2b/testdata/`'s own
  `.awk`-script convention for documenting exactly how embedded test
  constants were derived from raw fixture data).

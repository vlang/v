# TLS fuzz corpus (deterministic seeds)

Fixed seeds for `vlib/net/tls/fuzz_test.v` (`test_fuzz_corpus_no_panic`).
Every file is exercised both verbatim (must parse) and under
single-byte mutations (must fail closed, never panic).

| file                      | content                                   | parser                     |
|---------------------------|-------------------------------------------|----------------------------|
| `client_hello_seed.hex`   | hex of a minimal valid ClientHello body   | `parse_client_hello`       |
| `server_hello_seed.hex`   | hex of a minimal valid ServerHello body   | `parse_server_hello`       |
| `plain_record_seed.hex`   | hex of a TLSPlaintext ClientHello record  | `decode_plain_record`      |
| `nst_seed.hex`            | hex of a NewSessionTicket body            | `parse_new_session_ticket` |
| `ee_seed.hex`             | hex of an EncryptedExtensions body        | `ee_accepts_early_data`    |
| `handshake_ch_seed.hex`   | hex of a ClientHello handshake message    | `parse_handshake_message`  |
| `handshake_sh_seed.hex`   | hex of a ServerHello handshake message    | `parse_handshake_message`  |
| `pem_seed.pem`            | minimal valid CERTIFICATE block           | `pem.decode`               |

Regenerating: the `*.hex` seeds are written by the `genseeds.v` helper
(kept outside the repo, in the assurance TMPDIR) which uses the `net.tls`
encoders; `pem_seed.pem` is written by the `genpem.v` helper via
`crypto.pem` encoding (self-decode verified). Seeds are checked in so runs
stay deterministic without network access.

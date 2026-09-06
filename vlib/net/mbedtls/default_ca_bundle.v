module mbedtls

// default_ca_bundle_pem is a maintained snapshot of Mozilla's trusted root
// CA certificates (curl's ca-bundle distribution,
// https://curl.se/docs/caextract.html), vendored into
// thirdparty/cacert/cacert.pem and embedded into the binary at compile
// time — matching how thirdparty/mbedtls itself is vendored.
//
// Used as the trust anchor for OUTBOUND (client) TLS connections whenever
// a caller does not supply their own CA bundle/file: see SSLConn.init's
// client-only cert-loading block (ssl_connection.c.v) and
// verify_certificate_chain (x509_standalone.c.v, net.quic's HTTP/3 client).
// Deliberately NOT wired into SSLListener.init (also ssl_connection.c.v):
// that path's `verify` means "the CA that authenticates an mTLS CLIENT
// certificate", a fundamentally different, server-specific trust decision
// this public root store must never silently satisfy.
pub const default_ca_bundle_pem = $embed_file('../../../thirdparty/cacert/cacert.pem').to_string()

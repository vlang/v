module openssl

// SSL_get_peer1_certificate not defined in LibreSSL (OpenSSL fork) on OpenBSD,
// use SSL_get_peer_certificate instead.
fn C.SSL_get_peer_certificate(ssl &SSL) &C.X509

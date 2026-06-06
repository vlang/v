// Match the init API to the OpenSSL headers that are actually available.
#if defined(LIBRESSL_VERSION_NUMBER) || !defined(OPENSSL_VERSION_NUMBER) \
	|| OPENSSL_VERSION_NUMBER < 0x10100000L
static int v_net_openssl_init_ssl(void) {
	SSL_load_error_strings();
	return SSL_library_init();
}
#else
static int v_net_openssl_init_ssl(void) {
	return OPENSSL_init_ssl(OPENSSL_INIT_LOAD_SSL_STRINGS, 0);
}
#endif

// SSL_get1_peer_certificate is only available in OpenSSL 3.x.
#if defined(LIBRESSL_VERSION_NUMBER) || !defined(OPENSSL_VERSION_NUMBER) \
	|| OPENSSL_VERSION_NUMBER < 0x30000000L
static X509 *v_net_openssl_get1_peer_certificate(SSL *ssl) {
	return SSL_get_peer_certificate(ssl);
}
#else
static X509 *v_net_openssl_get1_peer_certificate(SSL *ssl) {
	return SSL_get1_peer_certificate(ssl);
}
#endif

// ALPN (SSL_set_alpn_protos / SSL_get0_alpn_selected) is only available in
// OpenSSL 1.0.2 and later. On older OpenSSL-compatible headers, fall back to
// no-op shims so the module still links; ALPN is simply unavailable there.
// LibreSSL reports a high OPENSSL_VERSION_NUMBER and provides ALPN, so it uses
// the native path below.
#if !defined(OPENSSL_VERSION_NUMBER) || OPENSSL_VERSION_NUMBER < 0x10002000L
static int v_net_openssl_set_alpn_protos(SSL *ssl, const unsigned char *protos, unsigned int protos_len) {
	(void)ssl;
	(void)protos;
	(void)protos_len;
	return -1; // ALPN unsupported on this OpenSSL version
}
static void v_net_openssl_get0_alpn_selected(SSL *ssl, const unsigned char **data, unsigned int *len) {
	(void)ssl;
	*data = NULL;
	*len = 0;
}
#else
static int v_net_openssl_set_alpn_protos(SSL *ssl, const unsigned char *protos, unsigned int protos_len) {
	return SSL_set_alpn_protos(ssl, protos, protos_len);
}
static void v_net_openssl_get0_alpn_selected(SSL *ssl, const unsigned char **data, unsigned int *len) {
	SSL_get0_alpn_selected(ssl, data, len);
}
#endif

// LibreSSL and older OpenSSL-compatible headers may not expose the async
// SSL_ERROR constants, but V's SSLError enum needs stable values for them.
#ifndef SSL_ERROR_WANT_ASYNC
#define SSL_ERROR_WANT_ASYNC 9
#endif

#ifndef SSL_ERROR_WANT_ASYNC_JOB
#define SSL_ERROR_WANT_ASYNC_JOB 10
#endif

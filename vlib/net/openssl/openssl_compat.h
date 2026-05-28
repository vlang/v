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

// LibreSSL and older OpenSSL-compatible headers may not expose the async
// SSL_ERROR constants, but V's SSLError enum needs stable values for them.
#ifndef SSL_ERROR_WANT_ASYNC
#define SSL_ERROR_WANT_ASYNC 9
#endif

#ifndef SSL_ERROR_WANT_ASYNC_JOB
#define SSL_ERROR_WANT_ASYNC_JOB 10
#endif

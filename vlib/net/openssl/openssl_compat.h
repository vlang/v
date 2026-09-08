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

// SSL_CTX_set_verify is a macro on some OpenSSL-compatible versions and its
// callback type is awkward to express through V's C interop. Keep the exact
// peer-verification setup behind a stable, version-independent shim.
static int v_net_openssl_configure_peer_verification(SSL_CTX *ctx, int load_default_paths) {
	SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
	if (load_default_paths) {
		return SSL_CTX_set_default_verify_paths(ctx);
	}
	return 1;
}

// Chain verification does not verify that the certificate belongs to the
// requested server. Configure OpenSSL's built-in IP SAN or hostname check
// before the handshake where that API is available. Fail closed on older
// versions: validation must never silently omit identity verification.
#if defined(LIBRESSL_VERSION_NUMBER) || (defined(OPENSSL_VERSION_NUMBER) \
	&& OPENSSL_VERSION_NUMBER >= 0x10002000L)
static int v_net_openssl_configure_peer_name_verification(SSL *ssl, const char *hostname) {
	X509_VERIFY_PARAM *param = SSL_get0_param(ssl);
	if (param == NULL) {
		return 0;
	}
	if (X509_VERIFY_PARAM_set1_ip_asc(param, hostname) == 1) {
		return 1;
	}
	return X509_VERIFY_PARAM_set1_host(param, hostname, 0);
}
#else
static int v_net_openssl_configure_peer_name_verification(SSL *ssl, const char *hostname) {
	(void)ssl;
	(void)hostname;
	return 0;
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

// X509_check_host and X509_check_ip_asc were added in OpenSSL 1.0.2. Keep
// older builds linkable and report that identity validation is unavailable so
// callers can reject validated connections before starting a handshake.
#if !defined(OPENSSL_VERSION_NUMBER) || OPENSSL_VERSION_NUMBER < 0x10002000L
static int v_net_openssl_has_x509_identity_checks(void) {
	return 0;
}
static int v_net_openssl_x509_check_host(X509 *cert, const char *name, size_t name_len, unsigned int flags, char **peer_name) {
	(void)cert;
	(void)name;
	(void)name_len;
	(void)flags;
	(void)peer_name;
	return 0;
}
static int v_net_openssl_x509_check_ip_asc(X509 *cert, const char *ip_asc, unsigned int flags) {
	(void)cert;
	(void)ip_asc;
	(void)flags;
	return 0;
}
#else
static int v_net_openssl_has_x509_identity_checks(void) {
	return 1;
}
static int v_net_openssl_x509_check_host(X509 *cert, const char *name, size_t name_len, unsigned int flags, char **peer_name) {
	return X509_check_host(cert, name, name_len, flags, peer_name);
}
static int v_net_openssl_x509_check_ip_asc(X509 *cert, const char *ip_asc, unsigned int flags) {
	return X509_check_ip_asc(cert, ip_asc, flags);
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

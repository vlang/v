#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/pem.h>
#include <openssl/x509_vfy.h>

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

static int v_net_openssl_SSL_CTX_use_certificate_chain_memory(SSL_CTX *ctx,
		const unsigned char *data, size_t len) {
	if (len > INT_MAX) {
		return 0;
	}
	BIO *bio = BIO_new_mem_buf((const void *)data, (int)len);
	if (bio == NULL) {
		return 0;
	}
	X509 *cert = PEM_read_bio_X509_AUX(bio, NULL, NULL, NULL);
	if (cert == NULL) {
		BIO_free(bio);
		return 0;
	}
	int result = SSL_CTX_use_certificate(ctx, cert);
	X509_free(cert);
	if (result != 1) {
		BIO_free(bio);
		return 0;
	}
	while ((cert = PEM_read_bio_X509(bio, NULL, NULL, NULL)) != NULL) {
		// SSL_CTX_add_extra_chain_cert takes ownership of cert on success.
		if (SSL_CTX_add_extra_chain_cert(ctx, cert) != 1) {
			X509_free(cert);
			BIO_free(bio);
			return 0;
		}
	}
	// Reaching the end of a PEM stream leaves PEM_R_NO_START_LINE queued.
	ERR_clear_error();
	BIO_free(bio);
	return 1;
}

static int v_net_openssl_SSL_CTX_extra_chain_certs_count(SSL_CTX *ctx) {
	STACK_OF(X509) *chain = NULL;
	SSL_CTX_get_extra_chain_certs(ctx, &chain);
	return chain == NULL ? 0 : sk_X509_num(chain);
}

static int v_net_openssl_SSL_CTX_use_PrivateKey_memory(SSL_CTX *ctx,
		const unsigned char *data, size_t len) {
	if (len > INT_MAX) {
		return 0;
	}
	BIO *bio = BIO_new_mem_buf((const void *)data, (int)len);
	if (bio == NULL) {
		return 0;
	}
	EVP_PKEY *key = PEM_read_bio_PrivateKey(bio, NULL, NULL, NULL);
	BIO_free(bio);
	if (key == NULL) {
		return 0;
	}
	int result = SSL_CTX_use_PrivateKey(ctx, key);
	EVP_PKEY_free(key);
	return result;
}

static int v_net_openssl_SSL_CTX_load_verify_memory(SSL_CTX *ctx,
		const unsigned char *data, size_t len) {
	if (len > INT_MAX) {
		return 0;
	}
	BIO *bio = BIO_new_mem_buf((const void *)data, (int)len);
	if (bio == NULL) {
		return 0;
	}
	X509_STORE *store = SSL_CTX_get_cert_store(ctx);
	X509 *cert = NULL;
	int loaded = 0;
	while ((cert = PEM_read_bio_X509_AUX(bio, NULL, NULL, NULL)) != NULL) {
		if (X509_STORE_add_cert(store, cert) != 1) {
			X509_free(cert);
			BIO_free(bio);
			return 0;
		}
		X509_free(cert);
		loaded++;
	}
	ERR_clear_error();
	BIO_free(bio);
	return loaded > 0 ? 1 : 0;
}

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
static int v_net_openssl_SSL_CTX_set_alpn_select_protos(SSL_CTX *ctx,
		const unsigned char *protos, unsigned int protos_len) {
	(void)ctx;
	(void)protos;
	(void)protos_len;
	return -1;
}
#else
typedef struct {
	unsigned char *protos;
	unsigned int protos_len;
} v_net_openssl_alpn_select_state;

static void v_net_openssl_free_alpn_select_state(void *parent, void *ptr,
		CRYPTO_EX_DATA *ad, int idx, long argl, void *argp) {
	(void)parent;
	(void)ad;
	(void)idx;
	(void)argl;
	(void)argp;
	v_net_openssl_alpn_select_state *selection =
		(v_net_openssl_alpn_select_state *)ptr;
	if (selection != NULL) {
		free(selection->protos);
		free(selection);
	}
}

static int v_net_openssl_alpn_select_cb(SSL *ssl, const unsigned char **out,
		unsigned char *outlen, const unsigned char *in, unsigned int inlen, void *arg) {
	(void)ssl;
	v_net_openssl_alpn_select_state *state = (v_net_openssl_alpn_select_state *)arg;
	if (SSL_select_next_proto((unsigned char **)out, outlen, state->protos,
			state->protos_len, in, inlen) == OPENSSL_NPN_NEGOTIATED) {
		return SSL_TLSEXT_ERR_OK;
	}
	return SSL_TLSEXT_ERR_NOACK;
}

static int v_net_openssl_SSL_CTX_set_alpn_select_protos(SSL_CTX *ctx,
		const unsigned char *protos, unsigned int protos_len) {
	v_net_openssl_alpn_select_state *selection =
		(v_net_openssl_alpn_select_state *)malloc(sizeof(*selection));
	if (selection == NULL) {
		return -1;
	}
	selection->protos = (unsigned char *)malloc(protos_len);
	if (selection->protos == NULL) {
		free(selection);
		return -1;
	}
	memcpy(selection->protos, protos, protos_len);
	selection->protos_len = protos_len;
	int state_index = SSL_CTX_get_ex_new_index(0, NULL, NULL, NULL,
		v_net_openssl_free_alpn_select_state);
	if (state_index < 0 || SSL_CTX_set_ex_data(ctx, state_index, selection) != 1) {
		free(selection->protos);
		free(selection);
		return -1;
	}
	SSL_CTX_set_alpn_select_cb(ctx, v_net_openssl_alpn_select_cb, selection);
	return 0;
}

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

// TLS_server_method() is only available in OpenSSL 1.1.0 and newer.
// On older OpenSSL/LibreSSL, fall back to SSLv23_server_method().
#if defined(LIBRESSL_VERSION_NUMBER) || !defined(OPENSSL_VERSION_NUMBER) \
	|| OPENSSL_VERSION_NUMBER < 0x10100000L
static const SSL_METHOD *v_net_openssl_TLS_server_method(void) {
	return SSLv23_server_method();
}
#else
static const SSL_METHOD *v_net_openssl_TLS_server_method(void) {
	return TLS_server_method();
}
#endif

#include <openssl/rand.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509v3.h>
#define V_NET_OPENSSL_LINKAGE
#include "openssl_compat.h"

int v_net_openssl_BIO_set_conn_hostname(BIO *bio, char *name) {
	return (int)BIO_set_conn_hostname(bio, name);
}
int v_net_openssl_BIO_get_ssl(BIO *bio, SSL **ssl) {
	return (int)BIO_get_ssl(bio, ssl);
}
int v_net_openssl_BIO_do_connect(BIO *bio) {
	return (int)BIO_do_connect(bio);
}
int v_net_openssl_BIO_do_handshake(BIO *bio) {
	return (int)BIO_do_handshake(bio);
}
int v_net_openssl_SSL_set_tlsext_host_name(SSL *ssl, char *name) {
	return (int)SSL_set_tlsext_host_name(ssl, name);
}
const SSL_METHOD *v_net_openssl_SSLv23_client_method(void) {
	return SSLv23_client_method();
}

_Static_assert(SSL_ERROR_NONE == 0, "V SSL_ERROR_NONE value");
_Static_assert(SSL_ERROR_SSL == 1, "V SSL_ERROR_SSL value");
_Static_assert(SSL_ERROR_WANT_READ == 2, "V SSL_ERROR_WANT_READ value");
_Static_assert(SSL_ERROR_WANT_WRITE == 3, "V SSL_ERROR_WANT_WRITE value");
_Static_assert(SSL_ERROR_WANT_X509_LOOKUP == 4, "V SSL_ERROR_WANT_X509_LOOKUP value");
_Static_assert(SSL_ERROR_SYSCALL == 5, "V SSL_ERROR_SYSCALL value");
_Static_assert(SSL_ERROR_ZERO_RETURN == 6, "V SSL_ERROR_ZERO_RETURN value");
_Static_assert(SSL_ERROR_WANT_CONNECT == 7, "V SSL_ERROR_WANT_CONNECT value");
_Static_assert(SSL_ERROR_WANT_ACCEPT == 8, "V SSL_ERROR_WANT_ACCEPT value");
_Static_assert(SSL_ERROR_WANT_ASYNC == 9, "V SSL_ERROR_WANT_ASYNC value");
_Static_assert(SSL_ERROR_WANT_ASYNC_JOB == 10, "V SSL_ERROR_WANT_ASYNC_JOB value");
_Static_assert(SSL_FILETYPE_PEM == 1, "V SSL_FILETYPE_PEM value");
_Static_assert(SSL_OP_NO_COMPRESSION == 131072, "V SSL_OP_NO_COMPRESSION value");
_Static_assert(SSL_OP_NO_SSLv2 == 0, "V SSL_OP_NO_SSLv2 value");
_Static_assert(SSL_OP_NO_SSLv3 == 33554432, "V SSL_OP_NO_SSLv3 value");
_Static_assert(SSL_VERIFY_PEER == 1, "V SSL_VERIFY_PEER value");
_Static_assert(SSL_VERIFY_FAIL_IF_NO_PEER_CERT == 2, "V SSL_VERIFY_FAIL_IF_NO_PEER_CERT value");
_Static_assert(X509_V_OK == 0, "V X509_V_OK value");

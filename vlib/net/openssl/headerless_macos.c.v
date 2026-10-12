// OpenSSL constants and macro entry points for the macOS C ABI.
module openssl

#define SSL_ERROR_NONE 0
#define SSL_ERROR_SSL 1
#define SSL_ERROR_WANT_READ 2
#define SSL_ERROR_WANT_WRITE 3
#define SSL_ERROR_WANT_X509_LOOKUP 4
#define SSL_ERROR_SYSCALL 5
#define SSL_ERROR_ZERO_RETURN 6
#define SSL_ERROR_WANT_CONNECT 7
#define SSL_ERROR_WANT_ACCEPT 8
#define SSL_ERROR_WANT_ASYNC 9
#define SSL_ERROR_WANT_ASYNC_JOB 10
#define SSL_FILETYPE_PEM 1
#define SSL_OP_NO_COMPRESSION 131072
#define SSL_OP_NO_SSLv2 0
#define SSL_OP_NO_SSLv3 33554432
#define SSL_VERIFY_PEER 1
#define SSL_VERIFY_FAIL_IF_NO_PEER_CERT 2
#define X509_V_OK 0

#define BIO_set_conn_hostname v_net_openssl_BIO_set_conn_hostname
#define BIO_get_ssl v_net_openssl_BIO_get_ssl
#define BIO_do_connect v_net_openssl_BIO_do_connect
#define BIO_do_handshake v_net_openssl_BIO_do_handshake
#define SSL_set_tlsext_host_name v_net_openssl_SSL_set_tlsext_host_name
#define SSLv23_client_method v_net_openssl_SSLv23_client_method

module openssl

// On Linux, prefer a localy built openssl, because it is
// much more likely for it to be newer, than the system
// openssl from libssl-dev. If there is no local openssl,
// the next flag is harmless, since it will still use the
// (older) system openssl.
#flag linux -I/usr/local/include/openssl -L/usr/local/lib
#flag windows -l libssl -l libcrypto
#flag -l ssl -l crypto
// MacPorts
#flag darwin -I/opt/local/include
#flag darwin -L/opt/local/lib
// Brew
#flag darwin -I/usr/local/opt/openssl/include
#flag darwin -L/usr/local/opt/openssl/lib
// Brew arm64
#flag darwin -I /opt/homebrew/opt/openssl/include
#flag darwin -L /opt/homebrew/opt/openssl/lib
//
#include <openssl/rand.h> # Please install OpenSSL development headers
#include <openssl/ssl.h>
#include <openssl/err.h>
pub struct C.SSL {
}

pub struct SSL_CTX {
}

pub struct SSL {
}

pub struct SSL_METHOD {
}

fn C.BIO_new_ssl_connect() voidptr

fn C.BIO_set_conn_hostname() int

fn C.BIO_get_ssl()

fn C.BIO_do_connect() int

fn C.BIO_do_handshake() int

fn C.BIO_puts()

fn C.BIO_read() int

fn C.BIO_free_all()

fn C.SSL_CTX_new() &C.SSL_CTX

fn C.SSL_CTX_set_options()

fn C.SSL_CTX_set_verify_depth()

fn C.SSL_CTX_load_verify_locations() int

fn C.SSL_CTX_free()

fn C.SSL_new() &C.SSL

fn C.SSL_set_fd() int

fn C.SSL_connect() int

fn C.SSL_set_cipher_list() int

fn C.SSL_get_peer_certificate() int

fn C.ERR_clear_error()

fn C.SSL_get_error() int

fn C.SSL_get_verify_result() int

fn C.SSL_set_tlsext_host_name() int

fn C.SSL_shutdown() int

fn C.SSL_free()

fn C.SSL_write() int

fn C.SSL_read() int

fn C.SSL_load_error_strings()

fn C.SSL_library_init()

fn C.SSLv23_client_method() &C.SSL_METHOD

fn C.TLS_method() voidptr

fn C.TLSv1_2_method() voidptr

fn init() {
	C.SSL_load_error_strings()
	C.SSL_library_init()
}

pub const (
	is_used = 1
)

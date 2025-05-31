module openssl

#define OPENSSL_API_COMPAT 0x30000000L

// On Linux, prefer a locally built openssl, because it is
// much more likely for it to be newer, than the system
// openssl from libssl-dev. If there is no local openssl,
// the next #pkgconfig flag is harmless, since it will still
// use the (older) system openssl.
#flag linux -I/usr/local/include/openssl
#flag linux -L/usr/local/lib64
// On FreeBSD, prefer openssl from the ports collection, because
// it is much more likely for it to be newer, than the system
// openssl.
#flag freebsd -I/usr/local/include
#flag freebsd -L/usr/local/lib

// Installed through choco:
#flag windows -IC:/Program Files/OpenSSL-Win64/include
#flag windows -LC:/Program Files/OpenSSL-Win64/lib/VC/x64/MD

// Installed on the CI:
#flag windows -IC:/Program Files/OpenSSL/include
#flag windows -LC:/Program Files/OpenSSL/lib/VC/x64/MD

$if $pkgconfig('openssl') {
	#pkgconfig --cflags --libs openssl
} $else {
	#flag windows -l libssl -l libcrypto
	$if !windows {
		#flag -lssl -lcrypto
	}
	#flag linux -ldl -lpthread
	// MacPorts
	#flag darwin -I/opt/local/include
	#flag darwin -L/opt/local/lib
	// Brew
	#flag darwin -I/usr/local/opt/openssl/include
	#flag darwin -L/usr/local/opt/openssl/lib
	// brew on macos-12 (ci runner)
	#flag darwin -I/usr/local/opt/openssl@3/include
	#flag darwin -L/usr/local/opt/openssl@3/lib
	// Brew arm64
	#flag darwin -I /opt/homebrew/opt/openssl/include
	#flag darwin -L /opt/homebrew/opt/openssl/lib
	// Procursus
	#flag darwin -I/opt/procursus/include
	#flag darwin -L/opt/procursus/lib
}

#include <openssl/rand.h> # Please install OpenSSL development headers
#include <openssl/ssl.h>
#include <openssl/err.h>

@[typedef]
pub struct C.SSL {
}

@[typedef]
pub struct C.BIO {
}

@[typedef]
pub struct C.SSL_METHOD {
}

@[typedef]
pub struct C.X509 {
}

@[typedef]
pub struct C.SSL_CTX {
}

// The above C structs, have incomplete declarations in the OpenSSL headers.
// For this reason, we have to prevent the automatic str() generation for them,
// by adding manual implementations of their .str() methods, that are defined on
// pointers to them:
fn (s &C.SSL) str() string {
	return 'C.SSL(0x${voidptr(s)})'
}

fn (c &C.SSL_CTX) str() string {
	return 'C.SSL_CTX(0x${voidptr(c)})'
}

fn C.BIO_new_ssl_connect(ctx &C.SSL_CTX) &C.BIO

fn C.BIO_set_conn_hostname(b &C.BIO, name &char) int

// there are actually 2 macros for BIO_get_ssl
// fn C.BIO_get_ssl(bp &C.BIO, ssl charptr, c int)
// fn C.BIO_get_ssl(bp &C.BIO, sslp charptr)
fn C.BIO_get_ssl(bp &C.BIO, vargs ...voidptr)

fn C.BIO_do_connect(b &C.BIO) int

fn C.BIO_do_handshake(b &C.BIO) int

fn C.BIO_puts(b &C.BIO, buf &char)

fn C.BIO_read(b &C.BIO, buf voidptr, len int) int

fn C.BIO_free_all(a &C.BIO)

fn C.SSL_CTX_new(method &C.SSL_METHOD) &C.SSL_CTX

fn C.SSL_CTX_set_options(ctx &C.SSL_CTX, options int)

fn C.SSL_CTX_set_verify_depth(s &C.SSL_CTX, depth int)

fn C.SSL_CTX_load_verify_locations(ctx &C.SSL_CTX, const_file &char, ca_path &char) int

fn C.SSL_CTX_free(ctx &C.SSL_CTX)

fn C.SSL_CTX_use_certificate_file(ctx &C.SSL_CTX, const_file &char, file_type int) int

fn C.SSL_CTX_use_PrivateKey_file(ctx &C.SSL_CTX, const_file &char, file_type int) int

fn C.SSL_new(&C.SSL_CTX) &C.SSL

fn C.SSL_set_fd(ssl &C.SSL, fd int) int

fn C.SSL_connect(&C.SSL) int

fn C.SSL_do_handshake(&C.SSL) int

fn C.SSL_set_cipher_list(ctx &SSL, str &char) int

fn C.SSL_get1_peer_certificate(ssl &SSL) &C.X509

fn C.X509_free(const_cert &C.X509)

fn C.ERR_clear_error()

fn C.SSL_get_error(ssl &C.SSL, ret int) int

fn C.SSL_get_verify_result(ssl &SSL) int

fn C.SSL_set_tlsext_host_name(s &SSL, name &char) int

fn C.SSL_shutdown(&C.SSL) int

fn C.SSL_free(&C.SSL)

fn C.SSL_write(ssl &C.SSL, buf voidptr, buflen int) int

fn C.SSL_read(ssl &C.SSL, buf voidptr, buflen int) int

fn C.SSL_load_error_strings()

fn C.SSL_library_init() int

fn C.SSLv23_client_method() &C.SSL_METHOD

fn C.TLS_method() voidptr

fn C.TLSv1_2_method() voidptr

fn C.OPENSSL_init_ssl(opts u64, settings &OPENSSL_INIT_SETTINGS) int

fn init() {
	$if ssl_pre_1_1_version ? {
		// OPENSSL_VERSION_NUMBER < 0x10100000L
		C.SSL_load_error_strings()
		C.SSL_library_init()
	} $else {
		C.OPENSSL_init_ssl(C.OPENSSL_INIT_LOAD_SSL_STRINGS, 0)
	}
}

// ssl_error returns non error ssl code or error if unrecoverable and we should panic
fn ssl_error(ret int, ssl voidptr) !SSLError {
	res := C.SSL_get_error(ssl, ret)
	$if trace_ssl ? {
		eprintln('${@METHOD} ret: ${ret} | ssl: ${ssl:x} | res: ${res}')
	}
	match unsafe { SSLError(res) } {
		.ssl_error_syscall {
			return error_with_code('unrecoverable syscall (${res})', res)
		}
		.ssl_error_ssl {
			return error_with_code('unrecoverable ssl protocol error (${res})', res)
		}
		else {
			return unsafe { SSLError(res) }
		}
	}
}

enum SSLError {
	ssl_error_none             = C.SSL_ERROR_NONE
	ssl_error_ssl              = C.SSL_ERROR_SSL
	ssl_error_want_read        = C.SSL_ERROR_WANT_READ
	ssl_error_want_write       = C.SSL_ERROR_WANT_WRITE
	ssl_error_want_x509_lookup = C.SSL_ERROR_WANT_X509_LOOKUP
	ssl_error_syscall          = C.SSL_ERROR_SYSCALL
	ssl_error_zero_return      = C.SSL_ERROR_ZERO_RETURN
	ssl_error_want_connect     = C.SSL_ERROR_WANT_CONNECT
	ssl_error_want_accept      = C.SSL_ERROR_WANT_ACCEPT
	ssl_error_want_async       = C.SSL_ERROR_WANT_ASYNC
	ssl_error_want_async_job   = C.SSL_ERROR_WANT_ASYNC_JOB
}

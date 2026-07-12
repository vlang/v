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
	// Prefer a single matching macOS OpenSSL prefix to avoid mixing Intel and arm64 installs.
	$if arm64 {
		#flag darwin -I$when_first_existing('/opt/local/include','/opt/homebrew/opt/openssl/include','/opt/homebrew/opt/openssl@3/include','/opt/procursus/include','/usr/local/opt/openssl/include','/usr/local/opt/openssl@3/include')
		#flag darwin -L$when_first_existing('/opt/local/lib','/opt/homebrew/opt/openssl/lib','/opt/homebrew/opt/openssl@3/lib','/opt/procursus/lib','/usr/local/opt/openssl/lib','/usr/local/opt/openssl@3/lib')
	} $else {
		#flag darwin -I$when_first_existing('/opt/local/include','/usr/local/opt/openssl/include','/usr/local/opt/openssl@3/include','/opt/procursus/include','/opt/homebrew/opt/openssl/include','/opt/homebrew/opt/openssl@3/include')
		#flag darwin -L$when_first_existing('/opt/local/lib','/usr/local/opt/openssl/lib','/usr/local/opt/openssl@3/lib','/opt/procursus/lib','/opt/homebrew/opt/openssl/lib','/opt/homebrew/opt/openssl@3/lib')
	}
}

#include <openssl/rand.h> # Please install OpenSSL development headers
#include <openssl/ssl.h>
#include <openssl/err.h>
#insert "@VEXEROOT/vlib/net/openssl/openssl_compat.h"

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

fn C.BIO_set_conn_hostname(b &C.BIO, name &char) i32

// there are actually 2 macros for BIO_get_ssl
// fn C.BIO_get_ssl(bp &C.BIO, ssl charptr, c int)
// fn C.BIO_get_ssl(bp &C.BIO, sslp charptr)
fn C.BIO_get_ssl(bp &C.BIO, vargs ...voidptr)

fn C.BIO_do_connect(b &C.BIO) i32

fn C.BIO_do_handshake(b &C.BIO) i32

fn C.BIO_puts(b &C.BIO, buf &char)

fn C.BIO_read(b &C.BIO, buf voidptr, len i32) i32

fn C.BIO_free_all(a &C.BIO)

fn C.SSL_CTX_new(method &C.SSL_METHOD) &C.SSL_CTX

fn C.SSL_CTX_set_options(ctx &C.SSL_CTX, options i32)

fn C.SSL_CTX_set_verify_depth(s &C.SSL_CTX, depth i32)

fn C.SSL_CTX_load_verify_locations(ctx &C.SSL_CTX, const_file &char, const_ca_path &char) i32

fn C.SSL_CTX_free(ctx &C.SSL_CTX)

fn C.SSL_CTX_use_certificate_file(ctx &C.SSL_CTX, const_file &char, file_type i32) i32

fn C.SSL_CTX_use_PrivateKey_file(ctx &C.SSL_CTX, const_file &char, file_type i32) i32

fn C.SSL_new(&C.SSL_CTX) &C.SSL

fn C.SSL_set_fd(ssl &C.SSL, fd i32) i32

fn C.SSL_connect(&C.SSL) i32

fn C.SSL_do_handshake(&C.SSL) i32

fn C.SSL_set_cipher_list(ctx &C.SSL, str &char) i32

fn C.v_net_openssl_get1_peer_certificate(ssl &C.SSL) &C.X509

fn C.X509_free(const_cert &C.X509)

fn C.ERR_clear_error()

fn C.ERR_get_error() u64

fn C.ERR_error_string_n(e u64, buf &char, len usize)

fn C.SSL_get_error(ssl &C.SSL, ret i32) i32

fn C.SSL_get_verify_result(ssl &C.SSL) i32

fn C.SSL_set_tlsext_host_name(s &C.SSL, name &char) i32

// The ALPN calls go through small version-guarded shims in openssl_compat.h,
// so the module still links against OpenSSL versions older than 1.0.2 that
// predate ALPN. v_net_openssl_set_alpn_protos returns 0 on success (like the
// underlying SSL_set_alpn_protos) and non-zero when ALPN is unavailable.
// `data` is `const unsigned char **`; declared as voidptr so V emits a clean
// `(void*)` cast and avoids -cstrict nested-pointer const warnings.
fn C.v_net_openssl_set_alpn_protos(ssl &C.SSL, protos &u8, protos_len u32) i32

fn C.v_net_openssl_get0_alpn_selected(ssl &C.SSL, data voidptr, len &u32)

fn C.SSL_shutdown(&C.SSL) i32

fn C.SSL_free(&C.SSL)

fn C.SSL_write(ssl &C.SSL, buf voidptr, buflen i32) i32

fn C.SSL_read(ssl &C.SSL, buf voidptr, buflen i32) i32

fn C.SSLv23_client_method() &C.SSL_METHOD

fn C.TLS_method() voidptr

fn C.TLSv1_2_method() voidptr

fn C.v_net_openssl_init_ssl() i32

fn init() {
	C.v_net_openssl_init_ssl()
}

// ssl_get_error_queue drains the current thread's OpenSSL error queue and
// returns a human-readable, semicolon-separated description of every queued
// entry (oldest first), or an empty string if the queue is empty. It always
// leaves the queue empty afterwards: this both turns the otherwise opaque
// `SSL_ERROR_SSL`/`SSL_ERROR_SYSCALL` codes into actionable messages, and
// prevents a leftover entry from making a later SSL_get_error() misreport the
// status of the next, unrelated I/O operation.
fn ssl_get_error_queue() string {
	mut reasons := []string{}
	for {
		code := C.ERR_get_error()
		if code == 0 {
			break
		}
		mut buf := [256]char{}
		C.ERR_error_string_n(code, &buf[0], usize(buf.len))
		reasons << unsafe { cstring_to_vstring(&buf[0]) }
	}
	return reasons.join('; ')
}

// ssl_error returns non error ssl code or error if unrecoverable and we should panic
fn ssl_error(ret int, ssl voidptr) !SSLError {
	res := C.SSL_get_error(ssl, ret)
	$if trace_ssl ? {
		eprintln('${@METHOD} ret: ${ret} | ssl: ${ssl:x} | res: ${res}')
	}
	match unsafe { SSLError(res) } {
		.ssl_error_syscall {
			details := ssl_get_error_queue()
			suffix := if details == '' { '' } else { ': ${details}' }
			return error_with_code('net.openssl unrecoverable syscall (${res})${suffix}', res)
		}
		.ssl_error_ssl {
			details := ssl_get_error_queue()
			suffix := if details == '' { '' } else { ': ${details}' }
			return error_with_code('net.openssl unrecoverable ssl protocol error (${res})${suffix}',
				res)
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

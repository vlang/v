module openssl

// ssl_error returns non error ssl code or error if unrecoverable and we should panic
fn ssl_error(ret int, ssl voidptr) !SSLError {
	res := C.SSL_get_error(ssl, ret)
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
	ssl_error_none = 0 // SSL_ERROR_NONE
	ssl_error_ssl = 1 // SSL_ERROR_SSL
	ssl_error_want_read = 2 // SSL_ERROR_WANT_READ
	ssl_error_want_write = 3 // SSL_ERROR_WANT_WRITE
	ssl_error_want_x509_lookup = 4 // SSL_ERROR_WANT_X509_LOOKUP
	ssl_error_syscall = 5 // SSL_ERROR_SYSCALL
	ssl_error_zero_return = 6 // SSL_ERROR_ZERO_RETURN
	ssl_error_want_connect = 7 // SSL_ERROR_WANT_CONNECT
	ssl_error_want_accept = 8 // SSL_ERROR_WANT_ACCEPT
	ssl_error_want_async = 9 // SSL_ERROR_WANT_ASYNC
	ssl_error_want_async_job = 10 // SSL_ERROR_WANT_ASYNC_JOB
	ssl_error_want_early = 11 // SSL_ERROR_WANT_EARLY
}

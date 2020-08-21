module openssl

// ssl_error returns non error ssl code or error if unrecoverable and we should panic
pub fn ssl_error(ret int, ssl voidptr) ?SSLError {
	res := C.SSL_get_error(ssl, ret)
	match SSLError(res) {
		.ssl_error_syscall { return error_with_code('unrecoverable syscall ($res)', res) }
		.ssl_error_ssl { return error_with_code('unrecoverable ssl protocol error ($res)',
				res) }
		else { return res }
	}
}

pub enum SSLError {
	ssl_error_none = C.SSL_ERROR_NONE
	ssl_error_ssl = C.SSL_ERROR_SSL
	ssl_error_want_read = C.SSL_ERROR_WANT_READ
	ssl_error_want_write = C.SSL_ERROR_WANT_WRITE
	ssl_error_want_x509_lookup = C.SSL_ERROR_WANT_X509_LOOKUP
	ssl_error_syscall = C.SSL_ERROR_SYSCALL
	ssl_error_zero_return = C.SSL_ERROR_ZERO_RETURN
	ssl_error_want_connect = C.SSL_ERROR_WANT_CONNECT
	ssl_error_want_accept = C.SSL_ERROR_WANT_ACCEPT
	ssl_error_want_async = C.SSL_ERROR_WANT_ASYNC
	ssl_error_want_async_job = C.SSL_ERROR_WANT_ASYNC_JOB
	ssl_error_want_client_hello_cb = C.SSL_ERROR_WANT_CLIENT_HELLO_CB
}

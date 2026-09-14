import net

// A failed dial has to preserve the error code of the underlying connect, so
// that callers (net.http's retry loop, the TLS backends, ...) can classify the
// failure, instead of having to parse the error message for it. dial_tcp used
// to aggregate the per address errors into a plain error(), dropping their
// codes, which made net.openssl.SSLConn.dial (that returns dial_tcp's error
// verbatim) report err.code() == 0 for a refused connection.
// See https://github.com/vlang/v/issues/28510 .

// free_port returns a port on 127.0.0.1, that nothing is listening on, so that
// dialing it fails fast and deterministically (ECONNREFUSED: 111 on Linux, 61
// on macOS, WSAECONNREFUSED 10061 on Windows - hence the tests below only
// assert, that the code is not 0).
fn free_port() !int {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := l.addr()!.port()!
	l.close()!
	return port
}

fn test_dial_tcp_failure_preserves_the_error_code() {
	port := free_port()!
	mut conn := net.dial_tcp('127.0.0.1:${port}') or {
		assert err.code() != 0, 'dial_tcp should preserve the connect error code, got: ${err}'
		assert err.msg().contains('dial_tcp failed for address'), 'unexpected message: ${err}'
		return
	}
	conn.close() or {}
	assert false, 'dialing a port, that nothing listens on, should have failed'
}

fn test_dial_tcp_with_bind_failure_preserves_the_error_code() {
	port := free_port()!
	mut conn := net.dial_tcp_with_bind('127.0.0.1:${port}', '0.0.0.0:0') or {
		assert err.code() != 0, 'dial_tcp_with_bind should preserve the connect error code, got: ${err}'
		assert err.msg().contains('dial_tcp_with_bind failed for address'), 'unexpected message: ${err}'
		return
	}
	conn.close() or {}
	assert false, 'dialing a port, that nothing listens on, should have failed'
}

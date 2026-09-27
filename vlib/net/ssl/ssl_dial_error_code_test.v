// vtest build: !sanitize-memory-clang
module ssl

import net

// SSLConn.dial has to report a non 0 error code, when the TCP connect under
// the TLS handshake fails, with both backends (mbedTLS by default, OpenSSL
// under -d use_openssl). The OpenSSL backend returns net.dial_tcp's error as
// is, so it used to lose the code, while the mbedTLS one returned
// MBEDTLS_ERR_NET_CONNECT_FAILED (-68).
// See https://github.com/vlang/v/issues/28510 .
fn test_dial_failure_has_a_non_zero_error_code() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	// nothing is listening on `port` any more:
	port_listener.close()!

	mut conn := new_ssl_conn(validate: false)!
	conn.dial('127.0.0.1', port) or {
		assert err.code() != 0, 'SSLConn.dial should report a coded failure, got: ${err}'
		return
	}
	conn.shutdown() or {}
	assert false, 'dialing a port, that nothing listens on, should have failed'
}

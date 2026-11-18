module net

// A failed bind has to preserve the error code of the underlying socket
// error, so that callers can tell "address in use" from "permission denied"
// without parsing the error message. socket_error_message used to wrap the
// message of socket_error in a plain error(), dropping its code, which made
// listen_tcp report err.code() == 0 for a failed bind.
// See https://github.com/vlang/v/issues/32335 .
fn test_socket_error_message_preserves_the_socket_error_code() {
	// Fail a socket call with invalid arguments, so that the thread-local
	// last socket error is a known nonzero value (EAFNOSUPPORT on POSIX,
	// WSAEAFNOSUPPORT on Windows).
	_ = C.socket(-1, -1, -1)
	expected := error_code()
	assert expected != 0, 'a socket call with invalid arguments must fail'
	socket_error_message(-1, 'binding to 127.0.0.1:8903 failed') or {
		assert err.code() == expected, 'socket_error_message must preserve the socket error code, got code ${err.code()} instead of ${expected}: ${err}'
		assert err.msg().starts_with('net: socket error:')
		assert err.msg().ends_with('; binding to 127.0.0.1:8903 failed')
		return
	}
	assert false, 'socket_error_message(-1) should have failed'
}

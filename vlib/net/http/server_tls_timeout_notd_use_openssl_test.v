module http

import time

fn test_tls_accept_timeouts_preserve_zero_handshake_timeout() {
	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(0)
	assert accept_poll_timeout == tls_accept_poll_timeout
	assert handshake_timeout == 0
}

fn test_tls_accept_timeouts_cap_poll_without_changing_handshake_timeout() {
	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(time.second)
	assert accept_poll_timeout == tls_accept_poll_timeout
	assert handshake_timeout == time.second
}

fn test_tls_accept_timeouts_keep_short_accept_timeout() {
	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(50 * time.millisecond)
	assert accept_poll_timeout == 50 * time.millisecond
	assert handshake_timeout == 50 * time.millisecond
}

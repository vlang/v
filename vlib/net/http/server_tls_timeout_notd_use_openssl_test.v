module http

import time

fn test_tls_accept_timeouts_finite_handshake_for_infinite_accept() {
	// An infinite accept timeout (<= 0) must still yield a finite handshake
	// timeout, so a client stalling mid-handshake cannot wedge the accept
	// thread or block shutdown.
	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(0)
	assert accept_poll_timeout == tls_accept_poll_timeout
	assert handshake_timeout == tls_handshake_timeout
	_, neg_handshake_timeout := tls_accept_timeouts(-1)
	assert neg_handshake_timeout == tls_handshake_timeout
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

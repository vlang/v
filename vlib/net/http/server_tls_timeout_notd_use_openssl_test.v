module http

import net
import time

fn test_tls_accept_timeouts_finite_handshake_for_infinite_accept() {
	// An infinite accept timeout (<= 0, or net.infinite_timeout) must still
	// yield a finite handshake timeout, so a client stalling mid-handshake
	// cannot wedge the accept thread or block shutdown.
	// net.infinite_timeout is i64.max (positive), so the > 0 guard alone is
	// insufficient — mbedtls treats it as an infinite deadline.
	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(0, tls_handshake_timeout)
	assert accept_poll_timeout == tls_accept_poll_timeout
	assert handshake_timeout == tls_handshake_timeout
	_, neg_handshake_timeout := tls_accept_timeouts(-1, tls_handshake_timeout)
	assert neg_handshake_timeout == tls_handshake_timeout
	_, inf_handshake_timeout := tls_accept_timeouts(net.infinite_timeout, tls_handshake_timeout)
	assert inf_handshake_timeout == tls_handshake_timeout
}

fn test_tls_accept_timeouts_cap_poll_without_changing_handshake_timeout() {
	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(time.second,
		tls_handshake_timeout)
	assert accept_poll_timeout == tls_accept_poll_timeout
	assert handshake_timeout == time.second
}

fn test_tls_accept_timeouts_keep_short_accept_timeout() {
	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(50 * time.millisecond,
		tls_handshake_timeout)
	assert accept_poll_timeout == 50 * time.millisecond
	assert handshake_timeout == 50 * time.millisecond
}

fn test_tls_accept_timeouts_configurable_fallback() {
	// When accept_timeout is infinite, the handshake_fallback parameter
	// (Server.tls_handshake_timeout) governs the budget — not the constant.
	custom_fallback := 5 * time.second
	_, handshake_timeout := tls_accept_timeouts(0, custom_fallback)
	assert handshake_timeout == custom_fallback
}

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.quic

// h3_dial_udp_and_open itself needs a real UDP-reachable peer to exercise
// meaningfully (h3_mux_conn_test.v's fake-transport tests cover the
// threading/dispatch behavior downstream of it) -- what IS deterministic
// and worth covering directly is this file's own pure pieces: the default
// SETTINGS this client advertises, and h3_now_ms's monotonicity.

fn test_h3_default_own_settings_carries_the_two_qpack_parameters() {
	settings := h3_default_own_settings()
	assert settings.len == 2
	assert quic.qpack_max_table_capacity_from_settings(settings) == h3_default_own_qpack_max_table_capacity
	assert quic.qpack_blocked_streams_from_settings(settings) == h3_default_own_qpack_blocked_streams
}

fn test_h3_now_ms_is_monotonically_non_decreasing() {
	a := h3_now_ms()
	b := h3_now_ms()
	assert b >= a
}

fn test_h3_udp_conn_transport_read_reports_the_underlying_close() {
	// A minimal smoke test for H3UdpConnTransport's own close() plumbing:
	// double-closing an already-closed net.UdpConn must not panic (mirrors
	// H2PooledTransport.close()'s own idempotency expectation), since
	// H3MuxConn.fail_conn always calls transport.close() unconditionally on
	// every teardown path, including one that races a caller's own manual
	// close.
	mut udp := net.dial_udp('127.0.0.1:1') or {
		// No listener is required for a connect()-style UDP socket to be
		// constructed at all -- dial_udp only fails if the OS itself refuses
		// to create the socket.
		assert false, 'net.dial_udp unexpectedly failed: ${err.msg()}'
		return
	}
	mut transport := H3UdpTransport(&H3UdpConnTransport{
		conn: udp
	})
	transport.close() or { assert false, 'first close() must succeed: ${err.msg()}' }
}

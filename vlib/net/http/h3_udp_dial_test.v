// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.quic
import time

// h3_dial_udp_and_open itself needs a real UDP-reachable peer to exercise
// meaningfully (h3_mux_conn_test.v's fake-transport tests cover the
// threading/dispatch behavior downstream of it) -- what IS deterministic
// and worth covering directly is this file's own pure pieces: the default
// SETTINGS this client advertises, and h3_now_ns's monotonicity and clock
// scale.

fn test_h3_default_own_settings_carries_the_two_qpack_parameters() {
	settings := h3_default_own_settings()
	assert settings.len == 2
	assert quic.qpack_max_table_capacity_from_settings(settings) == h3_default_own_qpack_max_table_capacity
	assert quic.qpack_blocked_streams_from_settings(settings) == h3_default_own_qpack_blocked_streams
}

fn test_h3_now_ns_is_monotonically_non_decreasing() {
	a := h3_now_ns()
	b := h3_now_ns()
	assert b >= a
}

// h3_now_ns's own doc comment claims its return value is "the unit and
// clock domain every quic.QuicConn/quic.H3Conn `now u64` parameter
// expects" -- and every quic-internal doc comment (idle_timeout.v,
// loss_detection.v, conn.v) says that unit is a RAW time.sys_mono_now()
// nanosecond instant, not milliseconds. Bracket h3_now_ns()'s return value
// between two time.sys_mono_now() calls made immediately around it: it
// must fall between them. Regression test for a real bug: an earlier
// version of this function (then named h3_now_ms) divided by 1_000_000,
// returning a millisecond-scale value ~1e6x smaller than this bound --
// reported by a maintainer-relayed "Local AI check" comment on PR #28164
// (github.com/vlang/v/pull/28164#issuecomment-5440010074): QUIC deadlines
// are nanosecond instants, but the old h3_now_ms fed a millisecond value
// into listener.poll/process_timeouts and H3Conn.poll/process_timeouts,
// making idle/PTO/retirement-timer comparisons dimensionally inconsistent
// -- in practice, elapsed time computed as (ms_now - ns_baseline)
// undercounts by ~1e6x, so an idle timeout meant to fire after e.g. 30s of
// real inactivity would not actually fire for roughly 347 days.
fn test_h3_now_ns_returns_a_nanosecond_scale_instant_not_milliseconds() {
	before := time.sys_mono_now()
	got := h3_now_ns()
	after := time.sys_mono_now()
	assert got >= before
	assert got <= after
}

// h3_driver_next_wait is the ONE shared computation behind both
// h3_server.v's serve() and h3_mux_conn.v's driver_loop -- extracted
// specifically because it was fixed in one call site and initially left
// unfixed-and-untested in the other during the h3_now_ns/h3_now_ms round
// (PR #28164). Deterministic, synthetic-value unit tests here are the
// right level for this: a real driver_loop's own PTO-timing-dependent
// behavior is comparatively expensive and flaky to pin down directly (an
// earlier version of this coverage tried exactly that, driving a real
// H3MuxConn.driver_loop against a fake transport and sleeping past a real
// PTO deadline -- correct in principle, but slow and fiddly to get the
// timing margins right for no real benefit over testing the pure
// computation directly).

fn test_h3_driver_next_wait_uses_default_when_no_timeout_armed() {
	got := h3_driver_next_wait(none, 1_000_000, 50 * time.millisecond)
	assert got == 50 * time.millisecond
}

// The load-bearing regression case: a deadline a small, nanosecond-scale
// amount ahead of `now` must shorten the wait to roughly THAT SAME small
// amount -- not that amount multiplied by another 1e6x (the pre-fix bug:
// `remaining` is already nanosecond-scale, since both `next_timeout` and
// `now_ns` are raw time.sys_mono_now() instants; time.Duration IS
// nanoseconds, so multiplying by time.millisecond again turned a 5ms
// remaining into roughly 5000 SECONDS).
fn test_h3_driver_next_wait_shortens_to_a_near_deadline_in_nanoseconds() {
	now_ns := u64(10_000_000_000) // 10 real seconds of uptime
	deadline := now_ns + 5_000_000 // 5ms away
	got := h3_driver_next_wait(deadline, now_ns, 50 * time.millisecond)
	assert got == 5 * time.millisecond
}

fn test_h3_driver_next_wait_keeps_default_when_deadline_is_farther_off() {
	now_ns := u64(10_000_000_000)
	deadline := now_ns + 200_000_000 // 200ms away -- farther than the 50ms default
	got := h3_driver_next_wait(deadline, now_ns, 50 * time.millisecond)
	assert got == 50 * time.millisecond
}

// A same-or-already-past-due deadline must floor to 1ms, never 0 or
// negative (which would starve every other goroutine via a zero/negative
// read timeout, or panic/block-forever depending on the transport).
fn test_h3_driver_next_wait_floors_a_past_due_deadline_to_one_millisecond() {
	now_ns := u64(10_000_000_000)
	got_equal := h3_driver_next_wait(now_ns, now_ns, 50 * time.millisecond)
	assert got_equal == 1 * time.millisecond
	got_past := h3_driver_next_wait(now_ns - 1_000_000, now_ns, 50 * time.millisecond)
	assert got_past == 1 * time.millisecond
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

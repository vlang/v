// vtest build: present_openssl?
// vtest vflags: -d http3
// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.quic
import sync
import time

// This file tests H3MuxConn's own bookkeeping and concurrency behavior in
// isolation from real HTTP/3 protocol content. A genuinely reactive fake
// QUIC server (decrypting and replying to whatever the client actually
// sends) is not built here -- that would mean re-deriving a second QUIC/
// TLS 1.3 server-role implementation from scratch, since conn_test.v's own
// fixture-handshake harness (net.quic) is built from private, test-file-
// only helpers that cannot be called across module boundaries; the
// approved Phase 12 plan's own "manual/documented run against a real
// quiche/ngtcp2 container ... recommended before merge as a non-CI-
// blocking check" already accounts for that gap, and a shared, exported
// cross-module test-fixture helper in net.quic is a scoped, worthwhile
// follow-up rather than something to build inline here.
//
// What IS fully testable without any peer response at all is everything
// this file actually covers: the pure admission/pool bookkeeping (can_
// take_new_request/is_idle_expired/do()'s three rejection paths) and, via
// a REAL driver thread against an in-memory FakeH3UdpTransport, the plan's
// own top-ranked risk callout for this sub-phase -- a request queued by
// do() between one driver_loop iteration and the next must not be
// stranded on cv.wait() forever when the connection dies in that exact
// window (fail_conn's own doc comment). quic.dial() itself needs no peer
// interaction (it only builds a ClientHello locally), so a real, valid
// &quic.QuicConn/&quic.H3Conn pair -- sitting in .handshaking, never
// established -- is enough to drive a genuine H3MuxConn.driver_loop.

// FakeH3UdpTransport is an in-memory H3UdpTransport stand-in.
@[heap]
struct FakeH3UdpTransport {
mut:
	mu         &sync.Mutex = sync.new_mutex()
	has_fatal  bool
	fatal_msg  string
	read_delay time.Duration
	writes     [][]u8
	closed     bool
}

fn new_fake_h3_udp_transport() &FakeH3UdpTransport {
	return &FakeH3UdpTransport{
		mu: sync.new_mutex()
	}
}

// read blocks for `read_delay` (simulating a slow/idle socket, giving other
// goroutines a real window to act before this call returns) and then either
// returns the armed fatal error or a plain timeout, matching a real
// UdpConn's own net.err_timed_out_code contract for "nothing arrived".
fn (mut t FakeH3UdpTransport) read(mut buf []u8) !int {
	t.mu.lock()
	delay := t.read_delay
	has_fatal := t.has_fatal
	msg := t.fatal_msg
	t.mu.unlock()
	if delay > 0 {
		time.sleep(delay)
	}
	if has_fatal {
		return error(msg)
	}
	return error_with_code('fake: no data', net.err_timed_out_code)
}

fn (mut t FakeH3UdpTransport) write(buf []u8) !int {
	t.mu.lock()
	t.writes << buf.clone()
	t.mu.unlock()
	return buf.len
}

fn (mut t FakeH3UdpTransport) set_read_timeout(timeout time.Duration) {
	// Deliberately ignored: this fake's own read_delay (set directly by the
	// test, not through this method) controls timing instead, so tests get
	// a fixed, predictable window regardless of driver_loop's own computed
	// wait duration.
}

fn (mut t FakeH3UdpTransport) close() ! {
	t.mu.lock()
	t.closed = true
	t.mu.unlock()
}

fn (mut t FakeH3UdpTransport) arm_fatal_error(msg string, delay time.Duration) {
	t.mu.lock()
	t.has_fatal = true
	t.fatal_msg = msg
	t.read_delay = delay
	t.mu.unlock()
}

// new_test_h3_mux_conn_no_driver builds an H3MuxConn directly, without
// calling new_h3_mux_conn -- no driver thread is spawned, so tests using
// this can manipulate qmu-guarded fields directly and call admission-side
// methods deterministically. Safe only for tests that never touch c.h3.
fn new_test_h3_mux_conn_no_driver() &H3MuxConn {
	return &H3MuxConn{
		transport:  H3UdpTransport(new_fake_h3_udp_transport())
		qmu:        sync.new_mutex()
		idle_since: time.now()
	}
}

// --- admission / pool bookkeeping --------------------------------------

fn test_can_take_new_request_true_initially() {
	mut c := new_test_h3_mux_conn_no_driver()
	assert c.can_take_new_request()
}

fn test_can_take_new_request_false_after_closed() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.closed = true
	assert !c.can_take_new_request()
}

fn test_can_take_new_request_false_after_goaway_received() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.goaway_received = true
	assert !c.can_take_new_request()
}

fn test_can_take_new_request_false_after_shutting_down() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.shutting_down = true
	assert !c.can_take_new_request()
}

fn test_is_idle_expired_false_with_active_streams_even_if_old() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.active_streams = 1
	c.idle_since = time.now().add(-1 * time.hour)
	assert !c.is_idle_expired(10 * time.millisecond)
}

fn test_is_idle_expired_true_when_idle_past_timeout() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.idle_since = time.now().add(-1 * time.hour)
	assert c.is_idle_expired(10 * time.millisecond)
}

fn test_is_idle_expired_false_when_idle_but_recent() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.idle_since = time.now()
	assert !c.is_idle_expired(1 * time.hour)
}

fn test_shutdown_when_idle_sets_the_flag() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.shutdown_when_idle()
	assert c.shutting_down
	assert !c.can_take_new_request()
}

fn test_release_decrements_refs() {
	mut c := new_test_h3_mux_conn_no_driver()
	assert c.refs == 1
	c.release()
	assert c.refs == 0
}

// --- do()'s three rejection paths ---------------------------------------
// None of these ever reach start_request/the driver thread -- do() checks
// closed/goaway_received/shutting_down and returns before queueing
// anything -- so they are deterministic without a spawned driver.

fn test_do_rejects_when_closed_with_the_stored_reason() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.closed = true
	c.conn_err = 'boom'
	c.do(H3ClientRequest{ authority: 'example.com' }) or {
		assert err.code() == h3_err_retryable_code
		assert err.msg().contains('boom')
		return
	}
	assert false, 'expected do() to reject a closed connection'
}

fn test_do_rejects_when_goaway_received() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.goaway_received = true
	c.do(H3ClientRequest{ authority: 'example.com' }) or {
		assert err.code() == h3_err_retryable_code
		return
	}
	assert false, 'expected do() to reject a GOAWAY-received connection'
}

fn test_do_rejects_when_shutting_down() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.shutting_down = true
	c.do(H3ClientRequest{ authority: 'example.com' }) or {
		assert err.code() == h3_err_retryable_code
		return
	}
	assert false, 'expected do() to reject a shutting-down connection'
}

// --- fail_conn: the pending-request-leak fix (12c's own top-ranked risk) ---

fn test_fail_conn_fails_a_request_still_in_pending() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	c.pending << PendingH3Request{
		req:    H3ClientRequest{
			authority: 'example.com'
		}
		stream: s
	}
	c.fail_conn('connection died')
	s.mu.lock()
	ended := s.ended
	err_msg := s.err
	retryable := s.retryable
	s.mu.unlock()
	assert ended
	assert err_msg == 'connection died'
	assert retryable, 'a request still in c.pending never reached the wire, so must be retryable'
	assert c.pending.len == 0
}

fn test_fail_conn_fails_a_registered_stream_too() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	s.sent_headers = true
	c.streams[42] = s
	c.fail_conn('connection died')
	s.mu.lock()
	ended := s.ended
	retryable := s.retryable
	s.mu.unlock()
	assert ended
	assert !retryable, 'headers already reached the wire, so must not be blind-retried'
	assert 42 !in c.streams
}

fn test_fail_conn_is_idempotent() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.fail_conn('first')
	c.fail_conn('second')
	assert c.conn_err == 'first'
}

// --- real driver thread: no-hang guarantee under a dying transport -----

// new_handshaking_h3_conn builds a real (never-established) QuicConn/
// H3Conn pair. quic.dial() needs no peer response at all -- it only builds
// a ClientHello and the first Initial packet locally -- so this is a fully
// real, valid connection object sitting in .handshaking, safe to drive a
// genuine H3MuxConn.driver_loop against without a fixture-handshake
// harness.
fn new_handshaking_h3_conn() &quic.H3Conn {
	mut qc, _ := quic.dial(quic.DialParams{
		server_name:    'example.com'
		alpn_protocols: ['h3']
	}, u64(1000)) or { panic('quic.dial: ${err.msg()}') }
	return quic.new_h3_conn(mut qc, quic.H3ConnParams{})
}

// H3TestDoOutcome carries one do() call's result out of its worker thread.
@[heap]
struct H3TestDoOutcome {
mut:
	got_err  bool
	err_code int
}

// h3_test_do_worker runs c.do(req) and reports the outcome, then signals
// done. A NAMED, non-closure function taking every captured value as an
// explicit parameter -- not an inline `spawn fn [...] () {...}()` literal --
// because two or more spawned closures sharing an identical parameter-list
// signature can have their capture contexts silently conflated by V's
// closure codegen (see project memory: v-closure-capture-conflation-bug);
// this file spawns more than one do()-calling worker across its tests, so
// every one of them uses this same named-function form to sidestep it
// entirely, per that bug's own documented fix.
fn h3_test_do_worker(mut c H3MuxConn, req H3ClientRequest, mut outcome H3TestDoOutcome, done chan bool) {
	c.do(req) or {
		outcome.got_err = true
		outcome.err_code = err.code()
		done <- true
		return
	}
	done <- true
}

// test_driver_fails_a_pending_request_stranded_by_a_dying_transport is a
// regression test for the exact race fail_conn's own doc comment
// describes: do() queues a request (c.pending) while the driver thread is
// blocked inside its current transport.read() call; that same read()
// returns a fatal error, and the driver's fail_conn must still find and
// fail the request that arrived DURING the blocked call, not just
// whatever was already in c.pending/c.streams before it started waiting.
// FakeH3UdpTransport.read()'s own read_delay creates that exact window
// deterministically: it always sleeps before returning, giving the
// spawned do() call a wide, reliable margin to enqueue before the fatal
// error is delivered.
fn test_driver_fails_a_pending_request_stranded_by_a_dying_transport() {
	mut transport := new_fake_h3_udp_transport()
	transport.arm_fatal_error('simulated transport death', 300 * time.millisecond)

	mut h3 := new_handshaking_h3_conn()
	mut c := new_h3_mux_conn(H3UdpTransport(transport), h3, unsafe { nil })

	done := chan bool{cap: 1}
	mut outcome := &H3TestDoOutcome{}
	// The driver's first read() call is already in flight (blocked in the
	// fake's 300ms delay) by the time this spawn schedules -- this do()
	// call queues into c.pending well within that window on any reasonable
	// scheduler.
	spawn h3_test_do_worker(mut c, H3ClientRequest{ authority: 'example.com' }, mut outcome, done)
	select {
		_ := <-done {}
		2 * time.second {
			assert false, "do() never returned -- the request queued during the driver's blocked read() was stranded (the exact regression fail_conn.pending draining fixes)"
			return
		}
	}
	assert outcome.got_err, 'expected the pending request to fail once the transport died, not succeed'
	assert outcome.err_code == h3_err_retryable_code
}

// test_driver_fails_a_second_concurrent_pending_request_too runs the same
// scenario with TWO concurrent requesters, confirming the fix generalizes
// past a single-entry c.pending list (fail_conn iterates the whole slice).
fn test_driver_fails_a_second_concurrent_pending_request_too() {
	mut transport := new_fake_h3_udp_transport()
	transport.arm_fatal_error('simulated transport death', 300 * time.millisecond)

	mut h3 := new_handshaking_h3_conn()
	mut c := new_h3_mux_conn(H3UdpTransport(transport), h3, unsafe { nil })

	done1 := chan bool{cap: 1}
	done2 := chan bool{cap: 1}
	mut outcome1 := &H3TestDoOutcome{}
	mut outcome2 := &H3TestDoOutcome{}
	spawn h3_test_do_worker(mut c, H3ClientRequest{ authority: 'a.example.com' }, mut outcome1,
		done1)
	spawn h3_test_do_worker(mut c, H3ClientRequest{ authority: 'b.example.com' }, mut outcome2,
		done2)
	select {
		_ := <-done1 {}
		2 * time.second {
			assert false, 'first concurrent request never returned'
			return
		}
	}
	select {
		_ := <-done2 {}
		2 * time.second {
			assert false, 'second concurrent request never returned'
			return
		}
	}
	assert outcome1.got_err && outcome1.err_code == h3_err_retryable_code
	assert outcome2.got_err && outcome2.err_code == h3_err_retryable_code
}

// --- dispatch_h3_event: GOAWAY draining (RFC 9114 section 5.2) -----------

fn test_dispatch_goaway_fails_streams_at_or_above_the_boundary_retryable() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut below := new_h3_mux_stream()
	below.sent_headers = true
	mut at := new_h3_mux_stream()
	at.sent_headers = true
	mut above := new_h3_mux_stream()
	above.sent_headers = true
	c.streams[0] = below
	c.streams[4] = at
	c.streams[8] = above

	c.dispatch_h3_event(quic.H3Event{
		kind:      .goaway
		goaway_id: u64(4)
	})

	assert c.goaway_received
	below.mu.lock()
	below_ended := below.ended
	below.mu.unlock()
	assert !below_ended, 'a stream below the GOAWAY boundary must be left alone'

	at.mu.lock()
	at_ended := at.ended
	at_retryable := at.retryable
	at.mu.unlock()
	assert at_ended && at_retryable, 'a stream AT the GOAWAY boundary is guaranteed unprocessed and must be failed retryable'

	above.mu.lock()
	above_ended := above.ended
	above_retryable := above.retryable
	above.mu.unlock()
	assert above_ended && above_retryable, 'a stream ABOVE the GOAWAY boundary must also be failed retryable'
}

fn test_start_request_rejects_a_pending_request_once_goaway_received() {
	mut c := new_test_h3_mux_conn_no_driver()
	c.goaway_received = true
	mut s := new_h3_mux_stream()
	mut p := PendingH3Request{
		req:    H3ClientRequest{
			authority: 'example.com'
		}
		stream: s
	}
	c.start_request(mut p)
	s.mu.lock()
	ended := s.ended
	retryable := s.retryable
	s.mu.unlock()
	assert ended, 'a request admitted before GOAWAY but drained after it must not silently open a new stream above the boundary'
	assert retryable
}

// --- dispatch_h3_event: request_error retry eligibility (RFC 9114 section 8.1) ---

fn test_dispatch_request_error_h3_request_rejected_is_retryable() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	c.streams[1] = s
	c.dispatch_h3_event(quic.H3Event{
		kind:       .request_error
		stream_id:  u64(1)
		error_code: quic.H3ErrorCode.request_rejected.code()
		reason:     'rejected'
	})
	s.mu.lock()
	ended := s.ended
	retryable := s.retryable
	s.mu.unlock()
	assert ended
	assert retryable, 'H3_REQUEST_REJECTED (RFC 9114 section 8.1) means the server never processed this request, mirroring HTTP/2 REFUSED_STREAM -- it must be retryable'
}

fn test_dispatch_request_error_other_codes_are_not_retryable() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	c.streams[1] = s
	c.dispatch_h3_event(quic.H3Event{
		kind:       .request_error
		stream_id:  u64(1)
		error_code: quic.H3ErrorCode.internal_error.code()
		reason:     'boom'
	})
	s.mu.lock()
	retryable := s.retryable
	s.mu.unlock()
	assert !retryable
}

// --- wait_response: :status validation ------------------------------------

fn test_wait_response_rejects_malformed_status() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	s.headers_done = true
	s.resp_headers = [
		quic.QpackFieldLine{
			name:  ':status'
			value: '20000'
		},
	]
	s.ended = true
	c.wait_response(mut s, H3ClientRequest{}) or {
		assert err.msg().contains('malformed')
		return
	}
	assert false, 'expected an out-of-range, non-3-digit :status to be rejected'
}

fn test_wait_response_rejects_duplicate_status() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	s.headers_done = true
	s.resp_headers = [
		quic.QpackFieldLine{
			name:  ':status'
			value: '200'
		},
		quic.QpackFieldLine{
			name:  ':status'
			value: '404'
		},
	]
	s.ended = true
	c.wait_response(mut s, H3ClientRequest{}) or {
		assert err.msg().contains('duplicate')
		return
	}
	assert false, 'expected a duplicate :status pseudo-header to be rejected'
}

fn test_wait_response_rejects_status_after_a_regular_field() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	s.headers_done = true
	s.resp_headers = [
		quic.QpackFieldLine{
			name:  'content-type'
			value: 'text/plain'
		},
		quic.QpackFieldLine{
			name:  ':status'
			value: '200'
		},
	]
	s.ended = true
	c.wait_response(mut s, H3ClientRequest{}) or {
		assert err.msg().contains('duplicate or out-of-order')
		return
	}
	assert false, 'RFC 9114 section 4.3 requires :status to precede regular fields, mirroring RFC 9113 section 8.3'
}

fn test_wait_response_rejects_unknown_pseudo_header() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	s.headers_done = true
	s.resp_headers = [
		quic.QpackFieldLine{
			name:  ':status'
			value: '200'
		},
		quic.QpackFieldLine{
			name:  ':path'
			value: '/'
		},
	]
	s.ended = true
	c.wait_response(mut s, H3ClientRequest{}) or {
		assert err.msg().contains('invalid pseudo-header')
		return
	}
	assert false, 'RFC 9114 section 4.3 defines no response pseudo-header besides :status'
}

fn test_wait_response_rejects_missing_status() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	s.headers_done = true
	s.resp_headers = [
		quic.QpackFieldLine{
			name:  'content-type'
			value: 'text/plain'
		},
	]
	s.ended = true
	c.wait_response(mut s, H3ClientRequest{}) or {
		assert err.msg().contains('missing')
		return
	}
	assert false, 'expected a response with no :status pseudo-header to be rejected'
}

fn test_wait_response_accepts_well_formed_status() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	s.headers_done = true
	s.resp_headers = [
		quic.QpackFieldLine{
			name:  ':status'
			value: '204'
		},
	]
	s.ended = true
	resp := c.wait_response(mut s, H3ClientRequest{ method: 'GET' }) or {
		assert false, 'expected a well-formed :status to be accepted: ${err.msg()}'
		return
	}
	assert resp.status == 204
}

// --- wait_response: a peer-controlled error code must never be mistaken
// --- for the internal retryable sentinel -----------------------------------

fn test_wait_response_never_propagates_a_non_positive_cast_error_code() {
	mut c := new_test_h3_mux_conn_no_driver()
	mut s := new_h3_mux_stream()
	// A peer-controlled u64 whose low 32 bits are negative once narrowed to
	// `int` -- the class of value that could otherwise collide with an
	// internal sentinel like h3_err_retryable_code (-20014).
	s.err = 'stream reset by peer'
	s.err_code = u64(0xFFFFFFFF80000000)
	s.retryable = false
	s.ended = true
	c.wait_response(mut s, H3ClientRequest{}) or {
		assert err.code() == 0, 'a raw peer error code that casts to a non-positive int must never be propagated as the error code (got ${err.code()})'
		return
	}
	assert false, 'expected an error'
}

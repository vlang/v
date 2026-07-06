// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import sync
import time

// This file implements a multiplexed HTTP/2 client connection: one connection
// carries many concurrent request streams. A background reader thread (the
// only reader of the transport, and the sole owner of the HPACK decoder and
// read buffer) demuxes incoming frames to per-stream state; request threads
// wait on their stream's condition variable. It complements the synchronous
// single-stream H2Conn in h2_conn.v, which remains for the one-shot paths.
//
// Locking protocol — a thread holds at most one of {smu, fmu, wmu} at a time,
// with two permitted nestings: wmu → smu and smu → fmu (so wmu → smu → fmu).
// No lock is ever taken in the reverse direction, which keeps the order
// acyclic. A stream's own mu is never held while taking a connection lock.
// The smu → fmu nesting is what serializes stream registration (which assigns
// the stream's initial send window) against WINDOW_UPDATE credit and SETTINGS
// initial-window deltas, so early credit can never be lost.
//   wmu      transport writes, the HPACK encoder, stream-id allocation +
//            registration + HEADERS send (one atomic critical section), and
//            the lazy connection preface.
//   fmu/fcv  flow control: the connection send window, every stream's send
//            window, and the peer's initial-window/max-frame mirrors. fcv is
//            broadcast whenever a window can grow and when the connection
//            dies, so blocked senders always wake.
//   smu      connection state: the stream map, refcount, active stream count,
//            goaway/closed/shutting_down, and the peer max-streams mirror.

// h2_max_recv_header_block caps the total size of a received HEADERS(+
// CONTINUATION) block. Without it a peer can stream unbounded CONTINUATION
// frames and exhaust memory (the CONTINUATION-flood DoS, CVE-2024-27316 class).
const h2_max_recv_header_block = 1024 * 1024

// h2_max_pending_preface_acks caps how many control-frame ACKs (SETTINGS + PING)
// may be deferred while waiting to send the lazy client preface. A well-behaved
// server sends only its preface SETTINGS (and perhaps a PING) before ours, so a
// small bound is ample; without it a peer that holds the connection pre-handshake
// could flood PING/SETTINGS and grow the deferred-ACK buffers without limit.
const h2_max_pending_preface_acks = 64

// h2_err_retryable_code tags stream errors where the request provably never
// reached server processing (GOAWAY-unprocessed, connection closed before the
// request was sent, admission refused), so it is safe to retry on a fresh
// connection even for non-idempotent methods.
pub const h2_err_retryable_code = -20012

// h2_retryable_error builds an error carrying h2_err_retryable_code.
fn h2_retryable_error(reason string) IError {
	return error_with_code('h2: ${reason}', h2_err_retryable_code)
}

// all_digits reports whether s is non-empty and every byte is an ASCII digit.
// Header values must be validated with this before lenient string.int()/.u64()
// parsing, which otherwise accept malformed input like '200 OK' or '12junk'.
fn all_digits(s string) bool {
	if s.len == 0 {
		return false
	}
	for ch in s {
		if ch < `0` || ch > `9` {
			return false
		}
	}
	return true
}

// h2_conn_specific_headers are connection-specific header fields that MUST NOT
// appear in any HTTP/2 message (RFC 9113 §8.2.2). A received response or trailer
// carrying one is malformed. (TE is the request-only exception and is handled on
// the send side, so it is not listed here.)
const h2_conn_specific_headers = ['connection', 'keep-alive', 'proxy-connection', 'transfer-encoding',
	'upgrade']

// h2_response_field_error returns a non-empty reason when a regular (non-pseudo)
// received header field is malformed per RFC 9113 §8.2, or '' when it is valid.
// Field names must be non-empty and lowercase (§8.2.1), and connection-specific
// fields are forbidden (§8.2.2). A malformed field makes the whole message
// malformed (§8.1.1); the mux path resets the stream and the sync path fails the
// request rather than delivering it. Pseudo-header validity is checked at the
// call site (the set of valid pseudo-headers differs between headers/trailers).
fn h2_response_field_error(name string) string {
	if name.len == 0 {
		return 'empty header field name'
	}
	for ch in name {
		if ch >= `A` && ch <= `Z` {
			return 'uppercase header field name "${name}"'
		}
	}
	if name in h2_conn_specific_headers {
		return 'connection-specific header field "${name}"'
	}
	return ''
}

// h2_header_list_size returns the RFC 9113 §6.5.2 size of a header list: the sum
// over all fields of (name length + value length + 32 octets of per-field
// overhead). Used to honor the peer's advisory SETTINGS_MAX_HEADER_LIST_SIZE.
fn h2_header_list_size(fields []H2HeaderField) u64 {
	mut total := u64(0)
	for f in fields {
		total += u64(f.name.len) + u64(f.value.len) + 32
	}
	return total
}

// H2MuxStream is the client-side state of one in-flight request stream.
@[heap]
struct H2MuxStream {
mut:
	id u32
	// --- response state, guarded by mu, signaled via cv ---
	mu            &sync.Mutex = unsafe { nil }
	cv            &sync.Cond  = unsafe { nil }
	status        int
	resp_headers  []H2HeaderField
	resp_trailers []H2HeaderField // non-pseudo fields from a trailing HEADERS block
	headers_done  bool
	chunks        [][]u8 // DATA payloads appended by the reader, drained by the requester
	body_rcvd     u64    // cumulative DATA bytes received
	ended         bool   // END_STREAM, RST, or connection death
	err           string // non-empty: the stream failed
	err_code      int    // set only by fail_with_code (e.g. net.err_timed_out_code); 0 otherwise
	retryable     bool   // the failure is safe to retry on a fresh connection
	sent_headers  bool   // the request HEADERS reached the transport
	cancelled     bool   // requester abandoned the stream; drop+credit late DATA
	send_closed   bool   // we closed our send side (sent END_STREAM or RST_STREAM)
	// last_activity is bumped on every HEADERS/trailers/DATA arrival, so the
	// response-wait watchdog can enforce read_timeout as an IDLE timeout (like
	// HTTP/1.1 and the one-shot H2Conn path) instead of a single deadline for
	// the whole response -- see h2_stream_timeout_watchdog.
	last_activity  time.Time
	upload_started time.Time // fixed baseline for the upload watchdog; deliberately never reset
	// in_callback is true while wait_response is inside a caller-supplied
	// on_data/progress callback (run outside mu so it can block without
	// stalling the reader). read_timeout documents itself as bounding how
	// long a request waits for a response over the WIRE, not how long the
	// caller's own callback takes to process a delivered chunk; without this,
	// a callback that applies backpressure or does slow work for longer than
	// read_timeout gets its stream reset even though nothing is actually
	// stalled on I/O (Codex P2, vlang/v#27643 pullrequestreview-4631763931,
	// discussion 3525390898). h2_response_phase_since treats this as "the
	// clock is paused" so the watchdog never counts callback time against the
	// budget, however long the callback runs.
	in_callback bool
	// --- send state, guarded by the connection's fmu ---
	send_window i64
	send_dead   bool // RST/GOAWAY: wake a body sender blocked on flow-control credit
	// --- receive state, guarded by mu ---
	recv_window i64 = i64(h2_default_initial_window) // our receive window (what we advertised)
}

fn new_h2_mux_stream() &H2MuxStream {
	mu := sync.new_mutex()
	return &H2MuxStream{
		mu:            mu
		cv:            sync.new_cond(mu)
		last_activity: time.now()
	}
}

// fail marks the stream failed and wakes its requester.
fn (mut s H2MuxStream) fail(msg string, retryable bool) {
	s.mu.lock()
	if !s.ended {
		s.err = msg
		s.retryable = retryable
		s.ended = true
		s.cv.signal()
	}
	s.mu.unlock()
}

// fail_with_code marks the stream failed with a specific error code, so a
// caller of do() can recognize it (request.v's is_no_need_retry_error checks
// err.code() to decide whether a failure is worth retrying at all). Not
// retryable: unlike a GOAWAY-unprocessed or admission-refused stream, a
// client-side read timeout does not prove the request never reached the
// server. Used by h2_stream_timeout_watchdog; ordinary connection/stream
// failures keep using plain fail() (err_code stays 0, and wait_response falls
// back to an uncoded error for them, unchanged). Returns true when this call
// actually transitioned the stream (it was not already ended), so the caller
// can tell whether it "won" a race against the phase finishing on its own —
// h2_stream_timeout_watchdog uses this to decide whether the peer still needs
// an RST_STREAM.
fn (mut s H2MuxStream) fail_with_code(msg string, code int) bool {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	if s.ended {
		return false
	}
	s.err = msg
	s.err_code = code
	s.retryable = false
	s.ended = true
	s.cv.signal()
	return true
}

// H2MuxConn is a multiplexed client-side HTTP/2 connection, safe for
// concurrent requests from multiple threads.
@[heap]
pub struct H2MuxConn {
mut:
	transport H2Transport
	// close_transport is called exactly once during teardown and MUST close or
	// interrupt the transport so the reader thread (blocked in transport.read())
	// can exit. It is required (non-nil): the H2Transport interface has no close()
	// of its own, so this is the only way to unblock the reader. new_h2_mux_conn
	// panics on a nil closer.
	close_transport fn () = unsafe { nil }
	// --- guarded by wmu ---
	wmu                       &sync.Mutex = sync.new_mutex()
	encoder                   H2HpackEncoder
	next_stream_id            u32 = 1
	handshaked                bool
	pending_settings_acks     int    // count of SETTINGS frames received before our preface; each needs one ACK
	pending_ping_acks         [][]u8 // PING frames received before our preface; each needs an ACK with the same data
	peer_max_header_list_size u32 = max_u32 // peer SETTINGS_MAX_HEADER_LIST_SIZE (advisory, §6.5.2)
	// --- guarded by fmu, fcv signals growth/death ---
	fmu                 &sync.Mutex = unsafe { nil }
	fcv                 &sync.Cond  = unsafe { nil }
	send_window         i64         = i64(h2_default_initial_window)
	peer_initial_window i64         = i64(h2_default_initial_window)
	peer_max_frame      u32         = h2_default_max_frame_size
	fmu_dead            bool // mirror of `closed` for blocked senders
	// --- guarded by smu ---
	smu                 &sync.Mutex = sync.new_mutex()
	streams             map[u32]&H2MuxStream
	refs                int = 1 // the owner's (pool's) reference; +1 per in-flight request
	active_streams      int
	max_streams         u32 = 100 // our own cap on concurrent streams
	peer_max_streams    u32 = max_u32
	goaway              bool
	goaway_last         u32
	closed              bool // the reader has exited; only the reader sets this
	shutting_down       bool
	transport_torn_down bool // close_transport has been (or is being) called
	conn_err            string
	idle_since          time.Time
	// --- guarded by recv_wmu ---
	// Inbound flow-control: tracks how much more data the peer is allowed to
	// send us (our advertised receive windows). Debited on DATA arrival,
	// replenished when we send WINDOW_UPDATE. recv_wmu is always acquired
	// solo — never while holding any other connection lock.
	recv_wmu         &sync.Mutex = sync.new_mutex()
	conn_recv_window i64         = i64(h2_default_initial_window)
	// --- reader-thread private (no locks) ---
	decoder H2HpackDecoder
	rbuf    []u8
}

// new_h2_mux_conn creates a multiplexed connection over `transport` and starts
// its background reader. The connection preface is sent lazily with the first
// request. `close_transport` is REQUIRED (non-nil): it is called once at final
// teardown and MUST close/interrupt the transport so the blocked reader exits.
// A nil closer is a programming error and panics here, because the reader can
// only be unblocked by closing the transport (see the field comment).
//
// CONCURRENCY REQUIREMENT: the background reader calls `transport.read()` while
// request threads call `transport.write()`, so `transport` MUST be safe for a
// read and a write to run simultaneously on separate threads. A raw TCP socket
// is (it is full-duplex); a TLS connection is NOT — one OpenSSL/mbedTLS `SSL`
// object must not be read and written at the same time. Wiring this to a TLS
// backend therefore requires a concurrent-safe transport adapter (a serialized
// or non-blocking I/O loop); a single blocking I/O mutex is insufficient because
// it would deadlock the blocked reader. That adapter is provided when the pooled
// transport is wired up (see the HTTP connection-pooling Phase 3 work); until
// then this type is exercised only over a full-duplex in-memory pipe in tests.
pub fn new_h2_mux_conn(transport H2Transport, close_transport fn ()) &H2MuxConn {
	if close_transport == unsafe { nil } {
		// The reader thread spawned below blocks in transport.read(); the
		// H2Transport interface has no close(), so this callback is the only way
		// to interrupt that read at teardown. Without it an idle retirement or
		// last-reference release would leak the reader thread and the connection.
		panic('new_h2_mux_conn: close_transport must be non-nil so teardown can wake the reader')
	}
	fmu := sync.new_mutex()
	mut c := &H2MuxConn{
		transport:       transport
		close_transport: close_transport
		fmu:             fmu
		fcv:             sync.new_cond(fmu)
		idle_since:      time.now()
	}
	spawn c.read_loop()
	return c
}

// can_take_new_request reports whether a new request may be admitted on this
// connection right now (it may still be refused later under smu).
pub fn (mut c H2MuxConn) can_take_new_request() bool {
	c.smu.lock()
	defer {
		c.smu.unlock()
	}
	limit := if c.peer_max_streams < c.max_streams { c.peer_max_streams } else { c.max_streams }
	return !c.closed && !c.goaway && !c.shutting_down && u32(c.active_streams) < limit
}

// is_idle_expired reports whether this connection has had zero in-flight
// streams for longer than `idle_timeout` -- the h2 pool's counterpart to
// Transport.checkout's h1_idle expiry check (transport.v). Without this,
// h2_round_trip's fast path only checked can_take_new_request() (stream
// capacity), so a pooled h2 connection could sit idle indefinitely past the
// documented idle_timeout and still be reused, unlike h1 (Codex P2,
// vlang/v#27643 pullrequestreview-4631763931, discussion 3525390896).
pub fn (mut c H2MuxConn) is_idle_expired(idle_timeout time.Duration) bool {
	c.smu.lock()
	defer {
		c.smu.unlock()
	}
	return c.active_streams == 0 && time.now() - c.idle_since > idle_timeout
}

// shutdown_when_idle asks the connection to retire: no new requests are
// admitted, and once no requests are in flight and the owner's reference is
// released, the transport is closed.
pub fn (mut c H2MuxConn) shutdown_when_idle() {
	c.smu.lock()
	c.shutting_down = true
	c.smu.unlock()
}

// release drops the owner's reference. The connection tears its transport
// down once the reader has exited and all references are gone.
pub fn (mut c H2MuxConn) release() {
	c.drop_ref()
}

fn (mut c H2MuxConn) drop_ref() {
	c.smu.lock()
	c.refs--
	last_ref := c.refs <= 0
	c.smu.unlock()
	if last_ref {
		// Tear the transport down even if the reader has not exited yet: closing
		// it interrupts the reader's blocking read so it can exit. Otherwise an
		// idle connection that is shut down and released leaks the reader thread
		// and the socket, because the reader only sets `closed` after a read
		// error/timeout, which never arrives on a silent transport.
		c.teardown_transport()
	}
}

// teardown_transport runs the close_transport callback exactly once. Calling it
// while the reader is still blocked in transport.read() interrupts that read, so
// the reader observes the closed transport and exits via fail_conn.
fn (mut c H2MuxConn) teardown_transport() {
	c.smu.lock()
	already := c.transport_torn_down
	c.transport_torn_down = true
	c.smu.unlock()
	if !already && c.close_transport != unsafe { nil } {
		c.close_transport()
	}
}

// --- request side -----------------------------------------------------------

// do sends one request over the connection, concurrently with other streams,
// and returns its response. Errors carrying h2_err_retryable_code are safe to
// retry on a fresh connection.
pub fn (mut c H2MuxConn) do(req H2ClientRequest) !H2ClientResponse {
	// Admission.
	c.smu.lock()
	if c.closed {
		reason := if c.conn_err != '' { c.conn_err } else { 'connection is closed' }
		c.smu.unlock()
		return h2_retryable_error(reason)
	}
	if c.goaway || c.shutting_down {
		c.smu.unlock()
		return h2_retryable_error('connection is shutting down')
	}
	limit := if c.peer_max_streams < c.max_streams { c.peer_max_streams } else { c.max_streams }
	if u32(c.active_streams) >= limit {
		c.smu.unlock()
		return h2_retryable_error('connection is at its concurrent stream limit')
	}
	c.refs++
	c.active_streams++
	c.smu.unlock()

	mut s := new_h2_mux_stream()
	resp := c.do_on_stream(mut s, req) or {
		c.finish_stream(mut s)
		return err
	}
	c.finish_stream(mut s)
	return resp
}

// h2_watchdog_poll_interval bounds how long h2_stream_timeout_watchdog sleeps
// per iteration, so a phase that completes well before its deadline (the
// common case: req.read_timeout defaults to 30s, but most uploads/responses
// complete in milliseconds) does not leave its watchdog thread sleeping for
// the full deadline — every pooled h2 request with a body spawns two of
// these in sequence (upload, then response), so without this bound a busy
// connection under sustained throughput would accumulate lingering threads
// for up to each phase's own timeout.
const h2_watchdog_poll_interval = 200 * time.millisecond

// h2_stream_upload_stall_limit bounds the upload phase, deliberately
// DECOUPLED from req.read_timeout (see h2_stream_timeout_watchdog's doc
// comment): read_timeout is documented as the response-read deadline, so a
// slow-but-healthy upload — potentially far longer than a caller's short
// read_timeout, over a constrained link or with a server that is merely slow
// to drain — must still succeed (Codex P1, vlang/v#27643
// pullrequestreview-4628439062; test_mux_read_timeout_excludes_upload_blocked_phase
// asserts exactly this: upload blocked well past read_timeout, then
// succeeds). Not user-configurable — there is no separate upload-timeout
// field — and deliberately generous so it only catches a peer that will
// simply never grant flow-control credit at all (never RSTs, never GOAWAYs,
// just goes quiet), which would otherwise pin the stream, the pooled
// connection (active_streams never reaches 0, so it is never idle-evicted
// either), and the caller's thread forever. Overridable via `-d
// h2_stream_upload_stall_limit_ms=<n>` (mirrors
// vlib/net/mbedtls/ssl_connection.c.v's mbedtls_client_read_timeout_ms) so a
// test can observe the watchdog actually firing without a real 5-minute wait.
const h2_stream_upload_stall_limit_ms = $d('h2_stream_upload_stall_limit_ms', 300_000)
const h2_stream_upload_stall_limit = h2_stream_upload_stall_limit_ms * time.millisecond

// h2_stream_timeout_watchdog enforces a timeout on ONE phase of a pooled h2
// request — uploading the body (h2_stream_upload_stall_limit), or waiting
// for the response (req.read_timeout) — rather than the request as a whole:
// H2MuxConn's background reader treats every transport read timeout as a
// benign wake-up (it must survive one to keep servicing other streams), so
// nothing else bounds either phase's wait. Splitting it into two calls (see
// do_on_stream) instead of one watchdog spanning the entire call, each with
// its own budget, is what keeps the two phases' timeouts independent.
//
// `done` reports whether the phase this call covers has already finished,
// checked under s.mu by the caller-supplied function (called with the same
// `s` this watchdog was spawned with — NOT a captured closure: V's closure
// codegen conflates the capture context of two `fn [s] () T {...}` literals
// with identical signatures declared in the same enclosing function, so
// do_on_stream's upload- and response-phase callbacks silently aliased and
// the second one observed only the first one's stale, spawn-time snapshot.
// Passing `s` as an explicit parameter to plain, non-capturing named
// functions sidesteps that entirely). Polls in short bounded steps rather
// than one long sleep, so it exits promptly once the phase actually completes
// instead of always lingering for the full timeout (see
// h2_watchdog_poll_interval). Whether it exits early or fires (fail_with_code,
// a no-op if the stream already ended for any other reason — e.g. a
// RST_STREAM/GOAWAY mid-upload, which the upload watchdog's `done` also
// treats as "phase over" so it does not linger for the rest of its own
// timeout in that case), it always exits after a bounded number of steps, so
// it can never leak regardless of how the request actually finishes.
// wake_send covers both phases: a body upload blocked on flow-control credit
// (send_body_on_stream waits on c.fcv, a different condition variable than
// s.cv, so failing the stream alone would not wake it) and a response wait
// blocked on s.cv — safe to call unconditionally, matching every other
// fail()+wake_send call site in this file.
//
// `since` reports the timestamp the remaining budget counts down from,
// re-read every poll: the upload watchdog reads s.upload_started, a fixed
// point set once at spawn (deliberately NOT reset on progress — see
// h2_stream_upload_stall_limit's doc comment), while the response watchdog
// reads s.last_activity, which on_response_headers/on_response_data bump on
// every HEADERS/DATA arrival. Without this, req.read_timeout behaved as a
// single absolute deadline for the whole response instead of an idle timeout
// like HTTP/1.1 and the one-shot H2Conn path, so a response that kept
// delivering DATA past read_timeout overall was killed mid-transfer (Codex
// P1, vlang/v#27643 pullrequestreview-4630174759).
//
// When the timeout fires (fail_with_code returns true — this call actually
// transitioned the stream, so no other path already resolved it), the peer is
// told via RST_STREAM: otherwise the client abandons the stream locally while
// the peer keeps it open — and counted against
// SETTINGS_MAX_CONCURRENT_STREAMS — indefinitely (Codex P2, same review).
fn h2_stream_timeout_watchdog(mut c H2MuxConn, mut s H2MuxStream, timeout time.Duration, phase string, done fn (&H2MuxStream) bool, since fn (&H2MuxStream) time.Time) {
	for {
		remaining := timeout - (time.now() - since(s))
		if remaining <= 0 {
			break
		}
		step := if remaining < h2_watchdog_poll_interval {
			remaining
		} else {
			h2_watchdog_poll_interval
		}
		time.sleep(step)
		if done(s) {
			return
		}
	}
	if s.fail_with_code('request timed out ${phase} after ${timeout}', net.err_timed_out_code) {
		c.wake_send(mut s)
		c.rst_abandoned_stream(s.id, .cancel)
	}
}

fn h2_upload_phase_done(s &H2MuxStream) bool {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	return s.send_closed || s.ended
}

fn h2_upload_phase_since(s &H2MuxStream) time.Time {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	return s.upload_started
}

fn h2_response_phase_done(s &H2MuxStream) bool {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	return s.ended
}

fn h2_response_phase_since(s &H2MuxStream) time.Time {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	if s.in_callback {
		// A caller callback is running right now, outside mu — report "now"
		// so the watchdog's elapsed-time computation is always ~0 for as long
		// as the callback keeps the stream in this state, regardless of how
		// long it takes. wait_response bumps last_activity for real the
		// moment the callback returns, so a genuine post-callback network
		// stall is still caught normally.
		return time.now()
	}
	return s.last_activity
}

// rst_abandoned_stream deregisters a stream the client has already given up
// on locally (its requester's failure was already recorded via fail() or
// fail_with_code()) and sends RST_STREAM(code) so the peer stops
// processing/counting it. Deregistering first, like cancel_stream/
// reset_stream, routes any DATA still in flight for it to the unknown-stream
// backstop instead of queuing onto a stream nobody drains; finish_stream's
// later cleanup credits any bytes already queued before that point.
fn (mut c H2MuxConn) rst_abandoned_stream(stream_id u32, code H2ErrorCode) {
	c.smu.lock()
	c.streams.delete(stream_id)
	c.smu.unlock()
	c.wmu.lock()
	c.write_all_locked(H2Frame(H2RstStreamFrame{
		stream_id:  stream_id
		error_code: u32(code)
	}).encode()) or {
		c.wmu.unlock()
		c.note_write_failure()
		return
	}
	c.wmu.unlock()
}

// finish_stream removes the stream from the connection and releases the
// request's reference (possibly triggering teardown).
fn (mut c H2MuxConn) finish_stream(mut s H2MuxStream) {
	c.smu.lock()
	c.streams.delete(s.id)
	c.active_streams--
	c.idle_since = time.now()
	c.smu.unlock()
	// Credit any DATA bytes that were queued in s.chunks but never drained.
	// On the success path wait_response empties chunks before returning, so
	// queued is 0. On the error path (do_on_stream returned early, e.g. after
	// a RST_STREAM arrived mid-upload), chunks may hold bytes whose flow-size
	// was already debited from conn_recv_window; without this credit those bytes
	// are silently dropped, permanently shrinking the connection receive window.
	// Setting cancelled prevents a DATA frame in flight from re-queuing onto
	// the now-deregistered stream and leaking more bytes.
	s.mu.lock()
	s.cancelled = true
	mut queued := u64(0)
	for ch in s.chunks {
		queued += u64(ch.len)
	}
	s.chunks.clear()
	s.mu.unlock()
	if queued > 0 {
		c.send_conn_window_update(u32(queued)) or { c.note_write_failure() }
	}
	c.drop_ref()
}

fn (mut c H2MuxConn) do_on_stream(mut s H2MuxStream, req H2ClientRequest) !H2ClientResponse {
	mut fields := [
		H2HeaderField{':method', req.method},
		H2HeaderField{':scheme', req.scheme},
		H2HeaderField{':authority', req.authority},
		H2HeaderField{':path', req.path},
	]
	for h in req.headers {
		fields << h
	}
	has_body := req.body.len > 0

	// Stream-id allocation, registration, HPACK encoding and the HEADERS(+
	// CONTINUATION) send form one wmu critical section: ids must hit the wire
	// in increasing order, header blocks must be contiguous, and the stream
	// must be registered before its first byte is sent so the reader can
	// always deliver the response.
	c.wmu.lock()
	c.handshake_locked() or {
		// Preface write failed: tear down the transport so the reader exits
		// and the pool stops admitting new work to this dead connection.
		// Without this, closed/shutting_down stay false and the pool can keep
		// dispatching to a connection whose write side is already broken.
		c.wmu.unlock()
		c.note_write_failure()
		return h2_retryable_error('connection handshake failed: ${err.msg()}')
	}
	// RFC 9113 §6.5.2: honor the peer's advisory SETTINGS_MAX_HEADER_LIST_SIZE.
	// Refuse an over-limit request here rather than emit it and have the server
	// reject it (e.g. 431). Not retryable: a fresh connection to the same peer
	// carries the same limit. Read under wmu (apply_peer_settings sets it there).
	peer_max_list := c.peer_max_header_list_size
	if peer_max_list != max_u32 && h2_header_list_size(fields) > u64(peer_max_list) {
		c.wmu.unlock()
		return error('h2: request header list (${h2_header_list_size(fields)} bytes) exceeds peer SETTINGS_MAX_HEADER_LIST_SIZE (${peer_max_list})')
	}
	if c.next_stream_id > u32(0x7fff_ffff) {
		// RFC 7540 §5.1.1: client stream IDs are odd and must not exceed 2^31-1.
		// Retire this connection and let the caller open a fresh one.
		c.smu.lock()
		c.shutting_down = true
		c.smu.unlock()
		c.wmu.unlock()
		return h2_retryable_error('stream ID space exhausted')
	}
	s.id = c.next_stream_id
	c.next_stream_id += 2
	// Mark the HEADERS as sent *before* the stream becomes visible to the
	// reader: pessimistic, so a connection death racing this section can never
	// classify a request whose HEADERS may have reached the wire as safe to
	// replay. (The stream is still private here, so no lock is needed.)
	s.sent_headers = true
	c.smu.lock() // wmu -> smu -> fmu is the permitted lock nesting
	// A terminal event (GOAWAY, or fail_conn after the reader saw the transport
	// die) can land while we are blocked on wmu, after do()'s admission check
	// already passed. Such an event fails every stream in the map, but this one
	// is not registered yet, so it would slip through and wait_response could
	// block forever. Recheck under smu — the same lock those events use to set
	// these flags — and abort before registering or sending. The HEADERS never
	// hit the wire here, so the request is safe to retry on a fresh connection.
	// Also recheck the peer's stream limit: SETTINGS_MAX_CONCURRENT_STREAMS can
	// arrive while we are blocked on wmu, lowering the limit below active_streams.
	if c.closed || c.goaway || c.shutting_down {
		reason := if c.conn_err != '' { c.conn_err } else { 'connection is shutting down' }
		c.smu.unlock()
		c.wmu.unlock()
		return h2_retryable_error(reason)
	}
	recheck_limit := if c.peer_max_streams < c.max_streams {
		c.peer_max_streams
	} else {
		c.max_streams
	}
	if u32(c.active_streams) > recheck_limit {
		c.smu.unlock()
		c.wmu.unlock()
		return h2_retryable_error('peer lowered concurrent stream limit; retrying on a fresh connection')
	}
	// If this was the last valid client stream ID (RFC 7540 §5.1.1: max 2^31-1),
	// retire the connection so can_take_new_request() returns false immediately.
	// Without this, the pool dispatches one extra request that hits the admission
	// check at the top of do_on_stream and fails with a retryable error.
	if c.next_stream_id > u32(0x7fff_ffff) {
		c.shutting_down = true
	}
	c.fmu.lock()
	// The initial send window must be assigned atomically with registration:
	// the WINDOW_UPDATE and SETTINGS handlers also nest smu -> fmu, so credit
	// or a delta arriving right after our HEADERS serializes after this
	// assignment instead of being overwritten by it.
	s.send_window = c.peer_initial_window
	c.streams[s.id] = s
	c.fmu.unlock()
	c.smu.unlock()
	block := c.encoder.encode(fields)
	c.send_header_block_locked(s.id, block, !has_body) or {
		c.wmu.unlock()
		c.note_write_failure()
		// The HEADERS may have partially hit the wire; not safe to blind-retry
		// unless the transport wrote nothing, which we cannot distinguish here.
		return error('h2: failed to send request headers: ${err.msg()}')
	}
	c.wmu.unlock()

	if has_body {
		// Bounds the upload phase on h2_stream_upload_stall_limit, NOT
		// req.read_timeout (see that constant's doc comment) and NOT gated
		// on req.read_timeout > 0 — this is a robustness backstop against a
		// permanently wedged peer, independent of the caller's response-read
		// timeout config. `done` also treats the stream ending for any OTHER
		// reason (RST_STREAM, GOAWAY, an early final response) as "phase
		// over", so this watchdog does not linger for the rest of its own
		// timeout when one of those already resolved the upload.
		s.mu.lock()
		s.upload_started = time.now()
		s.mu.unlock()
		spawn h2_stream_timeout_watchdog(mut c, mut s, h2_stream_upload_stall_limit,
			'uploading the request body', h2_upload_phase_done, h2_upload_phase_since)
		c.send_body_on_stream(mut s, req.body)!
	} else {
		// The HEADERS carried END_STREAM, so our send side is now closed.
		s.mu.lock()
		s.send_closed = true
		s.mu.unlock()
	}
	// req.read_timeout documents itself as the timeout for reading the
	// response, so this watchdog is armed here — once the request is fully
	// sent and only the response wait remains — not any earlier. Arming it
	// before the request is fully sent (e.g. in do(), before HEADERS/body
	// ever reach the wire) let it fire while still waiting on wmu behind
	// another writer, or mid-upload on a large POST, aborting a request that
	// had not even started waiting for a response yet (Codex P1, vlang/v#27643
	// pullrequestreview-4628439062).
	if req.read_timeout > 0 {
		// Reset the activity clock to now: the upload phase (above) may have
		// taken arbitrarily long, and none of it should count against the
		// response-wait budget (that would just reintroduce the bug this
		// split was built to fix — see the pullrequestreview-4628439062
		// comment above).
		s.mu.lock()
		s.last_activity = time.now()
		s.mu.unlock()
		spawn h2_stream_timeout_watchdog(mut c, mut s, req.read_timeout, 'waiting for a response',
			h2_response_phase_done, h2_response_phase_since)
	}
	return c.wait_response(mut s, req)
}

// handshake_locked sends the connection preface and our SETTINGS once.
// Callers must hold wmu.
fn (mut c H2MuxConn) handshake_locked() ! {
	if c.handshaked {
		return
	}
	mut buf := h2_client_preface.bytes()
	buf << H2Frame(H2SettingsFrame{
		settings: [
			H2Setting{h2_settings_enable_push, 0},
			H2Setting{h2_settings_initial_window_size, h2_default_initial_window},
			H2Setting{h2_settings_max_frame_size, h2_default_max_frame_size},
		]
	}).encode()
	c.write_all_locked(buf)!
	c.handshaked = true
	// If the reader processed server frames before we sent our preface, ACKs were
	// deferred to preserve client-preface-first ordering (RFC 7540 §3.5). Flush
	// them now: one SETTINGS ACK per received non-ACK SETTINGS, then any PING ACKs.
	for _ in 0 .. c.pending_settings_acks {
		c.write_all_locked(H2Frame(H2SettingsFrame{ ack: true }).encode())!
	}
	c.pending_settings_acks = 0
	for data in c.pending_ping_acks {
		c.write_all_locked(H2Frame(H2PingFrame{ ack: true, data: data }).encode())!
	}
	c.pending_ping_acks.clear()
}

// send_header_block_locked writes a header block as HEADERS(+CONTINUATION)
// frames. Callers must hold wmu.
fn (mut c H2MuxConn) send_header_block_locked(stream_id u32, block []u8, end_stream bool) ! {
	c.fmu.lock()
	mut max := int(c.peer_max_frame)
	c.fmu.unlock()
	if block.len <= max {
		// Re-read peer_max_frame right before writing to close the TOCTOU window:
		// the peer may have lowered SETTINGS_MAX_FRAME_SIZE since the check above.
		c.fmu.lock()
		max = int(c.peer_max_frame)
		c.fmu.unlock()
		if block.len <= max {
			c.write_all_locked(H2Frame(H2HeadersFrame{
				stream_id:   stream_id
				fragment:    block
				end_headers: true
				end_stream:  end_stream
			}).encode())!
			return
		}
		// block no longer fits in one frame under the refreshed limit;
		// fall through to the multi-frame path.
	}
	// Re-read peer_max_frame under fmu immediately before the first HEADERS write
	// to minimise the TOCTOU window between the size-check above and this write.
	c.fmu.lock()
	max = int(c.peer_max_frame)
	c.fmu.unlock()
	// Clamp to block.len in case max grew large enough to fit the whole block.
	first := if max < block.len { max } else { block.len }
	c.write_all_locked(H2Frame(H2HeadersFrame{
		stream_id:   stream_id
		fragment:    block[..first]
		end_headers: first == block.len
		end_stream:  end_stream
	}).encode())!
	if first == block.len {
		return
	}
	mut off := first
	for off < block.len {
		// Re-read peer_max_frame under fmu before each CONTINUATION to minimise
		// the TOCTOU window: the peer could send a smaller SETTINGS_MAX_FRAME_SIZE
		// between iterations and enforce it on the next frame we send.
		c.fmu.lock()
		cur_max := int(c.peer_max_frame)
		c.fmu.unlock()
		mut next := off + cur_max
		if next > block.len {
			next = block.len
		}
		c.write_all_locked(H2Frame(H2ContinuationFrame{
			stream_id:   stream_id
			fragment:    block[off..next]
			end_headers: next == block.len
		}).encode())!
		off = next
	}
}

// send_body_on_stream writes the request body as DATA frames, bounded by both
// the connection and stream send windows.
fn (mut c H2MuxConn) send_body_on_stream(mut s H2MuxStream, body []u8) ! {
	mut off := 0
	for off < body.len {
		// Reserve a window-bounded chunk under fmu (waiting for WINDOW_UPDATE
		// room when both windows are exhausted), then write it under wmu.
		c.fmu.lock()
		for !c.fmu_dead && !s.send_dead && (c.send_window <= 0 || s.send_window <= 0) {
			c.fcv.wait()
		}
		if c.fmu_dead || s.send_dead {
			stream_dead := s.send_dead
			c.fmu.unlock()
			if stream_dead {
				// send_dead is always set together with a terminal stream state,
				// so distinguish the two by whether fail() recorded an error:
				//  - no error  → the server sent END_STREAM (early final response)
				//    while we still owed body; abandon the upload and close our
				//    half (below) so wait_response can still deliver the response.
				//  - error set → RST_STREAM, GOAWAY, or connection death; surface
				//    it here, preserving retryability so a request the server never
				//    processed can be replayed on a fresh connection.
				s.mu.lock()
				failure := s.err
				failure_code := s.err_code
				failure_retryable := s.retryable
				s.mu.unlock()
				if failure != '' {
					if failure_retryable {
						return h2_retryable_error(failure)
					}
					if failure_code != 0 {
						return error_with_code('h2: ${failure}', failure_code)
					}
					return error('h2: ${failure}')
				}
				// Early final response: the server completed and closed its half
				// while we still owed body. RST_STREAM(CANCEL) closes our half so
				// the server releases the stream instead of holding it half-open
				// and counting it against its concurrency limit (RFC 9113 §8.1).
				// The stream stays registered in c.streams, so wait_response still
				// returns the already-received response from the s reference.
				c.wmu.lock()
				c.write_all_locked(H2Frame(H2RstStreamFrame{
					stream_id:  s.id
					error_code: u32(H2ErrorCode.cancel)
				}).encode()) or {
					c.wmu.unlock()
					c.note_write_failure()
					return
				}
				c.wmu.unlock()
				s.mu.lock()
				s.send_closed = true
				s.mu.unlock()
				return
			}
			return error('h2: connection closed while sending the request body')
		}
		mut chunk := body.len - off
		if chunk > int(c.peer_max_frame) {
			chunk = int(c.peer_max_frame)
		}
		if i64(chunk) > c.send_window {
			chunk = int(c.send_window)
		}
		if i64(chunk) > s.send_window {
			chunk = int(s.send_window)
		}
		c.send_window -= i64(chunk)
		s.send_window -= i64(chunk)
		c.fmu.unlock()

		mut next := off + chunk
		c.wmu.lock()
		// Re-cap chunk under wmu→fmu (permitted order): the reader may have
		// processed SETTINGS_MAX_FRAME_SIZE and sent the ACK between our
		// fmu.unlock() above and wmu.lock() here; re-read and return any excess.
		c.fmu.lock()
		if chunk > int(c.peer_max_frame) {
			excess := i64(chunk) - i64(c.peer_max_frame)
			c.send_window += excess
			s.send_window += excess
			chunk = int(c.peer_max_frame)
			next = off + chunk
			// Wake any goroutines sleeping in fcv.wait() because c.send_window
			// was drained; no WINDOW_UPDATE is coming for these bytes since they
			// were never sent.
			c.fcv.broadcast()
		}
		// Also revalidate stream send window: an INITIAL_WINDOW_SIZE reduction
		// applies a negative delta to s.send_window under fmu, so a negative
		// value here means we over-claimed against the peer's new window.  Return
		// the excess bytes to both windows so the loop can re-wait for capacity.
		if s.send_window < 0 {
			trim := if i64(chunk) < -s.send_window { i64(chunk) } else { -s.send_window }
			chunk -= int(trim)
			s.send_window += trim
			c.send_window += trim
			next = off + chunk
			c.fcv.broadcast()
		}
		c.fmu.unlock()
		if chunk == 0 {
			c.wmu.unlock()
			continue
		}
		c.write_all_locked(H2Frame(H2DataFrame{
			stream_id:  s.id
			data:       body[off..next]
			end_stream: next == body.len
		}).encode()) or {
			c.wmu.unlock()
			c.note_write_failure()
			return error('h2: failed to send request body: ${err.msg()}')
		}
		c.wmu.unlock()
		off = next
	}
	// The final DATA frame carried END_STREAM, so our send side is now closed.
	s.mu.lock()
	s.send_closed = true
	s.mu.unlock()
}

// wait_response drains the stream until it ends, honoring the request's
// streaming callback and stop limits. Callbacks run on the requester's thread.
fn (mut c H2MuxConn) wait_response(mut s H2MuxStream, req H2ClientRequest) !H2ClientResponse {
	mut resp := H2ClientResponse{}
	mut body_expected := u64(0)
	mut has_content_length := false
	mut got_headers := false
	mut body_so_far := u64(0)
	mut cancelled := false
	s.mu.lock()
	for {
		// Drain everything currently buffered.
		if !got_headers && s.headers_done {
			resp.status = s.status
			for f in s.resp_headers {
				resp.headers << f
				// on_response_headers already rejects a non-numeric Content-Length,
				// but guard the lenient u64() at the parse site too so this stays
				// correct even if that upstream check is ever changed.
				if f.name == 'content-length' && all_digits(f.value) {
					body_expected = f.value.u64()
					has_content_length = true
				}
			}
			got_headers = true
		}
		mut drained := u64(0)
		for s.chunks.len > 0 {
			chunk := s.chunks[0]
			s.chunks.delete(0)
			body_so_far += u64(chunk.len)
			drained += u64(chunk.len)
			if req.stop_copying_limit < 0
				|| i64(body_so_far) - i64(chunk.len) < req.stop_copying_limit {
				if req.stop_copying_limit >= 0 && i64(body_so_far) > req.stop_copying_limit {
					remaining := req.stop_copying_limit - (i64(body_so_far) - i64(chunk.len))
					if remaining > 0 {
						resp.body << chunk[..int(remaining)]
					}
				} else {
					resp.body << chunk
				}
			}
			if req.on_data != unsafe { nil } {
				// Run the user callback outside the stream lock so it can
				// block without stalling the reader's delivery. in_callback
				// tells the response watchdog to treat this stretch as active
				// (not an idle/stalled read) no matter how long the callback
				// takes — see H2MuxStream.in_callback's doc comment.
				s.in_callback = true
				s.mu.unlock()
				req.on_data(chunk, body_so_far, body_expected, resp.status) or {
					// The callback aborted the request: credit the connection
					// window for what we consumed this round and RST the stream,
					// just like the stop_receiving_limit path, so the connection
					// window does not leak and the peer stops sending. Left
					// unlocked and in_callback still true here is harmless:
					// cancel_stream sets s.ended, which h2_response_phase_done
					// already makes the watchdog's own done() check exit on
					// (within one poll interval) regardless of in_callback.
					if drained > 0 {
						c.send_conn_window_update(u32(drained)) or {}
					}
					c.cancel_stream(mut s)
					return err
				}
				s.mu.lock()
				s.in_callback = false
				s.last_activity = time.now()
			}
			if req.stop_receiving_limit >= 0 && i64(body_so_far) >= req.stop_receiving_limit {
				cancelled = true
				break
			}
		}
		ended := s.ended
		serr := s.err
		serr_code := s.err_code
		retryable := s.retryable
		if cancelled {
			s.mu.unlock()
			// The connection-level window must still be credited for the bytes
			// this round consumed, or the connection's receive window shrinks
			// permanently for every other stream.
			if drained > 0 {
				c.send_conn_window_update(u32(drained)) or {}
			}
			c.cancel_stream(mut s)
			if !got_headers {
				return error('h2: stream cancelled before a response arrived')
			}
			return resp
		}
		if drained > 0 {
			// Replenish flow control for what was just consumed, outside s.mu.
			s.mu.unlock()
			c.send_window_updates(s.id, u32(drained)) or {}
			s.mu.lock()
			// New chunks may have arrived while unlocked; loop and re-drain.
			if s.chunks.len > 0 || (s.ended && !ended) {
				continue
			}
		}
		if ended {
			// Surface any trailers (a second HEADERS block) into the response
			// headers, mirroring the synchronous H2Conn.read_response path. The
			// stream has ended, so resp_trailers is complete; copy under the lock.
			for f in s.resp_trailers {
				resp.headers << f
			}
			s.mu.unlock()
			if serr != '' {
				if retryable {
					return h2_retryable_error(serr)
				}
				if serr_code != 0 {
					return error_with_code('h2: ${serr}', serr_code)
				}
				return error('h2: ${serr}')
			}
			if !got_headers {
				return error('h2: stream closed without a response')
			}
			// RFC 9110 §8.6: a Content-Length must match the bytes received.
			// Skip for responses defined to carry no body — HEAD requests and
			// 204/304 status codes — where Content-Length describes the absent
			// representation rather than transmitted DATA.
			body_allowed := req.method != 'HEAD' && resp.status != 204 && resp.status != 304
			if has_content_length && body_allowed && body_so_far != body_expected {
				return error('h2: response body length ${body_so_far} does not match Content-Length ${body_expected}')
			}
			return resp
		}
		s.cv.wait()
	}
	return resp
}

// cancel_stream aborts a stream early (stop_receiving_limit): the stream is
// deregistered first and then RST_STREAM is sent, so any in-flight late DATA
// for it is handled by the reader's unknown-stream backstop (connection-level
// WINDOW_UPDATE), keeping the connection fully usable for other streams.
fn (mut c H2MuxConn) cancel_stream(mut s H2MuxStream) {
	// Deregister first, then send RST_STREAM: once the stream is gone from
	// c.streams, any DATA the reader already had in flight for it hits the
	// unknown-stream backstop, which credits the connection-level receive
	// window. If we sent RST first, that in-flight DATA could still find the
	// registered stream and be queued without a WINDOW_UPDATE (the cancelled
	// requester has stopped draining), permanently leaking connection window.
	c.smu.lock()
	c.streams.delete(s.id)
	c.smu.unlock()
	// DATA already queued before cancellation consumed the connection receive
	// window but will never be drained; credit it back (its padding was already
	// credited on receipt, so only the chunk data remains). Setting `cancelled`
	// under s.mu also makes a frame still in flight credit-and-drop in
	// on_response_data instead of queuing onto a stream nobody drains. Without
	// this, repeated early cancellations permanently shrink the connection window.
	s.mu.lock()
	s.cancelled = true
	// A pending h2_stream_timeout_watchdog's `done` closure checks s.ended; a
	// stream that self-cancelled here (stop_receiving_limit or an aborting
	// on_data callback) is just as "over" as one that ended normally, so mark
	// it the same way. Without this, a still-sleeping watchdog would not
	// notice for up to its full timeout, then fire on an already-finished,
	// already-deregistered stream and send a second, bogus RST_STREAM (Codex
	// P2, vlang/v#27643 pullrequestreview-4630174759).
	s.ended = true
	mut queued := u64(0)
	for ch in s.chunks {
		queued += u64(ch.len)
	}
	s.chunks.clear()
	s.mu.unlock()
	if queued > 0 {
		// A write failure here means the transport is already dead; tear it down
		// immediately so the pool stops reusing it. note_write_failure is
		// sufficient — the RST_STREAM is skipped since the peer will not receive
		// it on a dead transport.
		c.send_conn_window_update(u32(queued)) or {
			c.note_write_failure()
			return
		}
	}
	c.wmu.lock()
	c.write_all_locked(H2Frame(H2RstStreamFrame{
		stream_id:  s.id
		error_code: u32(H2ErrorCode.cancel)
	}).encode()) or {
		c.wmu.unlock()
		c.note_write_failure()
		return
	}
	c.wmu.unlock()
}

// send_conn_window_update replenishes only the connection-level receive
// window (used when the stream itself is being cancelled).
fn (mut c H2MuxConn) send_conn_window_update(n u32) ! {
	if n == 0 {
		return
	}
	// Credit our tracked window before sending the frame so the budget is
	// never in deficit for longer than necessary.
	c.recv_wmu.lock()
	c.conn_recv_window += i64(n)
	c.recv_wmu.unlock()
	buf := H2Frame(H2WindowUpdateFrame{
		stream_id:             0
		window_size_increment: n
	}).encode()
	c.wmu.lock()
	c.write_all_locked(buf) or {
		c.wmu.unlock()
		c.note_write_failure()
		return err
	}
	c.wmu.unlock()
}

// send_window_updates replenishes both the connection and stream receive
// windows after the requester consumed `n` body bytes.
fn (mut c H2MuxConn) send_window_updates(stream_id u32, n u32) ! {
	if n == 0 {
		return
	}
	// Credit both tracked receive windows before sending the frames.
	c.recv_wmu.lock()
	c.conn_recv_window += i64(n)
	c.recv_wmu.unlock()
	mut stream_alive := false
	c.smu.lock()
	if mut s := c.streams[stream_id] {
		s.mu.lock()
		s.recv_window += i64(n)
		s.mu.unlock()
		stream_alive = true
	}
	c.smu.unlock()
	mut buf := H2Frame(H2WindowUpdateFrame{
		stream_id:             0
		window_size_increment: n
	}).encode()
	if stream_alive {
		buf << H2Frame(H2WindowUpdateFrame{
			stream_id:             stream_id
			window_size_increment: n
		}).encode()
	}
	c.wmu.lock()
	c.write_all_locked(buf) or {
		c.wmu.unlock()
		c.note_write_failure()
		return err
	}
	c.wmu.unlock()
}

// write_all_locked writes all of `data` to the transport. Callers hold wmu.
fn (mut c H2MuxConn) write_all_locked(data []u8) ! {
	mut sent := 0
	for sent < data.len {
		n := c.transport.write(data[sent..])!
		if n <= 0 {
			return error('transport write returned ${n}')
		}
		sent += n
	}
}

// note_write_failure handles a transport write failure: it stops admitting new
// requests and tears the transport down. Closing it interrupts the reader's
// blocking read so it runs fail_conn and fails every other in-flight stream,
// instead of leaving them hung when the transport's read side does not also
// break (a write-only failure). teardown_transport is once-guarded, so this
// never double-closes against the reader's own teardown.
fn (mut c H2MuxConn) note_write_failure() {
	c.smu.lock()
	c.shutting_down = true
	c.smu.unlock()
	// Interrupt the reader's blocking read if a closer can, then fail the
	// connection directly. fail_conn is idempotent (guarded by c.closed), so a
	// later reader invocation is a safe no-op. Calling it unconditionally — not
	// only when close_transport is nil — wakes every in-flight stream
	// immediately, closing both the window before the reader notices the dead
	// transport and the case of a closer that cannot interrupt a blocking read.
	c.teardown_transport()
	c.fail_conn('transport write failure')
}

// --- reader side -------------------------------------------------------------

// read_loop runs on the connection's background thread: it is the only reader
// of the transport and demuxes every incoming frame.
fn (mut c H2MuxConn) read_loop() {
	for {
		frame := c.mux_read_frame() or {
			if is_transport_timeout_error(err) {
				if c.reader_should_exit() {
					c.fail_conn('connection retired while idle')
					return
				}
				continue
			}
			c.fail_conn('connection lost: ${err.msg()}')
			return
		}
		c.dispatch_frame(frame) or {
			c.fail_conn(err.msg())
			return
		}
	}
}

// reader_should_exit lets an idle reader retire the connection on shutdown.
fn (mut c H2MuxConn) reader_should_exit() bool {
	c.smu.lock()
	defer {
		c.smu.unlock()
	}
	return c.shutting_down && c.active_streams == 0
}

// is_transport_timeout_error recognizes read-timeout errors, which wake the
// reader for shutdown checks rather than killing the connection.
fn is_transport_timeout_error(err IError) bool {
	msg := err.msg().to_lower()
	return msg.contains('timed out') || msg.contains('timeout')
}

// mux_read_frame reads and decodes one frame from the transport, enforcing
// the receive limit we advertised to the peer in our own SETTINGS. This is
// h2_default_max_frame_size, which H2MuxConn always sends and never renegotiates.
// (c.peer_max_frame is the peer's receive limit — our outbound cap — and must
// not be used here.)
fn (mut c H2MuxConn) mux_read_frame() !H2Frame {
	c.mux_fill_at_least(h2_frame_header_len)!
	header := h2_parse_frame_header(c.rbuf)!
	if header.length > h2_default_max_frame_size {
		return error('frame larger than SETTINGS_MAX_FRAME_SIZE (${header.length})')
	}
	total := h2_frame_header_len + int(header.length)
	c.mux_fill_at_least(total)!
	frame := h2_parse_frame(header, c.rbuf[h2_frame_header_len..total])!
	c.rbuf = c.rbuf[total..].clone()
	return frame
}

// mux_fill_at_least reads from the transport until rbuf holds n bytes.
fn (mut c H2MuxConn) mux_fill_at_least(n int) ! {
	for c.rbuf.len < n {
		mut tmp := []u8{len: h2_conn_read_chunk}
		got := c.transport.read(mut tmp)!
		if got <= 0 {
			return error('connection closed by peer')
		}
		c.rbuf << tmp[..got]
	}
}

fn (mut c H2MuxConn) dispatch_frame(frame H2Frame) ! {
	match frame {
		H2SettingsFrame {
			if !frame.ack {
				c.apply_peer_settings(frame.settings)!
				c.wmu.lock()
				if !c.handshaked {
					// RFC 7540 §3.5: no frame may precede the client connection
					// preface. Defer the ACK; handshake_locked() will flush it
					// immediately after sending the preface. Use a counter so
					// multiple SETTINGS frames each get their own ACK.
					c.pending_settings_acks++
					over := c.pending_settings_acks + c.pending_ping_acks.len > h2_max_pending_preface_acks
					c.wmu.unlock()
					if over {
						return error('h2: too many control frames before the client preface')
					}
				} else {
					c.write_all_locked(H2Frame(H2SettingsFrame{
						ack: true
					}).encode()) or {
						c.wmu.unlock()
						return error('failed to ack SETTINGS: ${err.msg()}')
					}
					c.wmu.unlock()
				}
			}
		}
		H2PingFrame {
			if !frame.ack {
				c.wmu.lock()
				if !c.handshaked {
					// RFC 7540 §3.5: client preface must be the first bytes sent.
					// Defer the ACK; handshake_locked() will flush it after the preface.
					c.pending_ping_acks << frame.data
					over := c.pending_settings_acks + c.pending_ping_acks.len > h2_max_pending_preface_acks
					c.wmu.unlock()
					if over {
						return error('h2: too many control frames before the client preface')
					}
				} else {
					c.write_all_locked(H2Frame(H2PingFrame{
						ack:  true
						data: frame.data
					}).encode()) or {
						c.wmu.unlock()
						return error('failed to ack PING: ${err.msg()}')
					}
					c.wmu.unlock()
				}
			}
		}
		H2WindowUpdateFrame {
			inc := frame.window_size_increment
			if frame.stream_id == 0 {
				// RFC 7540 6.9: a 0 increment is a connection PROTOCOL_ERROR;
				// a window past 2^31-1 is a connection FLOW_CONTROL_ERROR.
				if inc == 0 {
					return error('h2: connection WINDOW_UPDATE with a zero increment')
				}
				c.fmu.lock()
				new_window := c.send_window + i64(inc)
				overflow := new_window > i64(0x7fff_ffff)
				if !overflow {
					c.send_window = new_window
					c.fcv.broadcast()
				}
				c.fmu.unlock()
				if overflow {
					return error('h2: connection flow-control window exceeded 2^31-1')
				}
			} else {
				// Stream-level versions of the same rules are stream errors:
				// RST_STREAM the offending stream instead of killing the conn.
				if inc == 0 {
					c.reset_stream(frame.stream_id, .protocol_error,
						'WINDOW_UPDATE with a zero increment')
					return
				}
				// Hold smu across the lookup and the credit (smu -> fmu), so
				// this serializes with stream registration and the credit can
				// never be overwritten by the initial-window assignment.
				c.smu.lock()
				mut sref := c.streams[frame.stream_id] or { &H2MuxStream(unsafe { nil }) }
				mut overflow := false
				if sref != unsafe { nil } {
					c.fmu.lock()
					new_window := sref.send_window + i64(inc)
					overflow = new_window > i64(0x7fff_ffff)
					if !overflow {
						sref.send_window = new_window
						c.fcv.broadcast()
					}
					c.fmu.unlock()
				}
				c.smu.unlock()
				if overflow {
					c.reset_stream(frame.stream_id, .flow_control_error,
						'stream flow-control window exceeded 2^31-1')
				}
			}
		}
		H2GoawayFrame {
			// Take wmu before smu (the permitted wmu -> smu nesting) so setting
			// c.goaway serializes with do_on_stream's terminal-flag recheck, which
			// runs under smu while holding wmu. Without this, a GOAWAY landing
			// between that recheck (smu released at the end of the registration
			// section) and the HEADERS write (still under wmu) lets the client
			// open one more stream after observing GOAWAY (RFC 7540 6.8). Holding
			// wmu here means c.goaway cannot be set while any writer is mid-section,
			// so the recheck is authoritative.
			c.wmu.lock()
			c.smu.lock()
			c.goaway = true
			c.goaway_last = frame.last_stream_id
			mut above := []&H2MuxStream{}
			for id, st in c.streams {
				if id > frame.last_stream_id {
					above << st
				}
			}
			c.smu.unlock()
			c.wmu.unlock()
			for mut st in above {
				// Streams above last_stream_id were not processed by the
				// server, so they are safe to retry elsewhere (RFC 7540 6.8).
				st.fail('request not processed (GOAWAY)', true)
				c.wake_send(mut st)
			}
			if frame.error_code != u32(H2ErrorCode.no_error) {
				return error('connection error (GOAWAY ${h2_error_code_name(frame.error_code)})')
			}
		}
		H2HeadersFrame {
			c.on_response_headers(frame)!
		}
		H2DataFrame {
			c.on_response_data(frame)!
		}
		H2RstStreamFrame {
			mut s := c.lookup_stream(frame.stream_id)
			if s != unsafe { nil } {
				// REFUSED_STREAM means the server did not process the request
				// (RFC 7540 8.1.4), so it is safe to replay on a fresh connection
				// even for a non-idempotent method; any other reset code is not.
				retryable := frame.error_code == u32(H2ErrorCode.refused_stream)
				s.fail('stream reset by peer (${h2_error_code_name(frame.error_code)})', retryable)
				c.wake_send(mut s)
			}
		}
		H2ContinuationFrame {
			// CONTINUATION outside a header block is a connection error; the
			// in-block ones are consumed by on_response_headers.
			return error('unexpected CONTINUATION frame')
		}
		H2PushPromiseFrame {
			// We advertise SETTINGS_ENABLE_PUSH=0, so any PUSH_PROMISE is a
			// connection error (RFC 7540 6.6 / 8.2). Failing the connection
			// here also avoids the dropped fragment desyncing our HPACK decoder.
			return error('h2: unexpected PUSH_PROMISE (server push is disabled)')
		}
		else {
			// PRIORITY / unknown frame types: ignored per RFC 7540.
		}
	}
}

// apply_peer_settings folds the peer's SETTINGS into the connection,
// including the retroactive initial-window delta for every open stream
// (RFC 7540 6.9.2). Out-of-range values that would corrupt our framing or
// flow-control math are rejected as a connection error (the caller turns the
// error into fail_conn), per RFC 7540 6.5.2 / 6.5.3.
fn (mut c H2MuxConn) apply_peer_settings(settings []H2Setting) ! {
	for st in settings {
		match st.id {
			h2_settings_header_table_size {
				// RFC 7541 §6.3: even if our encoder uses only literals, we MUST
				// emit a Dynamic Table Size Update prefix at the start of the next
				// HEADERS block when the peer lowers this limit. encode() emits
				// the update when pending_max_table_size >= 0.
				c.wmu.lock()
				c.encoder.dyn_table.set_max_size(int(st.value))
				c.encoder.pending_max_table_size = int(st.value)
				c.wmu.unlock()
			}
			h2_settings_enable_push {
				if st.value > 1 {
					// RFC 7540 6.5.2: ENABLE_PUSH must be 0 or 1.
					return error('h2: peer SETTINGS_ENABLE_PUSH ${st.value} is not 0 or 1')
				}
				// We never use server push, so the value is otherwise irrelevant.
			}
			h2_settings_max_concurrent_streams {
				c.smu.lock()
				c.peer_max_streams = st.value
				c.smu.unlock()
			}
			h2_settings_max_header_list_size {
				// RFC 9113 §6.5.2: advisory cap on the size of the header list we
				// send. Store it (under wmu, with the other send-side header state)
				// so do_on_stream can refuse an over-limit request locally instead
				// of having the server reject it after the round trip.
				c.wmu.lock()
				c.peer_max_header_list_size = st.value
				c.wmu.unlock()
			}
			h2_settings_initial_window_size {
				if st.value > u32(0x7fff_ffff) {
					// RFC 7540 6.5.3: values above 2^31-1 are a FLOW_CONTROL_ERROR;
					// they also overflow our i64 send-window arithmetic.
					return error('h2: peer SETTINGS_INITIAL_WINDOW_SIZE ${st.value} exceeds 2^31-1')
				}
				// smu is held across the fmu section so the snapshot of open
				// streams and the delta application are atomic with respect to
				// stream registration (which nests the same way) — a stream can
				// neither miss the delta nor receive it twice.
				c.smu.lock()
				c.fmu.lock()
				delta := i64(st.value) - c.peer_initial_window
				c.peer_initial_window = i64(st.value)
				// RFC 7540 §6.9.2: validate ALL stream windows before applying any
				// delta so that an overflow on stream N does not leave streams 1..N-1
				// in a partially updated state.  Pre-validation also ensures the
				// broadcast below is always reached for a positive delta.
				for _, s in c.streams {
					if s.send_window + delta > i64(0x7fff_ffff) {
						c.fmu.unlock()
						c.smu.unlock()
						return error('h2: SETTINGS_INITIAL_WINDOW_SIZE delta overflows stream ${s.id} send window (RFC 7540 §6.9.2 FLOW_CONTROL_ERROR)')
					}
				}
				for _, mut s in c.streams {
					s.send_window += delta
				}
				if delta > 0 {
					c.fcv.broadcast()
				}
				c.fmu.unlock()
				c.smu.unlock()
			}
			h2_settings_max_frame_size {
				if st.value < h2_default_max_frame_size || st.value > u32(0x00ff_ffff) {
					// RFC 7540 6.5.2: valid range is 2^14..2^24-1. A value such as
					// 0 would make our HEADERS/DATA chunk step 0 and hang the send
					// path in a zero-length-frame loop, so fail the connection.
					return error('h2: peer SETTINGS_MAX_FRAME_SIZE ${st.value} out of range [16384, 16777215]')
				}
				// Take wmu (then fmu, the permitted wmu -> fmu nesting) before
				// lowering the cap. The send paths re-read peer_max_frame under fmu
				// and then write the frame under wmu, holding wmu across both; an
				// fmu-only update could land in the fmu-release -> write gap and let
				// a sender emit a frame larger than the peer's new limit
				// (FRAME_SIZE_ERROR). Serializing on wmu makes the senders' re-read
				// authoritative, mirroring the header_table_size arm above.
				// apply_peer_settings holds no connection lock at entry, so this
				// wmu acquisition cannot invert any held lock.
				c.wmu.lock()
				c.fmu.lock()
				c.peer_max_frame = st.value
				c.fmu.unlock()
				c.wmu.unlock()
			}
			else {} // unknown settings are ignored (RFC 7540 6.5.2)
		}
	}
}

// on_response_headers assembles a complete header block (reading any
// CONTINUATION frames inline — the reader owns the read path), decodes it,
// and delivers it to the stream. Blocks for unknown streams are still decoded
// to keep the connection's HPACK dynamic table in sync, then dropped.
fn (mut c H2MuxConn) on_response_headers(frame H2HeadersFrame) ! {
	mut fragment := frame.fragment.clone()
	if !frame.end_headers {
		for {
			cont := c.mux_read_frame() or {
				return error('connection lost inside a header block: ${err.msg()}')
			}
			if cont is H2ContinuationFrame {
				if cont.stream_id != frame.stream_id {
					return error('CONTINUATION on the wrong stream')
				}
				fragment << cont.fragment
				if fragment.len > h2_max_recv_header_block {
					return error('h2: response header block exceeds ${h2_max_recv_header_block} bytes')
				}
				if cont.end_headers {
					break
				}
			} else {
				return error('expected a CONTINUATION frame')
			}
		}
	}
	fields := c.decoder.decode(fragment)!
	mut s := c.lookup_stream(frame.stream_id)
	if s == unsafe { nil } {
		return c.check_unknown_stream(frame.stream_id)
	}
	s.mu.lock()
	// RFC 9113 §5.1: a HEADERS frame after the stream has already ended (a prior
	// END_STREAM, or RST/connection death that set s.ended) is a frame on a closed
	// stream, not more trailers. Without this, back-to-back trailer + extra HEADERS
	// in one read buffer would be appended to resp.headers before wait_response
	// flushes, delivering fields the peer sent after closing the stream. Reset
	// instead (the DATA path makes the same check via s.ended).
	if s.ended {
		s.mu.unlock()
		c.reset_stream(frame.stream_id, .stream_closed,
			'HEADERS frame after stream end on stream ${frame.stream_id}')
		return
	}
	was_headers_done := s.headers_done
	if !s.headers_done {
		mut status := 0
		mut status_valid := false
		for f in fields {
			if f.name == ':status' {
				// RFC 9113 §8.3.1: :status is exactly three digits. string.int()
				// is lenient ('200 OK' -> 200), so validate the raw value before
				// converting; otherwise a malformed status is accepted as valid.
				if f.value.len == 3 && all_digits(f.value) {
					status = f.value.int()
					status_valid = true
				}
				break
			}
		}
		// A response MUST carry a valid :status (RFC 9113 §8.3.1), and HTTP/2
		// forbids 101 (§8.1.1). A missing, malformed, out-of-range, or 101 status
		// is a stream-level PROTOCOL_ERROR — reset it rather than treat it as a
		// 1xx interim and wait forever for a "final" HEADERS that never arrives.
		// (Trailers legitimately omit :status, but they are handled below since
		// headers_done is already set by then.)
		if !status_valid || status < 100 || status > 599 || status == 101 {
			s.mu.unlock()
			c.reset_stream(frame.stream_id, .protocol_error,
				'response with a missing or invalid :status')
			return
		}
		// RFC 9110 §15.2 / RFC 9113 §8.1: a server may send 1xx interim responses
		// (100 Continue, 103 Early Hints) before the final response. They are not
		// the final response and carry no body, so ignore them and keep waiting
		// for the final (>= 200) HEADERS rather than latching the interim status
		// and headers — which would make the real final HEADERS look like trailers.
		if status >= 200 {
			s.status = status
			mut seen_regular := false
			mut seen_status := false
			for f in fields {
				if f.name.starts_with(':') {
					// RFC 9113 §8.3: the only valid response pseudo-header is :status
					// (consumed above); pseudo-headers MUST precede regular fields and
					// MUST NOT be duplicated. Any other ':' field, :status after a
					// regular field, or a second :status makes the response malformed.
					if f.name != ':status' || seen_regular || seen_status {
						s.mu.unlock()
						c.reset_stream(frame.stream_id, .protocol_error,
							'malformed response: invalid pseudo-header ${f.name}')
						return
					}
					seen_status = true
					continue
				}
				seen_regular = true
				// RFC 9113 §8.2: reject malformed field names (uppercase, empty) and
				// connection-specific fields rather than delivering them to the caller.
				reason := h2_response_field_error(f.name)
				if reason != '' {
					s.mu.unlock()
					c.reset_stream(frame.stream_id, .protocol_error,
						'malformed response: ${reason}')
					return
				}
				// RFC 9110 §8.6 / RFC 9113 §8.1.1: a malformed Content-Length makes
				// the message malformed (a stream-level PROTOCOL_ERROR). u64() is
				// lenient ('12junk' -> 12, '0x10' -> 16), so validate it strictly
				// here before wait_response trusts it for the body-length check.
				if f.name == 'content-length' && !all_digits(f.value) {
					s.mu.unlock()
					c.reset_stream(frame.stream_id, .protocol_error,
						'malformed Content-Length in response')
					return
				}
				s.resp_headers << f
			}
			s.headers_done = true
		}
	}
	// Trailers without END_STREAM violate RFC 7540 §8.1 (trailers must carry
	// END_STREAM); reset the stream rather than hanging wait_response forever.
	if was_headers_done && !frame.end_stream {
		s.mu.unlock()
		c.reset_stream(frame.stream_id, .protocol_error,
			'trailers HEADERS frame must carry END_STREAM')
		return
	}
	// A 1xx informational HEADERS with END_STREAM is also forbidden (RFC 9113 §8.1).
	if !s.headers_done && frame.end_stream {
		s.mu.unlock()
		c.reset_stream(frame.stream_id, .protocol_error, '1xx response must not carry END_STREAM')
		return
	}
	// A second HEADERS block on the stream carries trailers (RFC 9113 §8.1).
	// Preserve its non-pseudo fields (grpc-status, digest, ...) so callers on
	// the mux path keep the trailer metadata the synchronous
	// H2Conn.read_response surfaces. Pseudo-header fields are forbidden in
	// trailers (§8.1) and any malformed field name (§8.2) makes the message
	// malformed, so both reset the stream. wait_response flushes the kept fields
	// into resp.headers when the stream ends. (The trailers-without-END_STREAM
	// case was already reset above, so a trailer block here always carries
	// END_STREAM.)
	if was_headers_done {
		for f in fields {
			// RFC 9113 §8.1: trailers MUST NOT contain pseudo-header fields, and the
			// §8.2 field-name rules apply as for any header block.
			if f.name.starts_with(':') {
				s.mu.unlock()
				c.reset_stream(frame.stream_id, .protocol_error,
					'malformed trailers: pseudo-header ${f.name}')
				return
			}
			reason := h2_response_field_error(f.name)
			if reason != '' {
				s.mu.unlock()
				c.reset_stream(frame.stream_id, .protocol_error, 'malformed trailers: ${reason}')
				return
			}
			s.resp_trailers << f
		}
	}
	if frame.end_stream {
		s.ended = true
	}
	s.last_activity = time.now()
	s.cv.signal()
	s.mu.unlock()
	if frame.end_stream {
		// The server closed the stream: wake any body sender that is blocked
		// waiting for flow-control credit, so it does not hang forever when
		// the peer withholds WINDOW_UPDATEs after an early final response.
		c.wake_send(mut s)
	}
}

// on_response_data delivers a DATA payload to its stream, or — for recently
// cancelled/completed streams — keeps the connection-level flow-control
// account exact by returning the credit directly (mirroring the server's
// unknown-stream backstop).
fn (mut c H2MuxConn) on_response_data(frame H2DataFrame) ! {
	// Padding (the pad-length byte + padding) counts toward flow control
	// (RFC 7540 6.9.1) but is never delivered to the app, so credit it back
	// immediately; the data bytes are credited as the requester drains them.
	pad_overhead := frame.flow_size - frame.data.len
	// Enforce the connection-level receive window (RFC 7540 §6.9). Debit
	// first; credit paths (drain, unknown-stream, cancelled) restore it.
	// recv_wmu is always acquired solo, so no lock-order hazard here.
	c.recv_wmu.lock()
	c.conn_recv_window -= i64(frame.flow_size)
	conn_ok := c.conn_recv_window >= 0
	c.recv_wmu.unlock()
	if !conn_ok {
		return error('h2: peer exceeded connection-level receive window (RFC 7540 §6.9 FLOW_CONTROL_ERROR)')
	}
	mut s := c.lookup_stream(frame.stream_id)
	if s == unsafe { nil } {
		c.check_unknown_stream(frame.stream_id)!
		// Stream is gone: credit the whole received payload to the connection.
		if frame.flow_size > 0 {
			c.send_conn_window_update(u32(frame.flow_size)) or {}
		}
		return
	}
	s.mu.lock()
	if s.cancelled {
		// The requester abandoned this stream (it has been deregistered, but the
		// reader still held a reference). Account for the whole payload at the
		// connection level and drop it, so the window does not leak. Checking
		// the flag under s.mu — the same lock cancel_stream sets it under —
		// makes this race-free against a frame in flight during cancellation.
		s.mu.unlock()
		if frame.flow_size > 0 {
			c.send_conn_window_update(u32(frame.flow_size)) or {}
		}
		return
	}
	if !s.headers_done {
		// RFC 7540 §8.1: an HTTP/2 response must begin with HEADERS. DATA before
		// the final header block is malformed — RST this stream (PROTOCOL_ERROR)
		// rather than queue bytes a requester would see with status 0. Credit the
		// frame back to the connection window (debited above, never delivered).
		s.mu.unlock()
		if frame.flow_size > 0 {
			c.send_conn_window_update(u32(frame.flow_size)) or {}
		}
		c.reset_stream(frame.stream_id, .protocol_error,
			'DATA before response HEADERS on stream ${frame.stream_id}')
		return
	}
	// RFC 7540 §5.1: a DATA frame after the peer's END_STREAM is a stream-closed
	// error. If we have also closed our send side the stream is "closed" — a
	// connection error (the spec's MUST); otherwise it is "half-closed (remote)"
	// — a STREAM error, so one peer's misbehavior does not tear down every other
	// multiplexed stream. Checked before the flow-control debit: a half-closed
	// (remote) receiver is no longer obligated to maintain the window (§6.9.1).
	if s.ended {
		send_closed := s.send_closed
		s.mu.unlock()
		if send_closed {
			return error('h2: DATA frame after END_STREAM on closed stream ${frame.stream_id} (RFC 7540 §5.1)')
		}
		if frame.flow_size > 0 {
			c.send_conn_window_update(u32(frame.flow_size)) or {}
		}
		c.reset_stream(frame.stream_id, .stream_closed,
			'DATA after END_STREAM on stream ${frame.stream_id}')
		return
	}
	// Enforce the stream-level receive window (RFC 7540 §6.9.1). Padding
	// counts against stream flow control the same as data, so debit
	// whenever flow_size > 0 regardless of whether there are data bytes.
	if frame.flow_size > 0 {
		s.recv_window -= i64(frame.flow_size)
		if s.recv_window < 0 {
			s.mu.unlock()
			// RFC 7540 §6.9.1: a stream-level flow-control violation is a STREAM
			// error — RST just this stream (like the WINDOW_UPDATE overflow path)
			// rather than failing the whole connection and every other multiplexed
			// stream. Credit this frame's bytes back to the connection window
			// (debited above, never delivered); reset_stream credits queued chunks.
			c.send_conn_window_update(u32(frame.flow_size)) or {}
			c.reset_stream(frame.stream_id, .flow_control_error,
				'peer exceeded stream ${frame.stream_id} receive window')
			return
		}
	}
	if frame.data.len > 0 {
		s.chunks << frame.data.clone()
		s.body_rcvd += u64(frame.data.len)
	}
	if frame.end_stream {
		s.ended = true
	}
	s.last_activity = time.now()
	s.cv.signal()
	s.mu.unlock()
	if pad_overhead > 0 {
		c.send_window_updates(s.id, u32(pad_overhead)) or {}
	}
	if frame.end_stream {
		// The response ended on this DATA frame (an early final response with a
		// body, e.g. a 413/auth rejection). Wake any body sender still blocked on
		// flow-control credit, mirroring on_response_headers; otherwise it hangs
		// when the peer withholds further WINDOW_UPDATEs.
		c.wake_send(mut s)
	}
}

// check_unknown_stream distinguishes late frames for cancelled/finished
// streams (fine) from protocol garbage (connection error).
fn (mut c H2MuxConn) check_unknown_stream(stream_id u32) ! {
	c.wmu.lock()
	known_range := stream_id < c.next_stream_id
	c.wmu.unlock()
	if stream_id % 2 == 1 && known_range {
		return
	}
	return error('frame for an unknown stream ${stream_id}')
}

// wake_send marks a stream's send side dead and wakes a body sender that is
// blocked waiting for flow-control credit, so a RST_STREAM or GOAWAY terminates
// the upload instead of hanging it. The caller must hold no connection lock
// (it takes fmu). fail() must already have recorded the terminal error.
fn (mut c H2MuxConn) wake_send(mut s H2MuxStream) {
	c.fmu.lock()
	s.send_dead = true
	c.fcv.broadcast()
	c.fmu.unlock()
}

// reset_stream deregisters a stream, sends RST_STREAM(code), and fails its
// requester — used by the reader for a stream-level protocol/flow-control error.
// Deregistering before the RST lets any in-flight late DATA hit the
// unknown-stream backstop (which credits the connection window).
fn (mut c H2MuxConn) reset_stream(stream_id u32, code H2ErrorCode, reason string) {
	c.smu.lock()
	mut s := c.streams[stream_id] or { &H2MuxStream(unsafe { nil }) }
	c.streams.delete(stream_id)
	c.smu.unlock()
	c.wmu.lock()
	c.write_all_locked(H2Frame(H2RstStreamFrame{
		stream_id:  stream_id
		error_code: u32(code)
	}).encode()) or {
		c.wmu.unlock()
		c.note_write_failure()
		// The stream was removed from c.streams above, so fail_conn() will not
		// find it. Wake it directly so the requester does not block forever.
		if s != unsafe { nil } {
			s.fail('h2: transport write failure', false)
			c.wake_send(mut s)
		}
		return
	}
	c.wmu.unlock()
	if s != unsafe { nil } {
		// Credit back any DATA bytes already queued but never drained,
		// mirroring cancel_stream, so the connection receive window stays
		// accurate after a per-stream reset.
		s.mu.lock()
		mut queued := u64(0)
		for ch in s.chunks {
			queued += u64(ch.len)
		}
		s.chunks.clear()
		s.mu.unlock()
		if queued > 0 {
			c.send_conn_window_update(u32(queued)) or {}
		}
		s.fail('h2: ${reason}', false)
		c.wake_send(mut s)
	}
}

fn (mut c H2MuxConn) lookup_stream(stream_id u32) &H2MuxStream {
	c.smu.lock()
	defer {
		c.smu.unlock()
	}
	if s := c.streams[stream_id] {
		return s
	}
	return &H2MuxStream(unsafe { nil })
}

// fail_conn marks the connection dead, fails every in-flight stream, and wakes
// all blocked senders. Normally called by the reader on transport error; also
// called by note_write_failure when close_transport is nil and cannot interrupt
// the reader. Guards against double-invocation via the c.closed check under smu.
// Teardown is routed through teardown_transport, which runs close_transport
// exactly once, so this path and drop_ref's last-reference teardown can never
// double-close.
fn (mut c H2MuxConn) fail_conn(msg string) {
	c.smu.lock()
	if c.closed {
		c.smu.unlock()
		return
	}
	c.closed = true
	c.conn_err = msg
	mut open := []&H2MuxStream{}
	for _, s in c.streams {
		open << s
	}
	c.streams.clear()
	pending_teardown := c.refs <= 0
	c.smu.unlock()
	for mut s in open {
		s.mu.lock()
		retryable := !s.sent_headers
		s.mu.unlock()
		s.fail(msg, retryable)
	}
	c.fmu.lock()
	c.fmu_dead = true
	c.fcv.broadcast()
	c.fmu.unlock()
	if pending_teardown {
		c.teardown_transport()
	}
}

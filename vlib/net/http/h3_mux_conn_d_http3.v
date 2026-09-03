// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.quic
import sync
import time

// This file is Phase 12c of the HTTP/3 (QUIC) implementation
// (vlang/v#27675): the first UDP socket code and the first
// background-thread-drives-a-poll()-shaped-state-machine code in this
// repo. H2MuxConn (h2_mux_conn.v) is NOT a template to adapt here: its
// whole concurrency model assumes a blocking-read()/write() H2Transport,
// with request threads and one reader thread all touching the connection
// concurrently, coordinated by a wmu/fmu/smu lock split. quic.H3Conn is
// neither blocking-transport-shaped nor safe for concurrent access at
// all -- exactly one thread may ever call qc.poll()/process_timeouts()/
// open_stream()/write_stream()/read_stream() (or the h3 wrappers around
// them). That thread is THIS file's driver_loop, spawned once by
// new_h3_mux_conn and never touched by any other thread for the
// connection's whole lifetime.
//
// Request threads (do()) therefore cannot open their own QUIC stream and
// send on it synchronously the way do_on_stream does for H2 -- they queue
// a PendingH3Request and block on their own H3MuxStream's condition
// variable, exactly like H2's wait_response does for the RESPONSE half.
// The driver thread picks up queued requests once per loop iteration
// (start_request), opens the real stream, sends headers/body, and
// registers the mapping so later H3Events can find their way back to the
// right H3MuxStream. This means H3MuxConn needs only ONE lock (qmu) where
// H2MuxConn needs three: there is only ever one thread on the h3/qc/udp
// side to guard against, not a reader racing independent writers.

// h3_driver_poll_interval bounds how long the driver thread's UDP read
// waits before it re-checks queued requests, a possibly-elapsed h3 timer
// deadline, and pending shutdown -- the same role h2_pooled_poll_interval
// plays for H2's reader. $d()-overridable without a recompile so it can be
// tuned later without a design change, since the right value for this is
// currently an unmeasured guess: too long regresses first-byte latency
// for reasons unrelated to QUIC itself, too short burns CPU on an idle
// connection (documented risk in the approved Phase 12 plan).
const h3_driver_poll_interval_ms = $d('h3_driver_poll_interval_ms', 50)
const h3_driver_poll_interval = i64(h3_driver_poll_interval_ms) * time.millisecond

// h3_err_retryable_code tags a failure where the request provably never
// reached the server (the connection died, or the local stream/send setup
// failed, before any bytes could have left this endpoint) -- safe to retry
// on a fresh connection. Mirrors h2_err_retryable_code (h2_mux_conn.v)
// exactly, kept as its own distinct constant rather than reused: the two
// protocols' retry loops (h2_round_trip / the future h3_round_trip, 12d)
// are independent call sites and must never be confused by sharing a code.
pub const h3_err_retryable_code = -20014

// h3_retryable_error builds an error carrying h3_err_retryable_code.
fn h3_retryable_error(reason string) IError {
	return error_with_code('h3: ${reason}', h3_err_retryable_code)
}

// H3UdpTransport is the minimal shape driver_loop needs from a UDP socket.
// H3MuxConn depends on this interface, not the concrete net.UdpConn, for
// the same reason h2_conn.v defines H2Transport: it is the seam a test
// substitutes an in-memory fake through, so the real threading/dispatch
// code in this file runs unmodified against deterministic fixture bytes
// instead of a real socket (h3_mux_conn_test.v). h3_udp_dial.v's
// H3UdpConnTransport is the one production adapter, wrapping a real,
// already-connected net.UdpConn.
pub interface H3UdpTransport {
mut:
	read(mut buf []u8) !int
	write(buf []u8) !int
	set_read_timeout(t time.Duration)
	close() !
}

// H3ClientRequest is one HTTP/3 request at the wire level: pseudo-headers
// are not modeled separately from ordinary fields (matching QPACK's own
// QpackFieldLine, which has no pseudo-header concept) -- start_request
// synthesizes :method/:scheme/:authority/:path itself and prepends them.
// net.http's own Request/Response conversion (h3_client.v, Phase 12d)
// builds one of these; this type is defined here, in the file that first
// needs a concrete shape to compile against, the same way H2ClientRequest
// is defined in h2_conn.v rather than in h2_client.v (which only converts
// to/from it).
pub struct H3ClientRequest {
pub:
	method    string = 'GET'
	scheme    string = 'https'
	authority string
	path      string = '/'
	headers   []quic.QpackFieldLine
	body      []u8
}

// H3ClientResponse is one HTTP/3 response at the wire level. `headers`
// excludes the `:status` pseudo-header, which is parsed out into `status`
// (mirrors H2ClientResponse's identical split).
pub struct H3ClientResponse {
pub mut:
	status  int
	headers []quic.QpackFieldLine
	body    []u8
}

// H3MuxStream is one request's response-side state, filled in by the
// driver thread (via dispatch_h3_event) and drained by that request's own
// waiting thread (wait_response) -- the H3 counterpart of H2MuxStream,
// stripped of everything H2 needed that QUIC's own per-stream/connection
// flow control already makes unnecessary here: dispatch_request_stream_
// frames (h3_conn.v) reads via qc.read_stream, which already advances
// QUIC's receive windows internally, so this layer never needs to credit
// anything back the way H2MuxStream's own recv_window/WINDOW_UPDATE
// bookkeeping does.
@[heap]
struct H3MuxStream {
mut:
	quic_stream_id ?u64
	// --- guarded by mu, signaled via cv ---
	mu            &sync.Mutex = unsafe { nil }
	cv            &sync.Cond  = unsafe { nil }
	status        int
	resp_headers  []quic.QpackFieldLine
	resp_trailers []quic.QpackFieldLine
	headers_done  bool
	chunks        [][]u8
	ended         bool
	err           string
	err_code      u64 // set only on a request_error/reset failure; 0 otherwise
	retryable     bool
	// sent_headers is set once start_request's send_request_headers call
	// succeeds -- the H3 mirror of H2MuxStream.sent_headers, used by
	// fail_conn to decide whether a stream still in flight when the
	// connection dies is safe to blind-retry.
	sent_headers bool
}

fn new_h3_mux_stream() &H3MuxStream {
	mu := sync.new_mutex()
	return &H3MuxStream{
		mu: mu
		cv: sync.new_cond(mu)
	}
}

// fail marks the stream failed and wakes its requester. A no-op if the
// stream already reached a terminal state (ended) -- the first outcome
// wins, mirroring H2MuxStream.fail.
fn (mut s H3MuxStream) fail(msg string, retryable bool) {
	s.mu.lock()
	if !s.ended {
		s.err = msg
		s.retryable = retryable
		s.ended = true
		s.cv.signal()
	}
	s.mu.unlock()
}

// PendingH3Request is one admitted-but-not-yet-opened request, queued by
// do() and drained once per driver_loop iteration by start_request. This
// queueing step is the one genuine structural novelty versus H2:
// H2MuxConn.do_on_stream can open its stream and send synchronously on the
// REQUESTER's own thread because H2Transport is safe for a writer thread
// to use concurrently with the reader thread; H3Conn is not safe for ANY
// concurrent access at all, so only the driver thread may ever call
// h3.open_request_stream()/send_request_headers().
struct PendingH3Request {
	req H3ClientRequest
mut:
	stream &H3MuxStream
}

// H3MuxConn is a multiplexed client-side HTTP/3 connection, safe for
// concurrent requests from multiple threads. See this file's own module
// doc comment for why its concurrency model is a single lock (qmu) guarding
// admission-side bookkeeping, rather than H2MuxConn's wmu/fmu/smu split:
// there is exactly one thread (driver_loop) that ever touches h3/qc/udp.
@[heap]
pub struct H3MuxConn {
mut:
	transport H3UdpTransport
	h3        &quic.H3Conn = unsafe { nil }
	// --- guarded by qmu ---
	qmu             &sync.Mutex = sync.new_mutex()
	pending         []PendingH3Request
	streams         map[u64]&H3MuxStream // keyed by the real quic stream_id, once start_request assigns one
	active_streams  int                  // admitted (pending or dispatched) requests not yet finished
	closed          bool
	goaway_received bool
	shutting_down   bool
	conn_err        string
	idle_since      time.Time
	// refs mirrors H2MuxConn.refs's shape (the pool's own +1, plus one per
	// caller between do() and release()) for structural parity and future
	// policy use, but currently drives no teardown side effect of its own,
	// unlike H2MuxConn.drop_ref: H2's reader can block in transport.read()
	// indefinitely and needs an eager interrupt on the last reference,
	// while driver_loop's UDP read is always bounded by
	// h3_driver_poll_interval and notices shutdown_when_idle() +
	// active_streams == 0 on its own within one poll interval.
	refs int = 1
	// on_retired, when non-nil, is called exactly once by fail_conn, after
	// this connection has torn itself down on its OWN initiative (idle
	// QUIC max_idle_timeout, a fatal UDP read/write/h3 error) -- not when
	// Transport retires it via shutdown_when_idle(). Lets Transport
	// (transport_h3.v) remove this connection from its own pool the
	// instant it self-terminates; without it, a dead entry sits in
	// Transport.h3_conns until some later request for the same key happens
	// to overwrite it, and in the meantime evict_oldest_idle_locked's h3
	// scan -- which only checks active_streams == 0, not can_take_new_
	// request() -- can mistake it for a genuinely idle connection and
	// "evict" it instead of an actually-idle one under a different key.
	// Mirrors H2MuxConn's mandatory close_transport callback, but
	// deliberately optional (nil tolerated, not a panic): H2's callback is
	// the ONLY way to interrupt its reader's blocking transport.read(),
	// while driver_loop's UDP read is always bounded by h3_driver_poll_
	// interval regardless of whether this is set.
	on_retired fn () = unsafe { nil }
}

// new_h3_mux_conn wraps an already-dialed `transport`/`h3` pair
// (h3_udp_dial.v) and starts the background driver thread that owns them
// from this point on. The caller must not use `transport`/`h3` directly
// afterwards. `on_retired` is called once by fail_conn if this connection
// ever tears itself down on its own initiative -- see the field's own doc
// comment; pass `unsafe { nil }` to opt out (safe for e.g. tests that only
// exercise driver_loop directly, never through Transport's pool).
pub fn new_h3_mux_conn(transport H3UdpTransport, h3 &quic.H3Conn, on_retired fn ()) &H3MuxConn {
	mut c := &H3MuxConn{
		transport:  transport
		h3:         h3
		idle_since: time.now()
		on_retired: on_retired
	}
	spawn c.driver_loop()
	return c
}

// can_take_new_request reports whether a new request may be admitted on
// this connection right now (it may still be refused later, e.g. if the
// driver's own open_request_stream hits QUIC's peer-imposed stream limit --
// that failure is per-request retryable, not connection-fatal). Mirrors
// H2MuxConn.can_take_new_request's contract exactly, for Transport's shared
// eviction/pool logic (transport.v) to treat both uniformly.
pub fn (mut c H3MuxConn) can_take_new_request() bool {
	c.qmu.lock()
	defer {
		c.qmu.unlock()
	}
	return !c.closed && !c.goaway_received && !c.shutting_down
}

// is_idle_expired reports whether this connection has had zero in-flight
// requests for longer than `idle_timeout`. Mirrors H2MuxConn.is_idle_expired
// exactly.
pub fn (mut c H3MuxConn) is_idle_expired(idle_timeout time.Duration) bool {
	c.qmu.lock()
	defer {
		c.qmu.unlock()
	}
	return c.active_streams == 0 && time.now() - c.idle_since > idle_timeout
}

// shutdown_when_idle asks the connection to retire: no new requests are
// admitted, and once no requests are in flight the driver thread tears the
// connection down on its own (see this struct's own doc comment on `refs`).
pub fn (mut c H3MuxConn) shutdown_when_idle() {
	c.qmu.lock()
	c.shutting_down = true
	c.qmu.unlock()
}

// release drops the owner's reference. See `refs`'s own doc comment: unlike
// H2MuxConn.release, this alone never triggers teardown.
pub fn (mut c H3MuxConn) release() {
	c.qmu.lock()
	c.refs--
	c.qmu.unlock()
}

// --- request side ------------------------------------------------------

// do sends one request over the connection, concurrently with other
// requests, and returns its response. Errors carrying h3_err_retryable_code
// are safe to retry on a fresh connection.
pub fn (mut c H3MuxConn) do(req H3ClientRequest) !H3ClientResponse {
	c.qmu.lock()
	if c.closed {
		reason := if c.conn_err != '' { c.conn_err } else { 'connection is closed' }
		c.qmu.unlock()
		return h3_retryable_error(reason)
	}
	if c.goaway_received || c.shutting_down {
		c.qmu.unlock()
		return h3_retryable_error('connection is shutting down')
	}
	mut s := new_h3_mux_stream()
	c.refs++
	c.active_streams++
	c.pending << PendingH3Request{
		req:    req
		stream: s
	}
	c.qmu.unlock()

	resp := c.wait_response(mut s, req) or {
		c.finish_stream(mut s)
		return err
	}
	c.finish_stream(mut s)
	return resp
}

// finish_stream deregisters `s` (if the driver ever got as far as opening
// its real stream) and drops both the active-request count and this
// request's own reference. The sole place either bookkeeping value is
// decremented, regardless of which stage the request failed at -- see
// start_request's own doc comment for why start_request's failure
// branches must NOT also touch either counter.
fn (mut c H3MuxConn) finish_stream(mut s H3MuxStream) {
	c.qmu.lock()
	s.mu.lock()
	stream_id := s.quic_stream_id
	s.mu.unlock()
	if id := stream_id {
		c.streams.delete(id)
	}
	c.active_streams--
	c.idle_since = time.now()
	c.qmu.unlock()
	c.release()
}

// wait_response blocks until `s` has a complete response or a terminal
// failure, assembling one H3ClientResponse from the headers/data/trailers
// dispatch_h3_event delivers. Deliberately minimal versus H2MuxConn's own
// wait_response: H3ClientRequest carries no on_data/stop_*_limit knobs at
// this layer (those belong to net.http's own Request, layered on top in
// 12d's do_h3) and, per this file's module doc comment, needs no
// flow-control credit-back of its own.
fn (mut c H3MuxConn) wait_response(mut s H3MuxStream, req H3ClientRequest) !H3ClientResponse {
	mut resp := H3ClientResponse{}
	mut body_expected := u64(0)
	mut has_content_length := false
	mut got_headers := false
	mut body_so_far := u64(0)
	s.mu.lock()
	for {
		if !got_headers && s.headers_done {
			mut status_seen := false
			mut seen_regular := false
			for f in s.resp_headers {
				if f.name.starts_with(':') {
					if f.name != ':status' {
						// RFC 9114 §4.3 defines no response pseudo-header
						// besides :status.
						s.mu.unlock()
						return error('h3: response contains an invalid pseudo-header "${f.name}"')
					}
					if status_seen || seen_regular {
						// RFC 9114 §4.3 permits exactly one :status
						// pseudo-header, and mirrors RFC 9113 §8.3's
						// pseudo-headers-must-precede-regular-fields rule.
						// A duplicate, or one arriving after a regular
						// field, makes the response malformed. Without
						// this check, a duplicate silently last-wins
						// instead of being rejected.
						s.mu.unlock()
						return error('h3: response contains a duplicate or out-of-order :status pseudo-header')
					}
					status_seen = true
					// RFC 9114 §4.3 mirrors RFC 9113 §8.3.1: :status is
					// exactly three digits. string.int() is lenient
					// ('200 OK' -> 200, '20000' -> 20000), so validate the
					// raw value before converting -- otherwise a malformed
					// or out-of-range status is silently delivered to the
					// caller as if it were a real response code. Mirrors
					// h2_mux_conn.v's identical :status validation.
					if f.value.len != 3 || !all_digits(f.value) {
						s.mu.unlock()
						return error('h3: response has a malformed :status pseudo-header "${f.value}"')
					}
					resp.status = f.value.int()
					continue
				}
				seen_regular = true
				resp.headers << f
				if f.name == 'content-length' && all_digits(f.value) {
					body_expected = f.value.u64()
					has_content_length = true
				}
			}
			if !status_seen {
				s.mu.unlock()
				return error('h3: response is missing the :status pseudo-header')
			}
			got_headers = true
		}
		for s.chunks.len > 0 {
			chunk := s.chunks[0]
			s.chunks.delete(0)
			body_so_far += u64(chunk.len)
			resp.body << chunk
		}
		ended := s.ended
		serr := s.err
		serr_code := s.err_code
		retryable := s.retryable
		if ended {
			for f in s.resp_trailers {
				resp.headers << f
			}
			s.mu.unlock()
			if serr != '' {
				if retryable {
					return h3_retryable_error(serr)
				}
				if serr_code != 0 {
					// serr_code can be a PEER-CONTROLLED value (a QUIC
					// RESET_STREAM application error code is a full 62-bit
					// varint, RFC 9000 §16 -- not restricted to RFC 9114
					// §8.1's small registry), unlike every OTHER caller of
					// int() on an H3ErrorCode in this codebase, which only
					// ever narrows a value THIS implementation itself
					// chose. Narrowing an untrusted u64 to `int` can
					// produce ANY 32-bit value, including one that
					// collides bit-for-bit with h3_err_retryable_code --
					// h3_round_trip treats that exact value as "never
					// reached the server, safe to blind-retry", so an
					// unchecked collision would let a malicious or merely
					// unregistered peer error code cause this client to
					// silently replay a request the server explicitly
					// rejected. Only a positive int is trusted as a real
					// error code; anything else (zero, or negative -- which
					// covers every internal sentinel this codebase uses,
					// h3_err_retryable_code included) falls back to a
					// plain, codeless error instead.
					code_int := int(serr_code)
					if code_int > 0 {
						return error_with_code('h3: ${serr}', code_int)
					}
					return error('h3: ${serr} (raw error code ${serr_code})')
				}
				return error('h3: ${serr}')
			}
			if !got_headers {
				return error('h3: stream closed without a response')
			}
			// RFC 9110 §8.6: a Content-Length must match the bytes received.
			// Skip for responses defined to carry no body -- HEAD requests and
			// 204/304 status codes.
			body_allowed := req.method != 'HEAD' && resp.status != 204 && resp.status != 304
			if has_content_length && body_allowed && body_so_far != body_expected {
				return error('h3: response body length ${body_so_far} does not match Content-Length ${body_expected}')
			}
			return resp
		}
		s.cv.wait()
	}
	return resp
}

// --- driver thread -------------------------------------------------------

// driver_loop is the SOLE thread that ever touches c.h3/c.transport for
// reading or driving protocol state (see this file's own module doc
// comment). Each
// iteration: admits newly queued requests (start_request, opening their
// real QUIC streams), reads at most one UDP datagram bounded by
// h3_driver_poll_interval (so an armed H3Conn timer or a shutdown request
// is never missed for long), drives c.h3 forward with whatever arrived (or
// a bare timeout tick), writes out every resulting outgoing datagram, and
// dispatches every resulting H3Event to the H3MuxStream it belongs to.
fn (mut c H3MuxConn) driver_loop() {
	mut buf := []u8{len: h3_datagram_buf_size}
	mut next_timeout := ?u64(none)
	for {
		c.qmu.lock()
		should_exit := c.shutting_down && c.active_streams == 0
		mut to_open := c.pending.clone()
		c.pending.clear()
		c.qmu.unlock()
		if should_exit {
			c.fail_conn('connection retired while idle')
			return
		}
		for mut p in to_open {
			c.start_request(mut p)
		}

		mut wait := h3_driver_poll_interval
		if nt := next_timeout {
			now_ms := h3_now_ms()
			mut remaining := i64(0)
			if nt > now_ms {
				remaining = i64(nt - now_ms)
			}
			candidate := remaining * time.millisecond
			if candidate < wait {
				wait = candidate
			}
		}
		if wait <= 0 {
			wait = 1 * time.millisecond
		}
		c.transport.set_read_timeout(wait)

		n := c.transport.read(mut buf) or {
			if err.code() != net.err_timed_out_code {
				c.fail_conn('h3: udp read failed: ${err.msg()}')
				return
			}
			0
		}
		now_ms := h3_now_ms()
		result := if n > 0 {
			c.h3.poll(buf[..n].clone(), now_ms) or {
				c.fail_conn('h3: ${err.msg()}')
				return
			}
		} else {
			c.h3.process_timeouts(now_ms) or {
				c.fail_conn('h3: ${err.msg()}')
				return
			}
		}
		next_timeout = result.next_timeout
		mut write_failed := false
		for dg in result.outgoing {
			c.transport.write(dg.bytes) or {
				c.fail_conn('h3: udp write failed: ${err.msg()}')
				write_failed = true
				break
			}
		}
		if write_failed {
			return
		}
		for ev in result.events {
			c.dispatch_h3_event(ev)
		}
		// dispatch_h3_event's own .connection_error case can call fail_conn
		// synchronously (a QuicEvent.connection_closed the wrapped H3Conn
		// surfaced from THIS SAME poll()/process_timeouts() call, not a
		// transport-level failure this loop would otherwise have caught) --
		// closing c.transport right here, mid-iteration. Without this check
		// the loop falls through to its next iteration and calls set_read_
		// timeout/read on an already-closed transport instead of exiting,
		// unlike every OTHER fail_conn call site in this function, which all
		// return immediately after triggering it.
		c.qmu.lock()
		is_closed := c.closed
		c.qmu.unlock()
		if is_closed {
			return
		}
	}
}

// start_request opens the real QUIC request stream for one freshly
// admitted request, sends its headers (and body, if any), and registers
// the resulting quic_stream_id so dispatch_h3_event can find this
// H3MuxStream again for every later event. Runs ONLY on the driver thread.
//
// A failure here fails only THIS stream, never the whole connection:
// nothing has reached the wire yet at the point any of these three calls
// can fail (H3Conn.open_request_stream/send_request_headers/
// send_request_data only queue -- the actual bytes drain on the NEXT
// poll() call, matching QuicConn's own contract), so every failure here is
// safe to mark retryable.
//
// Registration into c.streams happens IMMEDIATELY once open_request_stream
// succeeds, before send_request_headers/send_request_data are even
// attempted -- not only after full success. Once open_request_stream
// returns a real stream_id, h3_conn.v's own internal state (request_
// streams/request_decoders) already owns it and has no external abort/
// cancel API (h3_conn.v's own fail_request_stream doc comment); a LATER
// local failure here (send_request_headers/send_request_data) does not
// retract whatever was already queued, so the peer can still legitimately
// see this stream and respond to it. Registering late meant such a
// response's H3Events would find no entry in c.streams and be silently
// dropped forever, with nothing on either side ever cleaning the stream
// up. Registering early lets fail_conn/finish_stream find and deregister
// it like any other stream, and lets a late, unretryable-anyway response
// still land on an H3MuxStream instead of a nil lookup.
//
// Deliberately does NOT touch c.active_streams or call c.release(): finish_
// stream (invoked once, from do()'s own wrapper around wait_response) is
// the sole owner of both, for every request regardless of which stage it
// failed at -- touching them here too would double-decrement once do()'s
// wait_response wakes up on this same failure and finish_stream runs.
fn (mut c H3MuxConn) start_request(mut p PendingH3Request) {
	c.qmu.lock()
	goaway := c.goaway_received
	c.qmu.unlock()
	if goaway {
		// do()'s own admission check and this drain are separated by at
		// least one driver_loop iteration (this file's own module doc
		// comment): a request can be admitted (do() ran before GOAWAY was
		// known) and still reach here AFTER dispatch_h3_event's .goaway
		// case has already set c.goaway_received. Opening a brand-new
		// stream now would land it at/above the server's already-declared
		// processing boundary, guaranteeing it is never answered. Fail it
		// exactly like that same .goaway handler fails every stream
		// already open at that moment: retryable, so the caller redials
		// immediately instead of hanging or being marked non-retryable
		// later.
		p.stream.fail('h3: connection is shutting down (GOAWAY)', true)
		return
	}
	stream_id := c.h3.open_request_stream() or {
		p.stream.fail('h3: ${err.msg()}', true)
		return
	}
	p.stream.mu.lock()
	p.stream.quic_stream_id = stream_id
	p.stream.mu.unlock()
	c.qmu.lock()
	c.streams[stream_id] = p.stream
	c.qmu.unlock()

	mut fields := [
		quic.QpackFieldLine{
			name:  ':method'
			value: p.req.method
		},
		quic.QpackFieldLine{
			name:  ':scheme'
			value: p.req.scheme
		},
		quic.QpackFieldLine{
			name:  ':authority'
			value: p.req.authority
		},
		quic.QpackFieldLine{
			name:  ':path'
			value: p.req.path
		},
	]
	fields << p.req.headers
	has_body := p.req.body.len > 0
	c.h3.send_request_headers(stream_id, fields, !has_body) or {
		p.stream.fail('h3: failed to send request headers: ${err.msg()}', true)
		return
	}
	p.stream.mu.lock()
	p.stream.sent_headers = true
	p.stream.mu.unlock()
	if has_body {
		c.h3.send_request_data(stream_id, p.req.body, true) or {
			p.stream.fail('h3: failed to send request body: ${err.msg()}', true)
			return
		}
	}
}

// dispatch_h3_event routes one H3Event to the H3MuxStream it belongs to
// (for the per-request-stream kinds) or applies it to connection-wide
// state (settings_received/goaway/connection_error). The four per-stream
// data-carrying kinds (response_headers/trailers/data/ended) all guard on
// `!s.ended`: since start_request registers a stream into c.streams as
// soon as it is opened -- before its headers/body are necessarily fully
// sent -- a request that failed LOCALLY (headers/body send error, marking
// s.ended via s.fail()) can still have a real response arrive for the
// underlying QUIC stream the peer legitimately saw. That response is no
// longer deliverable to anyone (the requester already got its local
// error and returned), so once a stream is ended, every later data-
// carrying event for it is a deliberate no-op rather than clobbering
// fields nobody will read.
fn (mut c H3MuxConn) dispatch_h3_event(ev quic.H3Event) {
	match ev.kind {
		.settings_received {}
		.goaway {
			// RFC 9114 §5.2: once GOAWAY(id) arrives, the server
			// guarantees it will not process any request whose stream id
			// is at/above `id` -- such a request is always safe to retry
			// immediately on a fresh connection, rather than being left to
			// hang on an unbounded cv.wait() or, once the connection
			// eventually dies for an unrelated reason, get misclassified
			// as non-retryable by fail_conn's `!sent_headers` heuristic
			// (headers already having been sent proves nothing about
			// whether the SERVER acted on them once it had already
			// promised not to). Mirrors h2_mux_conn.v's H2GoawayFrame
			// handler, which does the identical proactive walk-and-fail
			// over c.streams.
			boundary := ev.goaway_id or { 0 }
			c.qmu.lock()
			c.goaway_received = true
			mut to_fail := []&H3MuxStream{}
			for id, s in c.streams {
				if id >= boundary {
					to_fail << s
				}
			}
			c.qmu.unlock()
			for mut s in to_fail {
				s.fail('request not processed (GOAWAY)', true)
			}
		}
		.connection_error {
			reason := if ev.reason != '' { ev.reason } else { 'h3 connection closed' }
			c.fail_conn(reason)
		}
		.response_headers {
			stream_id := ev.stream_id or { return }
			mut s := c.lookup_stream(stream_id)
			if s == unsafe { nil } {
				return
			}
			s.mu.lock()
			if !s.ended {
				s.resp_headers = ev.headers
				s.headers_done = true
				s.cv.signal()
			}
			s.mu.unlock()
		}
		.response_trailers {
			stream_id := ev.stream_id or { return }
			mut s := c.lookup_stream(stream_id)
			if s == unsafe { nil } {
				return
			}
			s.mu.lock()
			if !s.ended {
				s.resp_trailers = ev.headers
				s.cv.signal()
			}
			s.mu.unlock()
		}
		.response_data {
			stream_id := ev.stream_id or { return }
			mut s := c.lookup_stream(stream_id)
			if s == unsafe { nil } {
				return
			}
			s.mu.lock()
			if !s.ended {
				s.chunks << ev.data
				s.cv.signal()
			}
			s.mu.unlock()
		}
		.response_ended {
			stream_id := ev.stream_id or { return }
			mut s := c.lookup_stream(stream_id)
			if s == unsafe { nil } {
				return
			}
			s.mu.lock()
			if !s.ended {
				s.ended = true
				s.cv.signal()
			}
			s.mu.unlock()
		}
		.request_error {
			stream_id := ev.stream_id or { return }
			mut s := c.lookup_stream(stream_id)
			if s == unsafe { nil } {
				return
			}
			code := ev.error_code or { u64(0) }
			// RFC 9114 §8.1: H3_REQUEST_REJECTED means the server "has not
			// processed" this request at all -- the client MAY retry it,
			// exactly the same guarantee HTTP/2's REFUSED_STREAM carries.
			// Mirrors h2_mux_conn.v's identical
			// `frame.error_code == u32(H2ErrorCode.refused_stream)` check;
			// every other request-stream error code is left non-retryable,
			// since the server may have already acted on the request.
			retryable := code == quic.H3ErrorCode.request_rejected.code()
			s.mu.lock()
			if !s.ended {
				s.err = ev.reason
				s.err_code = code
				s.retryable = retryable
				s.ended = true
				s.cv.signal()
			}
			s.mu.unlock()
		}
	}
}

// lookup_stream returns the H3MuxStream registered for `stream_id`, or a
// nil pointer if none is (e.g. an event for a stream this connection never
// admitted, or one that already finished and was deregistered).
fn (mut c H3MuxConn) lookup_stream(stream_id u64) &H3MuxStream {
	c.qmu.lock()
	defer {
		c.qmu.unlock()
	}
	if s := c.streams[stream_id] {
		return s
	}
	return &H3MuxStream(unsafe { nil })
}

// fail_conn marks the connection dead, fails every pending AND in-flight
// stream, releases the H3 connection, and closes the transport. Called only
// from the driver thread
// (on a fatal read/write/h3 error, or idle retirement).
//
// Draining c.pending here, not just c.streams, is required: a request
// admitted by do() between one driver_loop iteration and the next has not
// yet been given a quic_stream_id by start_request, so it would never
// appear in c.streams -- without this, it would be stranded on cv.wait()
// forever the instant a fatal error lands in that exact window (the
// approved Phase 12 plan's own top-ranked risk callout for this
// sub-phase).
fn (mut c H3MuxConn) fail_conn(msg string) {
	c.qmu.lock()
	if c.closed {
		c.qmu.unlock()
		return
	}
	c.closed = true
	c.conn_err = msg
	mut open := []&H3MuxStream{}
	for _, s in c.streams {
		open << s
	}
	c.streams.clear()
	for p in c.pending {
		open << p.stream
	}
	c.pending.clear()
	c.qmu.unlock()
	if c.on_retired != unsafe { nil } {
		c.on_retired()
	}
	if c.h3 != unsafe { nil } {
		c.h3.free()
	}
	for mut s in open {
		s.mu.lock()
		retryable := !s.sent_headers
		s.mu.unlock()
		s.fail(msg, retryable)
	}
	c.transport.close() or {}
}

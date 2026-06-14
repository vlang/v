// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

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

// h2_err_retryable_code tags stream errors where the request provably never
// reached server processing (GOAWAY-unprocessed, connection closed before the
// request was sent, admission refused), so it is safe to retry on a fresh
// connection even for non-idempotent methods.
pub const h2_err_retryable_code = -20012

// h2_retryable_error builds an error carrying h2_err_retryable_code.
fn h2_retryable_error(reason string) IError {
	return error_with_code('h2: ${reason}', h2_err_retryable_code)
}

// H2MuxStream is the client-side state of one in-flight request stream.
@[heap]
struct H2MuxStream {
mut:
	id u32
	// --- response state, guarded by mu, signaled via cv ---
	mu           &sync.Mutex = unsafe { nil }
	cv           &sync.Cond  = unsafe { nil }
	status       int
	resp_headers []H2HeaderField
	headers_done bool
	chunks       [][]u8 // DATA payloads appended by the reader, drained by the requester
	body_rcvd    u64    // cumulative DATA bytes received
	ended        bool   // END_STREAM, RST, or connection death
	err          string // non-empty: the stream failed
	retryable    bool   // the failure is safe to retry on a fresh connection
	sent_headers bool   // the request HEADERS reached the transport
	// --- send state, guarded by the connection's fmu ---
	send_window i64
	send_dead   bool // RST/GOAWAY: wake a body sender blocked on flow-control credit
}

fn new_h2_mux_stream() &H2MuxStream {
	mu := sync.new_mutex()
	return &H2MuxStream{
		mu: mu
		cv: sync.new_cond(mu)
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

// send_failure_reason returns the terminal error recorded by fail(), plus its
// retryable flag, for a body sender that observed send_dead while blocked on
// flow control. The flag must be preserved so a request the server never
// processed (e.g. reset by GOAWAY above last_stream_id) can still be retried.
fn (mut s H2MuxStream) send_failure_reason() (string, bool) {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	reason := if s.err != '' {
		s.err
	} else {
		'h2: stream terminated while sending the request body'
	}
	return reason, s.retryable
}

// H2MuxConn is a multiplexed client-side HTTP/2 connection, safe for
// concurrent requests from multiple threads.
@[heap]
pub struct H2MuxConn {
mut:
	transport H2Transport
	// close_transport, when set, is called exactly once during teardown, after
	// the reader thread has exited and the last reference is dropped.
	close_transport fn () = unsafe { nil }
	// --- guarded by wmu ---
	wmu            &sync.Mutex = sync.new_mutex()
	encoder        H2HpackEncoder
	next_stream_id u32 = 1
	handshaked     bool
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
	// --- reader-thread private (no locks) ---
	decoder H2HpackDecoder
	rbuf    []u8
}

// new_h2_mux_conn creates a multiplexed connection over `transport` and starts
// its background reader. The connection preface is sent lazily with the first
// request. `close_transport` (optional) is called once at final teardown.
pub fn new_h2_mux_conn(transport H2Transport, close_transport fn ()) &H2MuxConn {
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

// finish_stream removes the stream from the connection and releases the
// request's reference (possibly triggering teardown).
fn (mut c H2MuxConn) finish_stream(mut s H2MuxStream) {
	c.smu.lock()
	c.streams.delete(s.id)
	c.active_streams--
	c.idle_since = time.now()
	c.smu.unlock()
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
		c.wmu.unlock()
		return h2_retryable_error('connection handshake failed: ${err.msg()}')
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
	if c.closed || c.goaway || c.shutting_down {
		reason := if c.conn_err != '' { c.conn_err } else { 'connection is shutting down' }
		c.smu.unlock()
		c.wmu.unlock()
		return h2_retryable_error(reason)
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
		c.send_body_on_stream(mut s, req.body)!
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
}

// send_header_block_locked writes a header block as HEADERS(+CONTINUATION)
// frames. Callers must hold wmu.
fn (mut c H2MuxConn) send_header_block_locked(stream_id u32, block []u8, end_stream bool) ! {
	c.fmu.lock()
	max := int(c.peer_max_frame)
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
	c.write_all_locked(H2Frame(H2HeadersFrame{
		stream_id:   stream_id
		fragment:    block[..max]
		end_headers: false
		end_stream:  end_stream
	}).encode())!
	mut off := max
	for off < block.len {
		mut next := off + max
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
				// The stream was reset (RST_STREAM) or repudiated (GOAWAY) while
				// we were blocked for window credit; surface its terminal error
				// rather than hang, preserving retryability so a request the
				// server never processed can be replayed on a fresh connection.
				reason, retryable := s.send_failure_reason()
				if retryable {
					return h2_retryable_error(reason)
				}
				return error(reason)
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

		next := off + chunk
		c.wmu.lock()
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
}

// wait_response drains the stream until it ends, honoring the request's
// streaming callback and stop limits. Callbacks run on the requester's thread.
fn (mut c H2MuxConn) wait_response(mut s H2MuxStream, req H2ClientRequest) !H2ClientResponse {
	mut resp := H2ClientResponse{}
	mut body_expected := u64(0)
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
				if f.name == 'content-length' {
					body_expected = f.value.u64()
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
				// block without stalling the reader's delivery.
				s.mu.unlock()
				req.on_data(chunk, body_so_far, body_expected, resp.status) or {
					// The callback aborted the request: credit the connection
					// window for what we consumed this round and RST the stream,
					// just like the stop_receiving_limit path, so the connection
					// window does not leak and the peer stops sending.
					if drained > 0 {
						c.send_conn_window_update(u32(drained)) or {}
					}
					c.cancel_stream(mut s)
					return err
				}
				s.mu.lock()
			}
			if req.stop_receiving_limit >= 0 && i64(body_so_far) >= req.stop_receiving_limit {
				cancelled = true
				break
			}
		}
		ended := s.ended
		serr := s.err
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
			s.mu.unlock()
			if serr != '' {
				if retryable {
					return h2_retryable_error(serr)
				}
				return error('h2: ${serr}')
			}
			if !got_headers {
				return error('h2: stream closed without a response')
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
	c.wmu.lock()
	c.write_all_locked(H2Frame(H2RstStreamFrame{
		stream_id:  s.id
		error_code: u32(H2ErrorCode.cancel)
	}).encode()) or {}
	c.wmu.unlock()
}

// send_conn_window_update replenishes only the connection-level receive
// window (used when the stream itself is being cancelled).
fn (mut c H2MuxConn) send_conn_window_update(n u32) ! {
	if n == 0 {
		return
	}
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
	mut buf := H2Frame(H2WindowUpdateFrame{
		stream_id:             0
		window_size_increment: n
	}).encode()
	buf << H2Frame(H2WindowUpdateFrame{
		stream_id:             stream_id
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
	c.teardown_transport()
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
// our advertised max frame size. Only the reader thread calls this; it is the
// sole user of rbuf.
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
				c.write_all_locked(H2Frame(H2SettingsFrame{
					ack: true
				}).encode()) or {
					c.wmu.unlock()
					return error('failed to ack SETTINGS: ${err.msg()}')
				}
				c.wmu.unlock()
			}
		}
		H2PingFrame {
			if !frame.ack {
				c.wmu.lock()
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
				// The peer's HEADER_TABLE_SIZE bounds the dynamic table our
				// *encoder* may use when compressing request headers (the table
				// the server uses to decode them) — it does not change the table
				// we advertised for decoding the server's responses. Our encoder
				// never uses HPACK dynamic indexing (only static entries and
				// literals), so there is nothing to constrain; applying this to
				// c.decoder would wrongly shrink our response-decode table and
				// break valid dynamic references in the server's responses.
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
				c.fmu.lock()
				c.peer_max_frame = st.value
				c.fmu.unlock()
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
	if !s.headers_done {
		for f in fields {
			if f.name == ':status' {
				s.status = f.value.int()
			} else if !f.name.starts_with(':') {
				s.resp_headers << f
			}
		}
		s.headers_done = true
	}
	// A second HEADERS block on the stream carries trailers; only its
	// END_STREAM matters for this client.
	if frame.end_stream {
		s.ended = true
	}
	s.cv.signal()
	s.mu.unlock()
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
	if frame.data.len > 0 {
		s.chunks << frame.data.clone()
		s.body_rcvd += u64(frame.data.len)
	}
	if frame.end_stream {
		s.ended = true
	}
	s.cv.signal()
	s.mu.unlock()
	if pad_overhead > 0 {
		c.send_window_updates(s.id, u32(pad_overhead)) or {}
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
	}).encode()) or {}
	c.wmu.unlock()
	if s != unsafe { nil } {
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

// fail_conn marks the connection dead, fails every in-flight stream, wakes
// all blocked senders, and lets the reader exit. Only the reader calls this, so
// `closed == true` also guarantees the reader is gone. Teardown is routed
// through teardown_transport, which runs close_transport exactly once, so this
// path and drop_ref's last-reference teardown can never double-close.
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

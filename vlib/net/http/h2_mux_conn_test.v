// Hermetic tests for the multiplexed HTTP/2 client connection (h2_mux_conn.v):
// concurrent interleaved streams, flow-control blocking, GOAWAY mid-flight,
// per-stream cancellation that must not poison the connection, CONTINUATION
// assembly, and waking every waiter when the connection dies. The peer is a
// scripted in-process HTTP/2 server over an in-memory blocking pipe.
module http

import sync
import time

// MuxPipeBuf is a one-way in-memory FIFO with blocking, socket-like reads.
struct MuxPipeBuf {
mut:
	mu     &sync.Mutex = sync.new_mutex()
	data   []u8
	closed bool
}

fn (mut p MuxPipeBuf) write(buf []u8) !int {
	p.mu.lock()
	defer {
		p.mu.unlock()
	}
	if p.closed {
		return error('pipe: write to closed pipe')
	}
	p.data << buf
	return buf.len
}

fn (mut p MuxPipeBuf) read(mut buf []u8) !int {
	for {
		p.mu.lock()
		if p.data.len > 0 {
			mut n := p.data.len
			if n > buf.len {
				n = buf.len
			}
			for i in 0 .. n {
				buf[i] = p.data[i]
			}
			p.data = p.data[n..].clone()
			p.mu.unlock()
			return n
		}
		if p.closed {
			p.mu.unlock()
			return error('eof')
		}
		p.mu.unlock()
		time.sleep(time.millisecond)
	}
	return 0
}

fn (mut p MuxPipeBuf) close() {
	p.mu.lock()
	p.closed = true
	p.mu.unlock()
}

// MuxPipeEnd is one half of a bidirectional pipe.
@[heap]
struct MuxPipeEnd {
mut:
	incoming &MuxPipeBuf
	outgoing &MuxPipeBuf
}

fn (mut p MuxPipeEnd) read(mut buf []u8) !int {
	return p.incoming.read(mut buf)!
}

fn (mut p MuxPipeEnd) write(buf []u8) !int {
	return p.outgoing.write(buf)!
}

fn (mut p MuxPipeEnd) close_both() {
	p.incoming.close()
	p.outgoing.close()
}

fn new_mux_pipe() (&MuxPipeEnd, &MuxPipeEnd) {
	mut a := &MuxPipeBuf{}
	mut b := &MuxPipeBuf{}
	client := &MuxPipeEnd{
		incoming: b
		outgoing: a
	}
	server := &MuxPipeEnd{
		incoming: a
		outgoing: b
	}
	return client, server
}

// MuxTestPeer is the scripted server side: it parses real frames off the pipe
// (with its own HPACK state) and records what it saw for the test to assert.
@[heap]
struct MuxTestPeer {
mut:
	end     &MuxPipeEnd
	rbuf    []u8
	decoder H2HpackDecoder
	encoder H2HpackEncoder
	mu      &sync.Mutex = sync.new_mutex()
	// stream id -> request :path, in arrival order
	paths               map[u32]string
	stream_ids          []u32
	failure             string
	data_total          map[u32]u64 // DATA bytes received per stream
	rst_streams         []u32
	conn_window_updates u64 // sum of WINDOW_UPDATE increments on stream 0
}

fn (mut p MuxTestPeer) fail(msg string) {
	p.mu.lock()
	if p.failure == '' {
		p.failure = msg
	}
	p.mu.unlock()
}

fn (mut p MuxTestPeer) failure_msg() string {
	p.mu.lock()
	defer {
		p.mu.unlock()
	}
	return p.failure
}

fn (mut p MuxTestPeer) read_exact(n int) ![]u8 {
	for p.rbuf.len < n {
		mut tmp := []u8{len: 4096}
		got := p.end.read(mut tmp)!
		if got <= 0 {
			return error('peer: pipe closed')
		}
		p.rbuf << tmp[..got]
	}
	out := p.rbuf[..n].clone()
	p.rbuf = p.rbuf[n..].clone()
	return out
}

fn (mut p MuxTestPeer) read_preface() ! {
	got := p.read_exact(h2_client_preface.len)!
	if got.bytestr() != h2_client_preface {
		return error('peer: bad connection preface')
	}
}

fn (mut p MuxTestPeer) next_frame() !H2Frame {
	head := p.read_exact(h2_frame_header_len)!
	header := h2_parse_frame_header(head)!
	payload := p.read_exact(int(header.length))!
	return h2_parse_frame(header, payload)!
}

fn (mut p MuxTestPeer) write_frame(f H2Frame) ! {
	p.end.write(f.encode())!
}

// pump consumes one client frame, updating the peer's records. HEADERS blocks
// are decoded (tracking :path); WINDOW_UPDATE / SETTINGS / DATA / RST are
// tallied. Returns the frame.
fn (mut p MuxTestPeer) pump() !H2Frame {
	f := p.next_frame()!
	match f {
		H2SettingsFrame {
			if !f.ack {
				p.write_frame(H2SettingsFrame{
					ack: true
				})!
			}
		}
		H2HeadersFrame {
			mut fragment := f.fragment.clone()
			if !f.end_headers {
				for {
					cont := p.next_frame()!
					if cont is H2ContinuationFrame {
						fragment << cont.fragment
						if cont.end_headers {
							break
						}
					} else {
						return error('peer: expected CONTINUATION')
					}
				}
			}
			fields := p.decoder.decode(fragment)!
			mut path := ''
			for fld in fields {
				if fld.name == ':path' {
					path = fld.value
				}
			}
			p.mu.lock()
			p.paths[f.stream_id] = path
			p.stream_ids << f.stream_id
			p.mu.unlock()
		}
		H2DataFrame {
			p.mu.lock()
			p.data_total[f.stream_id] = (p.data_total[f.stream_id] or { u64(0) }) + u64(f.data.len)
			p.mu.unlock()
		}
		H2RstStreamFrame {
			p.mu.lock()
			p.rst_streams << f.stream_id
			p.mu.unlock()
		}
		H2WindowUpdateFrame {
			if f.stream_id == 0 {
				p.mu.lock()
				p.conn_window_updates += u64(f.window_size_increment)
				p.mu.unlock()
			}
		}
		else {}
	}

	return f
}

// wait_for_headers pumps until `n` request header blocks have arrived,
// returning the stream ids in arrival order.
fn (mut p MuxTestPeer) wait_for_headers(n int) ![]u32 {
	for {
		p.mu.lock()
		if p.stream_ids.len >= n {
			ids := p.stream_ids.clone()
			p.mu.unlock()
			return ids
		}
		p.mu.unlock()
		p.pump()!
	}
	return []u32{}
}

// respond_headers sends a 200 response header block for `stream_id`.
fn (mut p MuxTestPeer) respond_headers(stream_id u32, end_stream bool) ! {
	block := p.encoder.encode([H2HeaderField{':status', '200'},
		H2HeaderField{'content-type', 'text/plain'}])
	p.write_frame(H2HeadersFrame{
		stream_id:   stream_id
		fragment:    block
		end_headers: true
		end_stream:  end_stream
	})!
}

// --- worker plumbing ----------------------------------------------------------

@[heap]
struct MuxResults {
mut:
	mu       &sync.Mutex = sync.new_mutex()
	bodies   map[string]string
	statuses map[string]int
	errs     map[string]string
	codes    map[string]int
}

fn (mut r MuxResults) set_ok(path string, resp H2ClientResponse) {
	r.mu.lock()
	r.bodies[path] = resp.body.bytestr()
	r.statuses[path] = resp.status
	r.mu.unlock()
}

fn (mut r MuxResults) set_err(path string, e IError) {
	r.mu.lock()
	r.errs[path] = e.msg()
	r.codes[path] = e.code()
	r.mu.unlock()
}

fn mux_worker(mut c H2MuxConn, req H2ClientRequest, mut out MuxResults) {
	resp := c.do(req) or {
		out.set_err(req.path, err)
		return
	}
	out.set_ok(req.path, resp)
}

// --- tests --------------------------------------------------------------------

// Three concurrent GETs on one connection; the peer interleaves their DATA
// frames. Each requester must receive exactly its own body.
fn test_mux_concurrent_interleaved_streams() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	for i in 0 .. 3 {
		workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/w${i}' }, mut out)
	}
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(3) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		for id in ids {
			peer.respond_headers(id, false) or {
				peer.fail('respond: ${err.msg()}')
				return
			}
		}
		// Interleave the bodies: one chunk per stream per round.
		for round in 0 .. 2 {
			for id in ids {
				peer.mu.lock()
				path := peer.paths[id] or { '' }
				peer.mu.unlock()
				peer.write_frame(H2DataFrame{
					stream_id:  id
					data:       '${path}#${round};'.bytes()
					end_stream: round == 1
				}) or {
					peer.fail('data: ${err.msg()}')
					return
				}
			}
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.errs.len == 0, 'worker errors: ${out.errs}'
	for i in 0 .. 3 {
		assert out.statuses['/w${i}'] == 200
		assert out.bodies['/w${i}'] == '/w${i}#0;/w${i}#1;'
	}
	cend.close_both()
}

// A large request body must block on the 65535-byte connection/stream send
// windows and resume when the peer grants WINDOW_UPDATEs.
fn test_mux_flow_control_blocks_and_resumes() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	body_len := 100000
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		method:    'POST'
		authority: 't'
		path:      '/up'
		body:      []u8{len: body_len, init: `B`}
	}, mut out)
	peer_thread := spawn fn [body_len] (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		// Drain DATA until the client exhausts the 65535-byte windows.
		for {
			peer.mu.lock()
			got := peer.data_total[id] or { u64(0) }
			peer.mu.unlock()
			if got >= u64(h2_default_initial_window) {
				break
			}
			peer.pump() or {
				peer.fail('pump: ${err.msg()}')
				return
			}
		}
		peer.mu.lock()
		at_block := peer.data_total[id] or { u64(0) }
		peer.mu.unlock()
		if at_block != u64(h2_default_initial_window) {
			peer.fail('expected the client to stop at exactly 65535 sent bytes, got ${at_block}')
			return
		}
		// Grant room on both windows; the client must finish the body.
		peer.write_frame(H2WindowUpdateFrame{
			stream_id:             0
			window_size_increment: u32(body_len)
		}) or {
			peer.fail('wu0: ${err.msg()}')
			return
		}
		peer.write_frame(H2WindowUpdateFrame{
			stream_id:             id
			window_size_increment: u32(body_len)
		}) or {
			peer.fail('wu: ${err.msg()}')
			return
		}
		for {
			peer.mu.lock()
			got := peer.data_total[id] or { u64(0) }
			peer.mu.unlock()
			if got >= u64(body_len) {
				break
			}
			peer.pump() or {
				peer.fail('pump2: ${err.msg()}')
				return
			}
		}
		peer.respond_headers(id, true) or {
			peer.fail('respond: ${err.msg()}')
			return
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.errs.len == 0, 'worker errors: ${out.errs}'
	assert out.statuses['/up'] == 200
	cend.close_both()
}

// A peer that RST_STREAMs an upload after the client has exhausted its send
// windows must wake the body sender blocked on flow-control credit, so do()
// returns the reset error instead of hanging forever.
fn test_mux_rst_wakes_blocked_body_sender() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	body_len := 100000
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		method:    'POST'
		authority: 't'
		path:      '/up'
		body:      []u8{len: body_len, init: `B`}
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		// Let the client send until both windows are exhausted; it then blocks
		// in send_body_on_stream waiting for a WINDOW_UPDATE that never comes.
		for {
			peer.mu.lock()
			got := peer.data_total[id] or { u64(0) }
			peer.mu.unlock()
			if got >= u64(h2_default_initial_window) {
				break
			}
			peer.pump() or {
				peer.fail('pump: ${err.msg()}')
				return
			}
		}
		// Reset the stream instead of granting credit.
		peer.write_frame(H2RstStreamFrame{
			stream_id:  id
			error_code: u32(H2ErrorCode.cancel)
		}) or {
			peer.fail('rst: ${err.msg()}')
			return
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	upload_err := out.errs['/up'] or { '' }
	assert upload_err != '', 'expected the reset upload to fail, not hang'
	assert upload_err.contains('reset'), 'expected a reset error, got: ${upload_err}'
	cend.close_both()
}

// An early final response that carries a body ends with END_STREAM on the DATA
// frame, not on HEADERS. A body sender blocked on flow control must still be
// woken (by on_response_data), abandon the upload with RST_STREAM, and have the
// response delivered — not hang waiting for a WINDOW_UPDATE that never comes.
fn test_mux_early_final_response_with_body_wakes_blocked_upload() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	body_len := 100000
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		method:    'POST'
		authority: 't'
		path:      '/up'
		body:      []u8{len: body_len, init: `B`}
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		// Let the client exhaust both send windows; it then blocks in
		// send_body_on_stream waiting for a WINDOW_UPDATE that never comes.
		for {
			peer.mu.lock()
			got := peer.data_total[id] or { u64(0) }
			peer.mu.unlock()
			if got >= u64(h2_default_initial_window) {
				break
			}
			peer.pump() or {
				peer.fail('pump: ${err.msg()}')
				return
			}
		}
		// Send a complete final response WITH a body, so END_STREAM lands on the
		// DATA frame rather than HEADERS. No WINDOW_UPDATE is ever granted.
		block := peer.encoder.encode([H2HeaderField{':status', '413'},
			H2HeaderField{'content-type', 'text/plain'}])
		peer.write_frame(H2HeadersFrame{
			stream_id:   id
			fragment:    block
			end_headers: true
			end_stream:  false
		}) or {
			peer.fail('resp headers: ${err.msg()}')
			return
		}
		peer.write_frame(H2DataFrame{
			stream_id:  id
			data:       'rejected'.bytes()
			end_stream: true
		}) or {
			peer.fail('resp data: ${err.msg()}')
			return
		}
		// The client must wake, abandon the upload, and close its half with
		// RST_STREAM; pump until we observe it (or the pipe closes).
		for {
			f := peer.pump() or { return }
			if f is H2RstStreamFrame {
				return
			}
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	upload_err := out.errs['/up'] or { '' }
	assert upload_err == '', 'early final response must not error the request, got: ${upload_err}'
	assert out.statuses['/up'] or { 0 } == 413, 'expected the 413 response to be delivered'
	assert out.bodies['/up'] or { '' } == 'rejected', 'expected the response body to be delivered'
	peer.mu.lock()
	saw_rst := u32(1) in peer.rst_streams
	peer.mu.unlock()
	assert saw_rst, 'client must RST_STREAM to close its abandoned upload half'
	cend.close_both()
}

// RFC 9113 §8.3.1: a response HEADERS block without a valid :status is malformed.
// The client must reset the stream (PROTOCOL_ERROR) and surface an error to the
// requester rather than treating it as a 1xx interim and waiting forever.
fn test_mux_response_missing_status_resets_stream() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		method:    'GET'
		authority: 't'
		path:      '/get'
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		// A response header block with no :status pseudo-header (malformed); the
		// stream is left open so the client cannot fall back to "stream closed".
		block := peer.encoder.encode([H2HeaderField{'content-type', 'text/plain'}])
		peer.write_frame(H2HeadersFrame{
			stream_id:   id
			fragment:    block
			end_headers: true
			end_stream:  false
		}) or {
			peer.fail('resp headers: ${err.msg()}')
			return
		}
		// The client must reset the stream; pump until we observe the RST.
		for {
			f := peer.pump() or { return }
			if f is H2RstStreamFrame {
				return
			}
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	get_err := out.errs['/get'] or { '' }
	assert get_err != '', 'expected an error for a response missing :status, not a hang'
	assert get_err.contains('status'), 'expected a :status protocol error, got: ${get_err}'
	peer.mu.lock()
	saw_rst := u32(1) in peer.rst_streams
	peer.mu.unlock()
	assert saw_rst, 'client must RST_STREAM a malformed (no :status) response'
	cend.close_both()
}

// string.int() is lenient ('200 OK' -> 200), so a non-three-digit :status must
// be rejected by an explicit digit check rather than accepted as a 200 success.
fn test_mux_response_malformed_status_resets_stream() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		method:    'GET'
		authority: 't'
		path:      '/get'
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		// A :status that int() would parse leniently to 200, but is malformed.
		block := peer.encoder.encode([H2HeaderField{':status', '200 OK'},
			H2HeaderField{'content-type', 'text/plain'}])
		peer.write_frame(H2HeadersFrame{
			stream_id:   id
			fragment:    block
			end_headers: true
			end_stream:  false
		}) or {
			peer.fail('resp headers: ${err.msg()}')
			return
		}
		for {
			f := peer.pump() or { return }
			if f is H2RstStreamFrame {
				return
			}
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	get_err := out.errs['/get'] or { '' }
	assert get_err != '', 'expected an error for a malformed :status, not a success'
	assert get_err.contains('status'), 'expected a :status protocol error, got: ${get_err}'
	delivered := out.statuses['/get'] or { -1 }
	assert delivered == -1, 'a malformed :status must not be delivered as a success (got ${delivered})'
	cend.close_both()
}

// A stream reset with REFUSED_STREAM means the server did not process the
// request, so it must surface a retryable error (RFC 7540 8.1.4) — even for a
// non-idempotent method — so the pool can replay it on a fresh connection. A
// reset with any other code (e.g. CANCEL) must stay non-retryable.
fn test_mux_refused_stream_reset_is_retryable() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		method:    'POST'
		authority: 't'
		path:      '/refused'
		body:      'x'.bytes()
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		peer.write_frame(H2RstStreamFrame{
			stream_id:  ids[0]
			error_code: u32(H2ErrorCode.refused_stream)
		}) or {
			peer.fail('rst: ${err.msg()}')
			return
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.errs['/refused'] or { '' } != '', 'expected the refused POST to fail'
	assert out.codes['/refused'] or { 0 } == h2_err_retryable_code, 'REFUSED_STREAM reset must be retryable, got code ${out.codes['/refused']}'
	cend.close_both()
}

// Padding in a DATA frame counts toward flow control (RFC 7540 6.9.1) even
// though it is never delivered to the app. The client must credit the full
// received payload (pad-length byte + data + padding) back via WINDOW_UPDATE,
// or the connection receive window leaks and eventually stalls.
fn test_mux_padded_data_credits_full_flow_control() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	data := 'hi'.bytes()
	pad := []u8{len: 100}
	payload_len := 1 + data.len + pad.len // pad-length byte + data + padding
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/p' }, mut out)
	peer_thread := spawn fn [data, pad, payload_len] (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		peer.respond_headers(ids[0], false) or {
			peer.fail('respond: ${err.msg()}')
			return
		}
		mut payload := [u8(pad.len)]
		payload << data
		payload << pad
		raw := h2_frame_bytes(h2_frame_data, h2_flag_padded | h2_flag_end_stream, ids[0], payload)
		peer.end.write(raw) or {
			peer.fail('data: ${err.msg()}')
			return
		}
		// Pump until the connection-level WINDOW_UPDATEs sum to the full padded
		// payload; with the fix this terminates (data + padding both credited).
		for {
			peer.mu.lock()
			got := peer.conn_window_updates
			peer.mu.unlock()
			if got >= u64(payload_len) {
				break
			}
			peer.pump() or {
				peer.fail('pump: ${err.msg()}')
				return
			}
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.statuses['/p'] == 200
	assert out.bodies['/p'] == 'hi'
	assert peer.conn_window_updates == u64(payload_len), 'padding must count toward connection flow control'
	cend.close_both()
}

// A received PUSH_PROMISE must fail the connection: we advertise ENABLE_PUSH=0,
// so it is a PROTOCOL_ERROR (RFC 7540 6.6 / 8.2). The peer also sends a valid
// response — with the guard the request fails; without it the push is ignored
// and the request would wrongly succeed.
fn test_mux_push_promise_fails_connection() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/pp' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		block := peer.encoder.encode([H2HeaderField{':status', '200'}])
		peer.write_frame(H2PushPromiseFrame{
			stream_id:          ids[0]
			promised_stream_id: 2
			fragment:           block
			end_headers:        true
		}) or {
			peer.fail('push: ${err.msg()}')
			return
		}
		peer.respond_headers(ids[0], true) or {}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.statuses['/pp'] or { 0 } != 200, 'a PUSH_PROMISE must fail the connection, not be ignored'
	assert out.errs['/pp'] or { '' } != ''
	cend.close_both()
}

// An invalid SETTINGS_ENABLE_PUSH value (not 0/1) must fail the connection
// (RFC 7540 6.5.2). The peer sends ENABLE_PUSH=2 then a valid response; the
// request must fail rather than succeed.
fn test_mux_invalid_enable_push_fails_connection() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/ep' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		peer.write_frame(H2SettingsFrame{
			settings: [
				H2Setting{
					id:    h2_settings_enable_push
					value: 2
				},
			]
		}) or {
			peer.fail('settings: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		peer.respond_headers(ids[0], true) or {}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.statuses['/ep'] or { 0 } != 200, 'an invalid ENABLE_PUSH must fail the connection'
	assert out.errs['/ep'] or { '' } != ''
	cend.close_both()
}

// A stream-level WINDOW_UPDATE with a zero increment is a stream error (RFC
// 7540 6.9): that one stream must be RST and failed, but the connection and
// other concurrent streams must survive.
fn test_mux_zero_window_update_resets_only_that_stream() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut wa := []thread{}
	wa << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/a' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(2) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		mut a := u32(0)
		mut b := u32(0)
		peer.mu.lock()
		for id in ids {
			if peer.paths[id] or { '' } == '/a' {
				a = id
			} else {
				b = id
			}
		}
		peer.mu.unlock()
		// Illegal zero-increment WINDOW_UPDATE on stream A: A must be reset.
		peer.write_frame(H2WindowUpdateFrame{
			stream_id:             a
			window_size_increment: 0
		}) or {
			peer.fail('wu: ${err.msg()}')
			return
		}
		// B is served to completion; the connection must remain usable.
		peer.respond_headers(b, false) or {
			peer.fail('respond b: ${err.msg()}')
			return
		}
		peer.write_frame(H2DataFrame{
			stream_id:  b
			data:       'b-ok'.bytes()
			end_stream: true
		}) or {
			peer.fail('data b: ${err.msg()}')
			return
		}
	}(mut peer)
	time.sleep(50 * time.millisecond)
	mut wb := []thread{}
	wb << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/b' }, mut out)
	wa.wait()
	wb.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.errs['/a'] or { '' } != '', 'the zero-increment stream must fail'
	assert out.statuses['/b'] or { 0 } == 200, 'the other stream and the connection must survive'
	assert out.bodies['/b'] == 'b-ok'
	cend.close_both()
}

// GOAWAY with last_stream_id between two active streams: the lower id
// completes, the higher fails with a retryable error, and new requests are
// refused (also retryable).
fn test_mux_goaway_mid_flight() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut w1 := []thread{}
	w1 << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/g0' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		// First stream arrives, then wait for the second before answering, so
		// both are unambiguously in flight when GOAWAY is sent.
		ids := peer.wait_for_headers(2) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		mut sorted := ids.clone()
		sorted.sort()
		low := sorted[0]
		high := sorted[1]
		peer.write_frame(H2GoawayFrame{
			last_stream_id: low
			error_code:     u32(H2ErrorCode.no_error)
		}) or {
			peer.fail('goaway: ${err.msg()}')
			return
		}
		// The processed stream still completes.
		peer.respond_headers(low, false) or {
			peer.fail('respond: ${err.msg()}')
			return
		}
		peer.write_frame(H2DataFrame{
			stream_id:  low
			data:       'survivor'.bytes()
			end_stream: true
		}) or {
			peer.fail('data: ${err.msg()}')
			return
		}
		_ = high
	}(mut peer)
	// Make sure the first stream is registered before starting the second, so
	// the id order is deterministic.
	time.sleep(50 * time.millisecond)
	mut w2 := []thread{}
	w2 << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/g1' }, mut out)
	w1.wait()
	w2.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.statuses['/g0'] == 200
	assert out.bodies['/g0'] == 'survivor'
	assert out.errs['/g1'] or { '' } != ''
	assert out.codes['/g1'] or { 0 } == h2_err_retryable_code
	// New requests on a GOAWAYed connection are refused as retryable.
	conn.do(H2ClientRequest{ authority: 't', path: '/late' }) or {
		assert err.code() == h2_err_retryable_code
		cend.close_both()
		return
	}
	assert false, 'a request on a GOAWAYed connection must fail'
}

// Cancelling one stream (stop_receiving_limit) must not poison the
// connection: the peer's late DATA for the cancelled stream is absorbed, and
// a second stream completes afterwards.
fn test_mux_cancel_one_stream_other_lives() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut wa := []thread{}
	wa << spawn mux_worker(mut conn, H2ClientRequest{
		authority:            't'
		path:                 '/cancelme'
		stop_receiving_limit: 5
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(2) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		mut a := u32(0)
		mut b := u32(0)
		peer.mu.lock()
		for id in ids {
			if peer.paths[id] or { '' } == '/cancelme' {
				a = id
			} else {
				b = id
			}
		}
		peer.mu.unlock()
		// Over-deliver on stream A so the client cancels it.
		peer.respond_headers(a, false) or {
			peer.fail('respond a: ${err.msg()}')
			return
		}
		peer.write_frame(H2DataFrame{
			stream_id: a
			data:      'aaaaaaaaaa'.bytes()
		}) or {
			peer.fail('data a: ${err.msg()}')
			return
		}
		// Wait for the client's RST_STREAM(A).
		for {
			peer.mu.lock()
			rst := a in peer.rst_streams
			peer.mu.unlock()
			if rst {
				break
			}
			peer.pump() or {
				peer.fail('pump: ${err.msg()}')
				return
			}
		}
		// Late DATA for the cancelled stream: the client must absorb it and
		// keep the connection healthy.
		peer.write_frame(H2DataFrame{
			stream_id: a
			data:      'late-data!'.bytes()
		}) or {
			peer.fail('late data: ${err.msg()}')
			return
		}
		// Now serve stream B to completion.
		peer.respond_headers(b, false) or {
			peer.fail('respond b: ${err.msg()}')
			return
		}
		peer.write_frame(H2DataFrame{
			stream_id:  b
			data:       'b-survives'.bytes()
			end_stream: true
		}) or {
			peer.fail('data b: ${err.msg()}')
			return
		}
	}(mut peer)
	time.sleep(50 * time.millisecond)
	mut wb := []thread{}
	wb << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/lives' }, mut out)
	wa.wait()
	wb.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	// The cancelled stream returns its truncated body without an error.
	assert out.errs['/cancelme'] or { '' } == ''
	assert out.bodies['/cancelme'] == 'aaaaaaaaaa'
	// The other stream is unaffected — the connection was not poisoned. (This
	// exercises the deregister-before-RST ordering in cancel_stream: the late
	// DATA for the cancelled stream is absorbed by the unknown-stream backstop,
	// which credits the connection window so other streams keep flowing.)
	assert out.errs['/lives'] or { '' } == ''
	assert out.bodies['/lives'] == 'b-survives'
	cend.close_both()
}

// Cancelling a stream (stop_receiving_limit) must credit the connection window
// for ALL DATA received on it — including chunks queued before the cancel that
// are discarded undrained — or the connection window leaks on every early
// cancellation. The peer sends three 10-byte chunks; whatever the client does
// not deliver it must still credit back, so the connection WINDOW_UPDATEs sum
// to the full 30 bytes sent.
fn test_mux_cancel_credits_all_received_data() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	total := 30
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		authority:            't'
		path:                 '/c'
		stop_receiving_limit: 5
	}, mut out)
	peer_thread := spawn fn [total] (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		peer.respond_headers(ids[0], false) or {
			peer.fail('respond: ${err.msg()}')
			return
		}
		for _ in 0 .. 3 {
			peer.write_frame(H2DataFrame{
				stream_id: ids[0]
				data:      'xxxxxxxxxx'.bytes()
			}) or {
				peer.fail('data: ${err.msg()}')
				return
			}
		}
		// Pump until every sent DATA byte has been credited back on the
		// connection window; with the fix this terminates regardless of how the
		// chunks raced against the cancel (drained, queued-then-discarded, or
		// late via the backstop).
		for {
			peer.mu.lock()
			got := peer.conn_window_updates
			peer.mu.unlock()
			if got >= u64(total) {
				break
			}
			peer.pump() or {
				peer.fail('pump: ${err.msg()}')
				return
			}
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert peer.conn_window_updates == u64(total), 'cancel must credit every received DATA byte'
	cend.close_both()
}

// A peer SETTINGS_HEADER_TABLE_SIZE only bounds the dynamic table our encoder
// may use for request headers; it must not shrink the table we advertised for
// decoding the server's responses. Applying it to the response decoder would
// break valid dynamic references in later responses, so the decoder limit must
// stay at its advertised default.
fn test_mux_peer_header_table_size_keeps_decoder_limit() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/h' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		// The peer limits the table it uses to DECODE our request headers to 0.
		peer.write_frame(H2SettingsFrame{
			settings: [
				H2Setting{
					id:    h2_settings_header_table_size
					value: 0
				},
			]
		}) or {
			peer.fail('settings: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		peer.respond_headers(id, false) or {
			peer.fail('respond: ${err.msg()}')
			return
		}
		peer.write_frame(H2DataFrame{
			stream_id:  id
			data:       'ok'.bytes()
			end_stream: true
		}) or {
			peer.fail('data: ${err.msg()}')
			return
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.statuses['/h'] == 200
	// The response round-trip guarantees the reader processed the peer SETTINGS
	// first; our response decoder must still be at the advertised default.
	assert conn.decoder.max_dynamic_size == h2_hpack_default_table_size
	cend.close_both()
}

// A POST whose upload is flow-control blocked when GOAWAY repudiates its stream
// (id above last_stream_id) must surface a retryable error, not a plain one, so
// the pool can replay a request the server never processed.
fn test_mux_goaway_preserves_retryable_for_blocked_upload() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	body_len := 100000
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		method:    'POST'
		authority: 't'
		path:      '/up'
		body:      []u8{len: body_len, init: `B`}
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		// Let the upload exhaust both windows so the sender blocks in
		// send_body_on_stream waiting for a WINDOW_UPDATE.
		for {
			peer.mu.lock()
			got := peer.data_total[id] or { u64(0) }
			peer.mu.unlock()
			if got >= u64(h2_default_initial_window) {
				break
			}
			peer.pump() or {
				peer.fail('pump: ${err.msg()}')
				return
			}
		}
		// GOAWAY repudiating the in-flight stream (last_stream_id 0 < id), with
		// no connection error: the stream is retryable and the blocked sender
		// must wake with that classification preserved.
		peer.write_frame(H2GoawayFrame{
			last_stream_id: 0
			error_code:     u32(H2ErrorCode.no_error)
		}) or {
			peer.fail('goaway: ${err.msg()}')
			return
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.errs['/up'] or { '' } != '', 'expected the repudiated upload to fail, not hang'
	assert out.codes['/up'] or { 0 } == h2_err_retryable_code, 'upload error was not retryable: ${out.errs['/up']}'
	cend.close_both()
}

// An out-of-range peer SETTINGS_MAX_FRAME_SIZE (here 0) must fail the connection
// (RFC 7540 6.5.2), not be accepted — a zero frame size would make the send
// path's chunk step 0 and hang it in a zero-length-frame loop. The peer also
// sends a valid response afterwards: with the guard the request fails on the
// bad SETTINGS (processed first); without it the request would wrongly succeed.
fn test_mux_invalid_max_frame_size_fails_connection() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/x' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		peer.write_frame(H2SettingsFrame{
			settings: [
				H2Setting{
					id:    h2_settings_max_frame_size
					value: 0
				},
			]
		}) or {
			peer.fail('settings: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		// A perfectly valid response — the client must still have failed the
		// connection on the illegal SETTINGS processed before it.
		peer.respond_headers(ids[0], false) or {}
		peer.write_frame(H2DataFrame{
			stream_id:  ids[0]
			data:       'nope'.bytes()
			end_stream: true
		}) or {}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.statuses['/x'] or { 0 } != 200, 'request must not succeed on an illegal SETTINGS_MAX_FRAME_SIZE'
	err_msg := out.errs['/x'] or { '' }
	assert err_msg != '', 'expected the connection to fail, not hang or succeed'
	assert err_msg.contains('MAX_FRAME_SIZE'), 'unexpected error: ${err_msg}'
	cend.close_both()
}

// A response header block split across HEADERS + CONTINUATION is reassembled.
fn test_mux_continuation_assembly() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/cont' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		block := peer.encoder.encode([H2HeaderField{':status', '200'},
			H2HeaderField{'x-long-header', 'v'.repeat(64)}])
		half := block.len / 2
		peer.write_frame(H2HeadersFrame{
			stream_id:   id
			fragment:    block[..half]
			end_headers: false
		}) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		peer.write_frame(H2ContinuationFrame{
			stream_id:   id
			fragment:    block[half..]
			end_headers: true
		}) or {
			peer.fail('cont: ${err.msg()}')
			return
		}
		peer.write_frame(H2DataFrame{
			stream_id:  id
			data:       'ok'.bytes()
			end_stream: true
		}) or {
			peer.fail('data: ${err.msg()}')
			return
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.errs.len == 0, 'worker errors: ${out.errs}'
	assert out.statuses['/cont'] == 200
	assert out.bodies['/cont'] == 'ok'
	cend.close_both()
}

// When the connection dies with several streams in flight, every waiter must
// wake with an error (no hangs).
fn test_mux_conn_death_wakes_all_waiters() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	for i in 0 .. 4 {
		workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/d${i}' }, mut out)
	}
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		peer.wait_for_headers(4) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		// Kill the connection with all four streams mid-flight.
		peer.end.close_both()
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	assert out.errs.len == 4, 'all four waiters must fail, got: ${out.errs}'
	// A subsequent request on the dead connection fails fast and retryably.
	conn.do(H2ClientRequest{ authority: 't', path: '/postmortem' }) or {
		assert err.code() == h2_err_retryable_code
		return
	}
	assert false, 'a request on a dead connection must fail'
}

// When the connection preface write fails, note_write_failure must be called so
// the connection is torn down and the pool stops admitting new work. Both the
// failing request and any immediately subsequent one must return retryable errors.
fn test_mux_preface_write_failure_tears_down() {
	mut cend, mut pend := new_mux_pipe()
	// Close the write side so the very first transport write (the preface) fails.
	cend.outgoing.close()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut out := &MuxResults{}
	mut w1 := []thread{}
	w1 << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/x' }, mut out)
	w1.wait()
	assert out.errs['/x'] or { '' } != '', 'request on broken transport must fail'
	assert out.codes['/x'] or { 0 } == h2_err_retryable_code, 'preface write failure must be retryable'
	// note_write_failure must have set shutting_down; the second request must
	// fail retryably rather than be admitted to the dead connection.
	mut out2 := &MuxResults{}
	mut w2 := []thread{}
	w2 << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/y' }, mut out2)
	w2.wait()
	assert out2.errs['/y'] or { '' } != '', 'second request on torn-down connection must fail'
	assert out2.codes['/y'] or { 0 } == h2_err_retryable_code, 'second request must also be retryable'
	cend.incoming.close() // unblock the reader thread so it exits cleanly
	pend.close_both()
}

// Applying a positive SETTINGS_INITIAL_WINDOW_SIZE delta to a stream that
// already has extra credit from a WINDOW_UPDATE can overflow the stream's send
// window above 2^31-1, which is a connection FLOW_CONTROL_ERROR (RFC 7540
// §6.9.2). The connection must be failed, not silently keep the invalid window.
fn test_mux_settings_initial_window_delta_overflow_fails_connection() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	// A simple GET keeps the stream registered in c.streams (waiting for a
	// response that never arrives), so the SETTINGS delta has a stream to hit.
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/ov' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		// Raise the stream's send window close to 2^31-1 via WINDOW_UPDATE,
		// then raise SETTINGS_INITIAL_WINDOW_SIZE so the delta pushes the window
		// over the limit.  Stream send_window ≈ 65535 + 0x7fff_0000 = 0x7fff_ffff;
		// delta = 0x7fff_ffff - 65535 = 0x7fff_0000 → new window ≈ 0xfffe_0000
		// which exceeds 0x7fff_ffff → FLOW_CONTROL_ERROR.
		peer.write_frame(H2WindowUpdateFrame{
			stream_id:             1
			window_size_increment: u32(0x7fff_0000)
		}) or {
			peer.fail('wu: ${err.msg()}')
			return
		}
		peer.write_frame(H2SettingsFrame{
			settings: [H2Setting{h2_settings_initial_window_size, u32(0x7fff_ffff)}]
		}) or {
			peer.fail('settings: ${err.msg()}')
			return
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	err_msg := out.errs['/ov'] or { '' }
	assert err_msg != '', 'expected a connection FLOW_CONTROL_ERROR, got success or hang'
	assert err_msg.contains('FLOW_CONTROL_ERROR'), 'unexpected error: ${err_msg}'
	cend.close_both()
}

// A server that ignores our advertised receive window and sends more DATA than
// it allows must trigger a FLOW_CONTROL_ERROR: the connection must be failed,
// not silently buffer unbounded data. on_data blocks the requester from draining
// (and sending WINDOW_UPDATE) while the peer floods the connection window.
fn test_mux_peer_exceeding_recv_window_fails_connection() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	// Deplete the tracked connection-level receive window to 1 byte so the
	// very first DATA frame (2 bytes) immediately overflows it and triggers
	// FLOW_CONTROL_ERROR.  Doing this before any goroutine sends frames means
	// no WINDOW_UPDATE is ever sent that could re-fill the window first, so
	// the result is fully deterministic with no concurrent timing race.
	conn.recv_wmu.lock()
	conn.conn_recv_window = 1
	conn.recv_wmu.unlock()
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		authority: 't'
		path:      '/flood'
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		peer.respond_headers(ids[0], false) or {
			peer.fail('respond: ${err.msg()}')
			return
		}
		// 2-byte DATA frame: connection window is 1 byte, so this exceeds it
		// and must trigger FLOW_CONTROL_ERROR on the client.
		peer.write_frame(H2DataFrame{
			stream_id: ids[0]
			data:      [u8(0), 0]
		}) or { peer.fail('data: ${err.msg()}') }
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	err_msg := out.errs['/flood'] or { '' }
	assert err_msg != '', 'expected FLOW_CONTROL_ERROR, got success or hang'
	assert err_msg.contains('FLOW_CONTROL_ERROR'), 'unexpected error: ${err_msg}'
	cend.close_both()
}

// A STREAM-level receive-window violation (RFC 7540 §6.9.1) must reset only that
// stream, not tear down the whole connection: the offending request fails, but
// the connection stays alive (closed stays false) so other streams are unharmed.
fn test_mux_stream_window_violation_resets_only_that_stream() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	// Inflate the connection window so the oversized DATA trips ONLY the
	// stream-level window check, not the connection-level one.
	conn.recv_wmu.lock()
	conn.conn_recv_window = 10_000_000
	conn.recv_wmu.unlock()
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{
		authority: 't'
		path:      '/flood'
	}, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer, mut conn H2MuxConn) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		id := ids[0]
		peer.respond_headers(id, false) or {
			peer.fail('respond: ${err.msg()}')
			return
		}
		// White-box: shrink just this stream's receive window so the next DATA
		// frame exceeds it while the connection window stays valid. No DATA has
		// arrived yet, so the requester has not credited the window back.
		conn.smu.lock()
		if mut s := conn.streams[id] {
			s.mu.lock()
			s.recv_window = 1
			s.mu.unlock()
		}
		conn.smu.unlock()
		peer.write_frame(H2DataFrame{
			stream_id: id
			data:      [u8(0), 0]
		}) or {
			peer.fail('data: ${err.msg()}')
			return
		}
		// The client must RST only this stream; pump until we see it.
		for {
			f := peer.pump() or { return }
			if f is H2RstStreamFrame {
				return
			}
		}
	}(mut peer, mut conn)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	err_msg := out.errs['/flood'] or { '' }
	assert err_msg != '', 'stream-window violation must error the request'
	assert err_msg.contains('FLOW_CONTROL_ERROR') || err_msg.contains('receive window'), 'unexpected error: ${err_msg}'
	// The connection must survive — closed is set only when the reader exits on a
	// connection-level failure, which a stream reset must not cause.
	conn.smu.lock()
	closed := conn.closed
	conn.smu.unlock()
	assert !closed, 'a stream-level flow-control violation must NOT close the connection'
	cend.close_both()
}

// DATA after END_STREAM on a fully "closed" stream (we also sent our END_STREAM)
// is a connection error (RFC 7540 §5.1 MUST).
fn test_mux_data_after_end_stream_on_closed_stream_fails_connection() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	// White-box: register a stream in the "closed" state — both the peer (ended)
	// and we (send_closed) have finished sending.
	mut s := new_h2_mux_stream()
	s.id = 1
	s.headers_done = true
	s.ended = true
	s.send_closed = true
	conn.smu.lock()
	conn.streams[1] = s
	conn.smu.unlock()
	// A DATA frame on the closed stream must fail the whole connection.
	peer.write_frame(H2DataFrame{ stream_id: 1, data: [u8(0)] }) or {
		assert false, 'write: ${err.msg()}'
	}
	mut closed := false
	for _ in 0 .. 2000 {
		conn.smu.lock()
		closed = conn.closed
		conn.smu.unlock()
		if closed {
			break
		}
		time.sleep(time.millisecond)
	}
	assert closed, 'DATA after END_STREAM on a closed stream must fail the connection'
	cend.close_both()
}

// DATA after END_STREAM on a "half-closed (remote)" stream (the peer ended but
// we have NOT closed our send side) is a STREAM error: RST that stream, keep the
// connection alive (RFC 7540 §5.1).
fn test_mux_data_after_end_stream_half_closed_resets_only_stream() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	// White-box: register a stream where the peer has ended but our send side is
	// still open (send_closed = false) — "half-closed (remote)".
	mut s := new_h2_mux_stream()
	s.id = 1
	s.headers_done = true
	s.ended = true
	s.send_closed = false
	conn.smu.lock()
	conn.streams[1] = s
	conn.smu.unlock()
	peer.write_frame(H2DataFrame{ stream_id: 1, data: [u8(0)] }) or {
		assert false, 'write: ${err.msg()}'
	}
	// The client must RST just this stream; pump until we see it.
	mut saw_rst := false
	for _ in 0 .. 50 {
		f := peer.pump() or { break }
		if f is H2RstStreamFrame {
			saw_rst = true
			break
		}
	}
	assert saw_rst, 'half-closed-remote DATA-after-END_STREAM must RST the stream'
	conn.smu.lock()
	closed := conn.closed
	conn.smu.unlock()
	assert !closed, 'a stream-level reset must NOT close the connection'
	cend.close_both()
}

// A malformed Content-Length (string.u64() would leniently parse '12junk' -> 12)
// makes the response malformed; it must be rejected, not accepted as a success.
fn test_mux_malformed_content_length_resets_stream() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/x' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		block := peer.encoder.encode([H2HeaderField{':status', '200'},
			H2HeaderField{'content-length', '12junk'}])
		peer.write_frame(H2HeadersFrame{
			stream_id:   ids[0]
			fragment:    block
			end_headers: true
			end_stream:  false
		}) or {
			peer.fail('resp: ${err.msg()}')
			return
		}
		for {
			f := peer.pump() or { return }
			if f is H2RstStreamFrame {
				return
			}
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	err_msg := out.errs['/x'] or { '' }
	assert err_msg != '', 'malformed Content-Length must error the request, not succeed'
	assert err_msg.to_lower().contains('content-length'), 'unexpected error: ${err_msg}'
	cend.close_both()
}

// RFC 7540 §8.1: a response must begin with HEADERS. A DATA frame before any
// response HEADERS is malformed — reset that stream, but keep the connection.
fn test_mux_data_before_headers_resets_stream() {
	mut cend, mut pend := new_mux_pipe()
	mut conn := new_h2_mux_conn(cend, unsafe { nil })
	mut peer := &MuxTestPeer{
		end: pend
	}
	mut out := &MuxResults{}
	mut workers := []thread{}
	workers << spawn mux_worker(mut conn, H2ClientRequest{ authority: 't', path: '/x' }, mut out)
	peer_thread := spawn fn (mut peer MuxTestPeer) {
		peer.read_preface() or {
			peer.fail('preface: ${err.msg()}')
			return
		}
		ids := peer.wait_for_headers(1) or {
			peer.fail('headers: ${err.msg()}')
			return
		}
		// DATA with no preceding response HEADERS (malformed).
		peer.write_frame(H2DataFrame{
			stream_id: ids[0]
			data:      'oops'.bytes()
		}) or {
			peer.fail('data: ${err.msg()}')
			return
		}
		for {
			f := peer.pump() or { return }
			if f is H2RstStreamFrame {
				return
			}
		}
	}(mut peer)
	workers.wait()
	peer_thread.wait()
	assert peer.failure_msg() == ''
	err_msg := out.errs['/x'] or { '' }
	assert err_msg != '', 'DATA before HEADERS must error the request'
	conn.smu.lock()
	closed := conn.closed
	conn.smu.unlock()
	assert !closed, 'DATA before HEADERS is a stream error; the connection must survive'
	cend.close_both()
}

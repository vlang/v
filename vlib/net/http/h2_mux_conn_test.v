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

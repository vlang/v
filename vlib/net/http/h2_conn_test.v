module http

// Tests for the synchronous HTTP/2 client connection, driven over an in-memory
// transport so no socket is required.

// MockTransport plays a fixed script of server->client bytes and records what
// the client writes.
struct MockTransport {
mut:
	inbound  []u8 // server -> client; the client reads these
	rpos     int
	outbound []u8 // client -> server; what the client wrote
}

fn (mut m MockTransport) read(mut buf []u8) !int {
	if m.rpos >= m.inbound.len {
		return error('eof')
	}
	mut n := m.inbound.len - m.rpos
	if n > buf.len {
		n = buf.len
	}
	for i in 0 .. n {
		buf[i] = m.inbound[m.rpos + i]
	}
	m.rpos += n
	return n
}

fn (mut m MockTransport) write(buf []u8) !int {
	m.outbound << buf
	return buf.len
}

// build_server_stream encodes a server-side response on stream 1: SETTINGS, an
// ACK of our SETTINGS, a HEADERS frame, and optional DATA frames.
fn build_server_stream(resp_fields []H2HeaderField, body_chunks [][]u8) []u8 {
	mut senc := H2HpackEncoder{}
	hdr := senc.encode(resp_fields)
	mut out := []u8{}
	out << H2Frame(H2SettingsFrame{}).encode()
	out << H2Frame(H2SettingsFrame{
		ack: true
	}).encode()
	last_is_headers := body_chunks.len == 0
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    hdr
		end_headers: true
		end_stream:  last_is_headers
	}).encode()
	for i, chunk in body_chunks {
		out << H2Frame(H2DataFrame{
			stream_id:  1
			data:       chunk
			end_stream: i == body_chunks.len - 1
		}).encode()
	}
	return out
}

fn test_h2_conn_basic_get() {
	inbound := build_server_stream([
		H2HeaderField{':status', '200'},
		H2HeaderField{'content-type', 'text/plain'},
	], [' hello'.bytes()])
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{
		method:    'GET'
		authority: 'example.com'
		path:      '/'
	})!
	assert resp.status == 200
	assert resp.body.bytestr() == ' hello'
	assert resp.headers.any(it.name == 'content-type' && it.value == 'text/plain')

	// The client must open with the connection preface.
	assert t.outbound.len > h2_client_preface.len
	assert t.outbound[..h2_client_preface.len].bytestr() == h2_client_preface

	// First client frame after the preface is its SETTINGS.
	f1, n1 := h2_read_frame(t.outbound[h2_client_preface.len..])!
	assert f1 is H2SettingsFrame

	// Next is the request HEADERS on stream 1; decode and check pseudo-headers.
	f2, _ := h2_read_frame(t.outbound[h2_client_preface.len + n1..])!
	headers := f2 as H2HeadersFrame
	assert headers.stream_id == 1
	assert headers.end_stream // GET with no body
	mut dec := H2HpackDecoder{}
	req_fields := dec.decode(headers.fragment)!
	assert req_fields.any(it.name == ':method' && it.value == 'GET')
	assert req_fields.any(it.name == ':scheme' && it.value == 'https')
	assert req_fields.any(it.name == ':authority' && it.value == 'example.com')
	assert req_fields.any(it.name == ':path' && it.value == '/')
}

fn test_h2_conn_multi_data_frames() {
	inbound := build_server_stream([H2HeaderField{':status', '200'}], [
		'foo'.bytes(),
		'bar'.bytes(),
		'baz'.bytes(),
	])
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{ authority: 'h.example' })!
	assert resp.status == 200
	assert resp.body.bytestr() == 'foobarbaz'
}

fn test_h2_conn_post_body() {
	inbound := build_server_stream([H2HeaderField{':status', '201'}], [])
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{
		method:    'POST'
		authority: 'h.example'
		path:      '/submit'
		body:      'payload-data'.bytes()
	})!
	assert resp.status == 201

	// Walk the client's frames and confirm a DATA frame carried the body with
	// END_STREAM, after a HEADERS frame that did not end the stream.
	mut pos := h2_client_preface.len
	mut saw_headers_open := false
	mut data_payload := []u8{}
	mut data_end := false
	for pos < t.outbound.len {
		frame, n := h2_read_frame(t.outbound[pos..])!
		pos += n
		match frame {
			H2HeadersFrame {
				if frame.stream_id == 1 {
					assert !frame.end_stream
					saw_headers_open = true
				}
			}
			H2DataFrame {
				if frame.stream_id == 1 {
					data_payload << frame.data
					if frame.end_stream {
						data_end = true
					}
				}
			}
			else {}
		}
	}
	assert saw_headers_open
	assert data_end
	assert data_payload.bytestr() == 'payload-data'
}

fn test_h2_conn_goaway_errors() {
	mut inbound := []u8{}
	inbound << H2Frame(H2SettingsFrame{}).encode()
	inbound << H2Frame(H2GoawayFrame{
		last_stream_id: 0
		error_code:     u32(H2ErrorCode.protocol_error)
	}).encode()
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	c.do(H2ClientRequest{ authority: 'h.example' }) or {
		assert err.msg().contains('GOAWAY')
		assert err.msg().contains('PROTOCOL_ERROR')
		return
	}
	assert false, 'expected GOAWAY error'
}

fn test_h2_conn_rst_stream_errors() {
	mut senc := H2HpackEncoder{}
	mut inbound := []u8{}
	inbound << H2Frame(H2SettingsFrame{}).encode()
	inbound << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    senc.encode([H2HeaderField{':status', '200'}])
		end_headers: true
	}).encode()
	inbound << H2Frame(H2RstStreamFrame{
		stream_id:  1
		error_code: u32(H2ErrorCode.internal_error)
	}).encode()
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	c.do(H2ClientRequest{ authority: 'h.example' }) or {
		assert err.msg().contains('reset')
		return
	}
	assert false, 'expected RST_STREAM error'
}

fn test_h2_conn_continuation() {
	// Split the response header block across HEADERS + CONTINUATION.
	mut senc := H2HpackEncoder{}
	full := senc.encode([
		H2HeaderField{':status', '200'},
		H2HeaderField{'x-test', 'continued'},
	])
	split := full.len / 2
	mut inbound := []u8{}
	inbound << H2Frame(H2SettingsFrame{}).encode()
	inbound << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    full[..split]
		end_headers: false
	}).encode()
	inbound << H2Frame(H2ContinuationFrame{
		stream_id:   1
		fragment:    full[split..]
		end_headers: true
	}).encode()
	inbound << H2Frame(H2DataFrame{
		stream_id:  1
		data:       'ok'.bytes()
		end_stream: true
	}).encode()
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{ authority: 'h.example' })!
	assert resp.status == 200
	assert resp.body.bytestr() == 'ok'
	assert resp.headers.any(it.name == 'x-test' && it.value == 'continued')
}

fn test_h2_conn_ping_ack() {
	// A server PING before the response must be answered with a PING ACK.
	mut senc := H2HpackEncoder{}
	mut inbound := []u8{}
	inbound << H2Frame(H2SettingsFrame{}).encode()
	inbound << H2Frame(H2PingFrame{
		data: [u8(1), 2, 3, 4, 5, 6, 7, 8]
	}).encode()
	inbound << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    senc.encode([H2HeaderField{':status', '204'}])
		end_headers: true
		end_stream:  true
	}).encode()
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{ authority: 'h.example' })!
	assert resp.status == 204

	// Find a PING ACK in the client's output carrying the same opaque data.
	mut pos := h2_client_preface.len
	mut saw_ping_ack := false
	for pos < t.outbound.len {
		frame, n := h2_read_frame(t.outbound[pos..])!
		pos += n
		if frame is H2PingFrame {
			if frame.ack && frame.data == [u8(1), 2, 3, 4, 5, 6, 7, 8] {
				saw_ping_ack = true
			}
		}
	}
	assert saw_ping_ack
}

fn test_h2_conn_splits_large_request_headers() {
	inbound := build_server_stream([H2HeaderField{':status', '200'}], [])
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	// A header value larger than the default 16 KiB max frame size forces the
	// request header block to be split across HEADERS + CONTINUATION frames.
	big := 'x'.repeat(20000)
	resp := c.do(H2ClientRequest{
		authority: 'h.example'
		headers:   [H2HeaderField{'x-big', big}]
	})!
	assert resp.status == 200

	mut pos := h2_client_preface.len
	mut headers_open := false
	mut continuation_end := false
	mut block := []u8{}
	for pos < t.outbound.len {
		frame, n := h2_read_frame(t.outbound[pos..])!
		pos += n
		match frame {
			H2HeadersFrame {
				if frame.stream_id == 1 {
					assert !frame.end_headers // split, so END_HEADERS not on HEADERS
					headers_open = true
					block << frame.fragment
				}
			}
			H2ContinuationFrame {
				if frame.stream_id == 1 {
					block << frame.fragment
					if frame.end_headers {
						continuation_end = true
					}
				}
			}
			else {}
		}
	}
	assert headers_open
	assert continuation_end
	// The reassembled block must decode back to the original header.
	mut dec := H2HpackDecoder{}
	fields := dec.decode(block)!
	assert fields.any(it.name == 'x-big' && it.value == big)
}

fn test_h2_conn_large_body_flow_control() {
	// Body larger than the initial 64 KiB window: the client sends up to the
	// window, waits, then resumes once the peer grows both the connection and
	// stream windows.
	body := 'a'.repeat(70000)
	mut inbound := []u8{}
	inbound << H2Frame(H2SettingsFrame{}).encode()
	inbound << H2Frame(H2WindowUpdateFrame{
		stream_id:             0
		window_size_increment: 70000
	}).encode()
	inbound << H2Frame(H2WindowUpdateFrame{
		stream_id:             1
		window_size_increment: 70000
	}).encode()
	mut senc := H2HpackEncoder{}
	inbound << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    senc.encode([H2HeaderField{':status', '200'}])
		end_headers: true
		end_stream:  true
	}).encode()
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{
		method:    'POST'
		authority: 'h.example'
		path:      '/upload'
		body:      body.bytes()
	})!
	assert resp.status == 200

	// The client must have sent the entire body, ending with END_STREAM, and no
	// single DATA frame may exceed the max frame size.
	mut pos := h2_client_preface.len
	mut sent := 0
	mut ended := false
	for pos < t.outbound.len {
		frame, n := h2_read_frame(t.outbound[pos..])!
		pos += n
		if frame is H2DataFrame {
			if frame.stream_id == 1 {
				assert frame.data.len <= int(h2_default_max_frame_size)
				sent += frame.data.len
				if frame.end_stream {
					ended = true
				}
			}
		}
	}
	assert sent == body.len
	assert ended
}

fn test_h2_fetch_glue_roundtrip() {
	// Drive the full conversion glue (Request -> H2 -> Response) over the mock
	// transport, without a socket.
	inbound := build_server_stream([
		H2HeaderField{':status', '200'},
		H2HeaderField{'content-type', 'text/plain'},
	], [' world'.bytes()])
	mut t := &MockTransport{
		inbound: inbound
	}
	req := Request{
		user_agent: 'v.http'
	}
	h2req := req.to_h2_request(.get, 'example.com', '/', '', new_header())
	mut c := new_h2_conn(t)
	h2resp := c.do(h2req)!
	resp := h2_response_to_http(h2resp)
	assert resp.status_code == 200
	assert resp.version() == .v2_0
	assert resp.body == ' world'
	assert (resp.header.get_custom('content-type') or { '' }) == 'text/plain'
}

// build_streamed_response builds a server stream that delivers the response
// body in fixed-size chunks, useful for streaming tests.
fn build_streamed_response(status string, content_length string, chunks [][]u8) []u8 {
	mut senc := H2HpackEncoder{}
	mut fields := [H2HeaderField{':status', status}]
	if content_length != '' {
		fields << H2HeaderField{'content-length', content_length}
	}
	hdr := senc.encode(fields)
	mut out := []u8{}
	out << H2Frame(H2SettingsFrame{}).encode()
	out << H2Frame(H2SettingsFrame{
		ack: true
	}).encode()
	last_is_headers := chunks.len == 0
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    hdr
		end_headers: true
		end_stream:  last_is_headers
	}).encode()
	for i, chunk in chunks {
		out << H2Frame(H2DataFrame{
			stream_id:  1
			data:       chunk
			end_stream: i == chunks.len - 1
		}).encode()
	}
	return out
}

// ChunkCapture records on_data invocations via a reference captured by the
// returned closure.
struct ChunkCapture {
mut:
	chunks   [][]u8
	running  []u64
	expected []u64
	status   []int
}

fn make_capture_fn(cap &ChunkCapture) H2DataFn {
	return fn [cap] (chunk []u8, body_so_far u64, body_expected u64, status int) ! {
		unsafe {
			cap.chunks << chunk.clone()
			cap.running << body_so_far
			cap.expected << body_expected
			cap.status << status
		}
	}
}

fn test_h2_on_data_fires_per_chunk() {
	mut cap := &ChunkCapture{}
	inbound := build_streamed_response('200', '14', [
		'foo'.bytes(),
		'bar'.bytes(),
		'baz quux'.bytes(),
	])
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{
		authority: 'h.example'
		on_data:   make_capture_fn(cap)
	})!
	assert resp.status == 200
	assert resp.body.bytestr() == 'foobarbaz quux'
	// Three DATA frames -> three callback invocations.
	assert cap.chunks.len == 3
	assert cap.chunks[0].bytestr() == 'foo'
	assert cap.chunks[1].bytestr() == 'bar'
	assert cap.chunks[2].bytestr() == 'baz quux'
	// body_so_far is cumulative including the current chunk.
	assert cap.running == [u64(3), u64(6), u64(14)]
	// content-length is reported.
	assert cap.expected == [u64(14), u64(14), u64(14)]
	// Status was known by the first callback (headers arrived first).
	assert cap.status == [200, 200, 200]
}

fn test_h2_stop_copying_limit_caps_body_but_keeps_callback() {
	mut cap := &ChunkCapture{}
	inbound := build_streamed_response('200', '', [
		'AAAA'.bytes(),
		'BBBB'.bytes(),
		'CCCC'.bytes(),
	])
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{
		authority:          'h.example'
		on_data:            make_capture_fn(cap)
		stop_copying_limit: 6
	})!
	// Body is capped at 6 bytes (4 from first chunk + 2 from second).
	assert resp.body.bytestr() == 'AAAABB'
	// All three chunks still produced callbacks.
	assert cap.chunks.len == 3
	assert cap.chunks[0].bytestr() == 'AAAA'
	assert cap.chunks[1].bytestr() == 'BBBB'
	assert cap.chunks[2].bytestr() == 'CCCC'
	// body_so_far still reports the true cumulative size.
	assert cap.running == [u64(4), u64(8), u64(12)]
}

fn test_h2_stop_receiving_limit_breaks_early() {
	mut cap := &ChunkCapture{}
	inbound := build_streamed_response('200', '', [
		'XXXX'.bytes(),
		'YYYY'.bytes(),
		'ZZZZ'.bytes(), // should never be delivered
	])
	mut t := &MockTransport{
		inbound: inbound
	}
	mut c := new_h2_conn(t)
	resp := c.do(H2ClientRequest{
		authority:            'h.example'
		on_data:              make_capture_fn(cap)
		stop_receiving_limit: 6
	})!
	// Loop breaks after the second DATA frame (8 bytes >= limit 6); body
	// contains both chunks delivered so far.
	assert resp.body.bytestr() == 'XXXXYYYY'
	assert cap.chunks.len == 2
	assert cap.chunks[1].bytestr() == 'YYYY'

	// On early termination the client must send RST_STREAM(CANCEL) on the
	// stream, and the connection must refuse further requests.
	mut saw_cancel := false
	mut pos := h2_client_preface.len
	for pos < t.outbound.len {
		frame, n := h2_read_frame(t.outbound[pos..])!
		pos += n
		if frame is H2RstStreamFrame {
			if frame.stream_id == 1 && frame.error_code == u32(H2ErrorCode.cancel) {
				saw_cancel = true
			}
		}
	}
	assert saw_cancel, 'expected RST_STREAM(CANCEL) on early termination'
	c.do(H2ClientRequest{ authority: 'h.example' }) or {
		assert err.msg().contains('no longer usable')
		return
	}
	assert false, 'expected error on reuse after early termination'
}

// RFC 9113 §8.2: the synchronous client must reject a response carrying a
// malformed field (connection-specific header or uppercase field name) rather
// than delivering it — parity with the mux path. Conformance gap G1.
fn test_h2_conn_rejects_malformed_response_fields() {
	// A connection-specific header (§8.2.2) is forbidden in HTTP/2.
	inbound1 := build_server_stream([H2HeaderField{':status', '200'},
		H2HeaderField{'transfer-encoding', 'chunked'}], [])
	mut c1 := new_h2_conn(&MockTransport{ inbound: inbound1 })
	if _ := c1.do(H2ClientRequest{ authority: 'h.example' }) {
		assert false, 'connection-specific response header was accepted'
	} else {
		assert err.msg().contains('transfer-encoding'), 'unexpected error: ${err.msg()}'
	}
	// An uppercase field name (§8.2.1) is malformed.
	inbound2 := build_server_stream([H2HeaderField{':status', '200'},
		H2HeaderField{'Content-Type', 'text/plain'}], [])
	mut c2 := new_h2_conn(&MockTransport{ inbound: inbound2 })
	if _ := c2.do(H2ClientRequest{ authority: 'h.example' }) {
		assert false, 'uppercase response header name was accepted'
	} else {
		assert err.msg().contains('uppercase'), 'unexpected error: ${err.msg()}'
	}
}

// RFC 9113 §6.5.2: the client honors the peer's advisory
// SETTINGS_MAX_HEADER_LIST_SIZE and refuses an over-limit request rather than
// emitting it. Conformance gap G4 (set white-box: on the sync path the peer's
// SETTINGS arrive only with the response, after the request is built).
fn test_h2_conn_respects_peer_max_header_list_size() {
	mut c := new_h2_conn(&MockTransport{
		inbound: build_server_stream([H2HeaderField{':status', '200'}], [])
	})
	c.peer.max_header_list_size = 40 // tiny: even the pseudo-headers exceed it
	if _ := c.do(H2ClientRequest{
		method:    'GET'
		scheme:    'https'
		authority: 'example.com'
		path:      '/a-fairly-long-path-to-exceed-the-limit'
	})
	{
		assert false, 'over-limit request was sent'
	} else {
		assert err.msg().contains('MAX_HEADER_LIST_SIZE'), 'unexpected error: ${err.msg()}'
	}
}

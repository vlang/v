// Hermetic round-trip test for the server-side HTTP/2 driver. Uses an
// in-memory blocking ReadWriter pair so client and server share the same
// address space and no socket is required.
module http

import sync
import time

// PipeBuf is a one-way in-memory FIFO between a writer and a reader.
// `read` blocks (poll-with-sleep) until data is available, mirroring socket
// semantics so the h2 client and server can drive each other to completion.
struct PipeBuf {
mut:
	mu     &sync.Mutex = sync.new_mutex()
	data   []u8
	closed bool
}

fn (mut p PipeBuf) write(buf []u8) !int {
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

fn (mut p PipeBuf) read(mut buf []u8) !int {
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

// PipeEnd is one half of a bidirectional pipe: reads come from `incoming`,
// writes go to `outgoing`.
struct PipeEnd {
mut:
	incoming &PipeBuf
	outgoing &PipeBuf
}

fn (mut p PipeEnd) read(mut buf []u8) !int {
	return p.incoming.read(mut buf)!
}

fn (mut p PipeEnd) write(buf []u8) !int {
	return p.outgoing.write(buf)!
}

fn new_pipe() (&PipeEnd, &PipeEnd) {
	mut a := &PipeBuf{}
	mut b := &PipeBuf{}
	client := &PipeEnd{
		incoming: b
		outgoing: a
	}
	server := &PipeEnd{
		incoming: a
		outgoing: b
	}
	return client, server
}

struct ServerEchoHandler {
mut:
	last_method Method
	last_url    string
	last_body   string
}

fn (mut h ServerEchoHandler) handle(req Request) Response {
	h.last_method = req.method
	h.last_url = req.url
	h.last_body = req.data
	mut resp_header := new_header()
	resp_header.add_custom('content-type', 'text/plain') or {}
	// Echo the URL and body back so callers can verify end-to-end delivery
	// without depending on Handler-state mutation across goroutines.
	return Response{
		status_code: 200
		header:      resp_header
		body:        'echo: ${req.url} body=${req.data}'
	}
}

fn test_h2_server_basic_request() {
	mut client_end, mut server_end := new_pipe()

	// Spawn the server-side h2 driver against the server end of the pipe.
	mut handler := ServerEchoHandler{}
	mut handler_iface := Handler(handler)
	spawn fn [mut server_end, mut handler_iface] () {
		mut transport := H2Transport(server_end)
		serve_h2_conn(mut transport, mut handler_iface) or {}
	}()

	// Drive a client-side request through the same pipe.
	mut conn := new_h2_conn(client_end)
	resp := conn.do(H2ClientRequest{
		method:    'GET'
		authority: 'example.com'
		path:      '/hello'
	}) or {
		assert false, 'client do() failed: ${err}'
		return
	}
	assert resp.status == 200
	assert resp.body.bytestr() == 'echo: /hello body='
	assert resp.headers.any(it.name == 'content-type' && it.value == 'text/plain')
}

fn test_h2_server_post_with_body() {
	mut client_end, mut server_end := new_pipe()
	mut handler := ServerEchoHandler{}
	mut handler_iface := Handler(handler)
	spawn fn [mut server_end, mut handler_iface] () {
		mut transport := H2Transport(server_end)
		serve_h2_conn(mut transport, mut handler_iface) or {}
	}()

	mut conn := new_h2_conn(client_end)
	resp := conn.do(H2ClientRequest{
		method:    'POST'
		authority: 'svc.example'
		path:      '/upload'
		body:      'hello world'.bytes()
	}) or {
		assert false, 'client do() failed: ${err}'
		return
	}
	assert resp.status == 200
	assert resp.body.bytestr() == 'echo: /upload body=hello world'
}

struct StatusHandler {
	code int
}

fn (mut h StatusHandler) handle(req Request) Response {
	return Response{
		status_code: h.code
		body:        'status ${h.code}'
	}
}

fn test_h2_server_non_200_status() {
	mut client_end, mut server_end := new_pipe()
	mut handler := StatusHandler{
		code: 404
	}
	mut handler_iface := Handler(handler)
	spawn fn [mut server_end, mut handler_iface] () {
		mut transport := H2Transport(server_end)
		serve_h2_conn(mut transport, mut handler_iface) or {}
	}()

	mut conn := new_h2_conn(client_end)
	resp := conn.do(H2ClientRequest{ authority: 'h.example' }) or {
		assert false, 'client do() failed: ${err}'
		return
	}
	assert resp.status == 404
	assert resp.body.bytestr() == 'status 404'
}

struct BigBodyHandler {
	size int
}

fn (mut h BigBodyHandler) handle(req Request) Response {
	return Response{
		status_code: 200
		body:        'x'.repeat(h.size)
	}
}

// FrameReader reads HTTP/2 frames one at a time off a PipeEnd, buffering any
// leftover bytes between frames.
struct FrameReader {
mut:
	end &PipeEnd
	buf []u8
}

fn (mut r FrameReader) next() !H2Frame {
	for {
		if r.buf.len >= h2_frame_header_len {
			hdr := h2_parse_frame_header(r.buf)!
			total := h2_frame_header_len + int(hdr.length)
			if r.buf.len >= total {
				f := h2_parse_frame(hdr, r.buf[h2_frame_header_len..total])!
				r.buf = r.buf[total..].clone()
				return f
			}
		}
		mut tmp := []u8{len: 4096}
		n := r.end.read(mut tmp)!
		r.buf << tmp[..n]
	}
	return error('unreachable')
}

fn test_h2_server_respects_send_window() {
	mut client_end, mut server_end := new_pipe()
	mut handler_iface := Handler(BigBodyHandler{
		size: 100
	})
	spawn fn [mut server_end, mut handler_iface] () {
		mut transport := H2Transport(server_end)
		serve_h2_conn(mut transport, mut handler_iface) or {}
	}()

	// Raw client: preface + SETTINGS(initial_window_size = 10) + a GET, then a
	// WINDOW_UPDATE(stream 1, +1000). The server must send at most 10 body
	// bytes before consuming the WINDOW_UPDATE, then deliver the rest.
	mut enc := H2HpackEncoder{}
	block := enc.encode([
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', '/big'},
	])
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{
		settings: [H2Setting{h2_settings_initial_window_size, 10}]
	}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    block
		end_headers: true
		end_stream:  true
	}).encode()
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             1
		window_size_increment: 1000
	}).encode()
	client_end.write(out) or {
		assert false, 'client write failed: ${err}'
		return
	}

	mut fr := FrameReader{
		end: client_end
	}
	mut body := []u8{}
	mut status := 0
	mut first_data_len := -1
	mut got_end := false
	mut dec := H2HpackDecoder{}
	for !got_end {
		f := fr.next() or {
			assert false, 'frame read failed: ${err}'
			return
		}
		match f {
			H2HeadersFrame {
				for hf in dec.decode(f.fragment) or { []H2HeaderField{} } {
					if hf.name == ':status' {
						status = hf.value.int()
					}
				}
				if f.end_stream {
					got_end = true
				}
			}
			H2DataFrame {
				if first_data_len < 0 {
					first_data_len = f.data.len
				}
				body << f.data
				if f.end_stream {
					got_end = true
				}
			}
			else {}
		}
	}
	assert status == 200
	assert body.len == 100
	// The first DATA frame must not exceed the initial 10-byte window.
	assert first_data_len <= 10
}

// spawn_h2_echo_server starts the h2 server driver on the server end of a fresh
// pipe and returns the client end for raw-frame scripting.
fn spawn_h2_echo_server() &PipeEnd {
	mut client_end, mut server_end := new_pipe()
	mut handler_iface := Handler(ServerEchoHandler{})
	spawn fn [mut server_end, mut handler_iface] () {
		mut transport := H2Transport(server_end)
		serve_h2_conn(mut transport, mut handler_iface) or {}
	}()
	return client_end
}

// drive_until_rst_or_response writes `out` to the server, then reads frames back
// until it sees a RST_STREAM on stream 1 (returns the error code) or a HEADERS
// frame (a response — returns -1, meaning the request was accepted). It never
// blocks indefinitely: the server always answers a malformed request with
// RST_STREAM and a valid one with HEADERS.
fn drive_until_rst_or_response(mut client_end PipeEnd, out []u8, label string) i64 {
	client_end.write(out) or {
		assert false, '${label}: client write failed: ${err}'
		return -2
	}
	mut fr := FrameReader{
		end: client_end
	}
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		match f {
			H2RstStreamFrame {
				if f.stream_id == 1 {
					return i64(f.error_code)
				}
			}
			H2HeadersFrame {
				return -1
			}
			else {}
		}
	}
	assert false, '${label}: server sent neither RST_STREAM nor a response'
	return -2
}

// malformed_headers_out builds preface + SETTINGS + a single complete HEADERS
// frame (END_HEADERS, END_STREAM) carrying `fields`.
fn malformed_headers_out(fields []H2HeaderField) []u8 {
	mut enc := H2HpackEncoder{}
	block := enc.encode(fields)
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    block
		end_headers: true
		end_stream:  true
	}).encode()
	return out
}

fn assert_request_malformed(fields []H2HeaderField, label string) {
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, malformed_headers_out(fields), label)
	assert code == i64(u32(H2ErrorCode.protocol_error)), '${label}: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

const valid_get_pseudo = [
	H2HeaderField{':method', 'GET'},
	H2HeaderField{':scheme', 'https'},
	H2HeaderField{':authority', 'h.example'},
	H2HeaderField{':path', '/'},
]

// RFC 9113 §8.1.2: a field name with uppercase letters is malformed.
fn test_h2_server_rejects_uppercase_header() {
	mut fields := valid_get_pseudo.clone()
	fields << H2HeaderField{'X-Test', 'v'}
	assert_request_malformed(fields, 'uppercase field name')
}

// RFC 9113 §8.1.2.1: a pseudo-header after a regular field is malformed.
fn test_h2_server_rejects_pseudo_after_regular() {
	fields := [
		H2HeaderField{'x-a', '1'},
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', '/'},
	]
	assert_request_malformed(fields, 'pseudo after regular')
}

// RFC 9113 §8.1.2.2: TE may only carry the value "trailers".
fn test_h2_server_rejects_te_not_trailers() {
	mut fields := valid_get_pseudo.clone()
	fields << H2HeaderField{'te', 'gzip'}
	assert_request_malformed(fields, 'TE other than trailers')
}

// RFC 9113 §8.1.2.2: connection-specific fields are forbidden.
fn test_h2_server_rejects_connection_specific_header() {
	mut fields := valid_get_pseudo.clone()
	fields << H2HeaderField{'connection', 'keep-alive'}
	assert_request_malformed(fields, 'connection-specific field')
}

// RFC 9113 §8.1.2.3: omitting :scheme is malformed.
fn test_h2_server_rejects_missing_scheme() {
	fields := [
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', '/'},
	]
	assert_request_malformed(fields, 'missing :scheme')
}

// RFC 9113 §8.1.2.3: a duplicated request pseudo-header is malformed.
fn test_h2_server_rejects_duplicate_pseudo() {
	for dup in [
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':path', '/'},
		H2HeaderField{':scheme', 'https'},
	] {
		mut fields := valid_get_pseudo.clone()
		fields << dup
		assert_request_malformed(fields, 'duplicate ${dup.name}')
	}
}

// RFC 9113 §8.2.1: a field value containing NUL/CR/LF is malformed (and would be
// a header-injection vector if forwarded to an HTTP/1.x peer).
fn test_h2_server_rejects_control_char_in_value() {
	mut fields := valid_get_pseudo.clone()
	fields << H2HeaderField{'x-evil', 'a\r\nInjected: 1'}
	assert_request_malformed(fields, 'CR/LF in field value')
}

// RFC 9113 §8.3.1: an empty :path pseudo-header is malformed for http/https.
fn test_h2_server_rejects_empty_path() {
	fields := [
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', ''},
	]
	assert_request_malformed(fields, 'empty :path')
}

// RFC 9113 §8.1.2.6: content-length must equal the DATA payload length.
fn test_h2_server_rejects_content_length_mismatch() {
	mut enc := H2HpackEncoder{}
	mut fields := [
		H2HeaderField{':method', 'POST'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', '/upload'},
		H2HeaderField{'content-length', '5'},
	]
	block := enc.encode(fields)
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    block
		end_headers: true
		end_stream:  false
	}).encode()
	// Two DATA frames totalling 2 bytes, not the declared 5 (covers the
	// "sum of multiple DATA frames" variant too).
	out << H2Frame(H2DataFrame{
		stream_id:  1
		data:       'h'.bytes()
		end_stream: false
	}).encode()
	out << H2Frame(H2DataFrame{
		stream_id:  1
		data:       'i'.bytes()
		end_stream: true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, out, 'content-length mismatch')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'content-length mismatch: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

// RFC 9110 §8.6: conflicting duplicate content-length fields are malformed —
// every occurrence is validated, not just the last.
fn test_h2_server_rejects_conflicting_content_length() {
	mut enc := H2HpackEncoder{}
	block := enc.encode([
		H2HeaderField{':method', 'POST'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', '/upload'},
		H2HeaderField{'content-length', '5'},
		H2HeaderField{'content-length', '0'},
	])
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    block
		end_headers: true
		end_stream:  true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, out, 'conflicting content-length')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'conflicting content-length: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §8.1: a trailer section that does not carry END_STREAM is malformed.
// It must be a STREAM error (RST_STREAM) — the connection stays up — not a GOAWAY.
fn test_h2_server_rejects_trailers_without_end_stream() {
	mut enc := H2HpackEncoder{}
	req_block := enc.encode([
		H2HeaderField{':method', 'POST'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', '/upload'},
	])
	trailer_block := enc.encode([
		H2HeaderField{'x-checksum', 'abc123'},
	])
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    req_block
		end_headers: true
		end_stream:  false
	}).encode()
	out << H2Frame(H2DataFrame{
		stream_id:  1
		data:       'hi'.bytes()
		end_stream: false
	}).encode()
	// Trailer HEADERS missing END_STREAM.
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    trailer_block
		end_headers: true
		end_stream:  false
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, out, 'trailers without END_STREAM')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'trailers without END_STREAM: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §8.1: a POST with a trailer section (a 2nd HEADERS block ending the
// stream) is accepted and dispatched.
fn test_h2_server_accepts_post_with_trailers() {
	mut enc := H2HpackEncoder{}
	req_block := enc.encode([
		H2HeaderField{':method', 'POST'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', '/upload'},
	])
	trailer_block := enc.encode([
		H2HeaderField{'x-checksum', 'abc123'},
	])
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    req_block
		end_headers: true
		end_stream:  false
	}).encode()
	out << H2Frame(H2DataFrame{
		stream_id:  1
		data:       'hi'.bytes()
		end_stream: false
	}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    trailer_block
		end_headers: true
		end_stream:  true
	}).encode()

	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'trailers POST: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut dec := H2HpackDecoder{}
	mut status := 0
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		if f is H2HeadersFrame {
			for hf in dec.decode(f.fragment) or { []H2HeaderField{} } {
				if hf.name == ':status' {
					status = hf.value.int()
				}
			}
			break
		}
	}
	assert status == 200, 'trailers POST should succeed, got status ${status}'
}

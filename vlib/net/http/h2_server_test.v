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

// RFC 9113 §4.3: an HPACK decoding error is a CONNECTION error
// (GOAWAY COMPRESSION_ERROR), not a stream reset — the decoder's dynamic table is
// desynced, so the whole connection must close.
fn test_h2_server_hpack_error_closes_connection() {
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{}).encode()
	// HEADERS with an invalid HPACK block: indexed header field with index 0
	// (0x80), which RFC 7541 §6.1 forbids -> decode error.
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    [u8(0x80)]
		end_headers: true
		end_stream:  true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'hpack error: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut code := i64(-1)
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		match f {
			H2GoawayFrame {
				code = i64(f.error_code)
				break
			}
			H2RstStreamFrame {
				// Wrong scope: a per-stream reset for an HPACK error.
				code = -2
				break
			}
			else {}
		}
	}
	assert code == i64(u32(H2ErrorCode.compression_error)), 'HPACK decode error must be GOAWAY(COMPRESSION_ERROR), got ${code}'
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

// drive_until_goaway_or_close writes `out`, then reads frames until it sees a
// GOAWAY (returns its error code) or the connection closes (returns -1). A
// RST_STREAM on any stream is a wrong-scope answer for a connection error and
// returns -2.
fn drive_until_goaway_or_close(mut client_end PipeEnd, out []u8, label string) i64 {
	client_end.write(out) or {
		assert false, '${label}: client write failed: ${err}'
		return -3
	}
	mut fr := FrameReader{
		end: client_end
	}
	for _ in 0 .. 32 {
		f := fr.next() or { return -1 }
		match f {
			H2GoawayFrame {
				return i64(f.error_code)
			}
			H2RstStreamFrame {
				return -2
			}
			else {}
		}
	}
	assert false, '${label}: server sent neither GOAWAY nor closed'
	return -3
}

// preface_and_settings returns the client preface followed by an empty SETTINGS
// frame — the start of every scripted session.
fn preface_and_settings() []u8 {
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{}).encode()
	return out
}

// RFC 9113 §5.1: a DATA frame on an idle stream (one never opened) is a
// connection error of type PROTOCOL_ERROR.
fn test_h2_server_data_on_idle_stream_is_connection_error() {
	mut out := preface_and_settings()
	out << H2Frame(H2DataFrame{
		stream_id:  1
		data:       'x'.bytes()
		end_stream: true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_goaway_or_close(mut client_end, out, 'DATA on idle stream')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'DATA on idle: expected GOAWAY(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §5.1/§6.4: a RST_STREAM on an idle stream is a connection error of
// type PROTOCOL_ERROR.
fn test_h2_server_rst_on_idle_stream_is_connection_error() {
	mut out := preface_and_settings()
	out << H2Frame(H2RstStreamFrame{
		stream_id:  1
		error_code: u32(H2ErrorCode.cancel)
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_goaway_or_close(mut client_end, out, 'RST on idle stream')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'RST on idle: expected GOAWAY(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §5.1: a WINDOW_UPDATE on an idle stream is a connection error of type
// PROTOCOL_ERROR.
fn test_h2_server_window_update_on_idle_stream_is_connection_error() {
	mut out := preface_and_settings()
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             1
		window_size_increment: 100
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_goaway_or_close(mut client_end, out, 'WINDOW_UPDATE on idle stream')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'WINDOW_UPDATE on idle: expected GOAWAY(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §5.1/§6.1: a DATA frame on a closed stream (one already served and
// removed) is a STREAM error of type STREAM_CLOSED — the connection stays up.
fn test_h2_server_data_on_closed_stream_is_stream_closed() {
	mut enc := H2HpackEncoder{}
	block := enc.encode(valid_get_pseudo)
	mut out := preface_and_settings()
	// Open and immediately close stream 1 (HEADERS + END_STREAM -> served).
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    block
		end_headers: true
		end_stream:  true
	}).encode()
	// Then DATA on the now-closed stream 1.
	out << H2Frame(H2DataFrame{
		stream_id:  1
		data:       'x'.bytes()
		end_stream: true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'DATA on closed stream: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut code := i64(-1)
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		if f is H2RstStreamFrame {
			if f.stream_id == 1 {
				code = i64(f.error_code)
				break
			}
		}
	}
	assert code == i64(u32(H2ErrorCode.stream_closed)), 'DATA on closed: expected RST_STREAM(STREAM_CLOSED), got ${code}'
}

// RFC 9113 §5.1.2: a HEADERS frame opening a stream beyond the advertised
// SETTINGS_MAX_CONCURRENT_STREAMS (1) is refused with RST_STREAM(REFUSED_STREAM).
// Both streams omit END_STREAM so the first stays parked (open) when the second
// arrives, exercising the concurrency check rather than serial processing.
fn test_h2_server_refuses_stream_over_concurrency_limit() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  false
	}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  false
	}).encode()
	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'concurrency limit: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut code := i64(-1)
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		if f is H2RstStreamFrame {
			if f.stream_id == 3 {
				code = i64(f.error_code)
				break
			}
		}
	}
	assert code == i64(u32(H2ErrorCode.refused_stream)), 'over-limit stream: expected RST_STREAM(REFUSED_STREAM) on stream 3, got ${code}'
}

// RFC 9113 §5.1: an even (server-initiated) stream id is never opened by this
// server — it is idle, NOT closed, even after the client has opened a higher odd
// stream. A DATA frame on it must be a connection error PROTOCOL_ERROR, not a
// STREAM_CLOSED stream error. Regression for the last_stream_id-parity miss
// (Codex, #27589 discussion_r3488271404).
fn test_h2_server_data_on_even_stream_is_connection_error() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	// Client opens odd stream 3 (served, advancing last_stream_id to 3)...
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()
	// ...then sends DATA on even stream 2, which is below last_stream_id but was
	// never opened (even ids are server-initiated). It is idle, so -> conn error.
	out << H2Frame(H2DataFrame{
		stream_id:  2
		data:       'x'.bytes()
		end_stream: true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_goaway_or_close(mut client_end, out, 'DATA on even idle stream')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'DATA on even idle stream: expected GOAWAY(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §5.1.2: an over-limit stream opened while the server is blocked writing
// a stalled response (flow control) must still be HPACK-decoded and refused — the
// stall path (send_body -> pump_for_window) routes frames through the same dispatch
// as the main loop. Regression for the divergent-reader miss (Codex, #27589
// discussion_r3488302384): set the initial window to 0 so stream 1's response body
// stalls, then send HEADERS for stream 3 before the WINDOW_UPDATE that unblocks it.
fn test_h2_server_refuses_over_limit_stream_during_window_stall() {
	mut enc := H2HpackEncoder{}
	mut out := []u8{}
	out << h2_client_preface.bytes()
	// Zero initial window -> stream 1's response body cannot be sent until a
	// WINDOW_UPDATE arrives, parking the server in pump_for_window.
	out << H2Frame(H2SettingsFrame{
		settings: [H2Setting{h2_settings_initial_window_size, 0}]
	}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()
	// Over-limit stream 3 arrives while stream 1's response is stalled.
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()
	// Unblock stream 1 so its response completes after the refusal.
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             1
		window_size_increment: 1000
	}).encode()

	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'window-stall refusal: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut dec := H2HpackDecoder{}
	mut rst3 := i64(-1)
	mut s1_status := 0
	mut s1_ended := false
	for _ in 0 .. 64 {
		f := fr.next() or { break }
		match f {
			H2RstStreamFrame {
				if f.stream_id == 3 {
					rst3 = i64(f.error_code)
				}
			}
			H2HeadersFrame {
				if f.stream_id == 1 {
					for hf in dec.decode(f.fragment) or { []H2HeaderField{} } {
						if hf.name == ':status' {
							s1_status = hf.value.int()
						}
					}
				}
			}
			H2DataFrame {
				if f.stream_id == 1 && f.end_stream {
					s1_ended = true
				}
			}
			else {}
		}

		if rst3 >= 0 && s1_ended {
			break
		}
	}
	assert rst3 == i64(u32(H2ErrorCode.refused_stream)), 'over-limit stream during stall: expected RST_STREAM(REFUSED_STREAM) on stream 3, got ${rst3}'
	assert s1_status == 200, 'stalled stream 1 should still complete with 200, got ${s1_status}'
	assert s1_ended, 'stalled stream 1 response should complete after WINDOW_UPDATE'
}

// RFC 9113 §6.8 / §5.1.2: GOAWAY's last_stream_id is the highest stream the
// server "might have taken some action on" — a stream refused purely for
// exceeding the concurrency limit (REFUSED_STREAM) was, per §5.1.2, refused
// "prior to any processing" and is already told via its own RST_STREAM that it
// is safe to retry elsewhere. It should not inflate last_stream_id in a later
// GOAWAY beyond the highest stream actually processed.
//
// Setup: stream 1 stays open (ambiguous outcome). Stream 3 is refused (over
// the concurrency limit). Stream 2 (an even/server-initiated id) then triggers
// a hard connection error, forcing a GOAWAY. The reported last_stream_id must
// be 1 (the highest genuinely-processed/ambiguous stream), not 3 (the refused
// one, whose fate is already known from its own RST_STREAM).
fn test_h2_server_goaway_last_stream_id_excludes_refused_stream() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	// Stream 1: left open (no END_STREAM) so its outcome stays ambiguous.
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  false
	}).encode()
	// Stream 3: refused (over concurrency limit) — never processed.
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()
	// Stream 2 (even, server-initiated) is always invalid — forces a hard
	// connection error and a best-effort GOAWAY.
	out << H2Frame(H2HeadersFrame{
		stream_id:   2
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()

	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'GOAWAY last_stream_id: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut goaway_last_id := u32(0xFFFFFFFF)
	mut got_goaway := false
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		if f is H2GoawayFrame {
			goaway_last_id = f.last_stream_id
			got_goaway = true
			break
		}
	}
	assert got_goaway, 'GOAWAY last_stream_id: server did not send a GOAWAY'
	assert goaway_last_id == u32(1), 'GOAWAY last_stream_id: expected 1 (refused stream 3 must not count), got ${goaway_last_id}'
}

// RFC 9113 §6.8: GOAWAY's last_stream_id must include a stream that is still
// mid-CONTINUATION (HEADERS not yet fully assembled) when an unrelated
// connection error forces a GOAWAY — the server has already started "acting
// on" that stream (accepted its id, is holding its partial HPACK block) even
// though finalize_headers has not run yet. Bumping last_processed_stream_id
// only inside finalize_headers misses this window entirely.
//
// Setup: stream 1's HEADERS arrives without END_HEADERS (awaiting
// CONTINUATION). A WINDOW_UPDATE frame then arrives instead of the expected
// CONTINUATION — a RFC 9113 §6.10 connection error, unrelated to stream 1's
// own content. The resulting GOAWAY must report last_stream_id 1, not 0.
fn test_h2_server_goaway_last_stream_id_includes_mid_continuation_stream() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: false
		end_stream:  true
	}).encode()
	// Not a CONTINUATION — violates RFC 9113 §6.10 while stream 1 is still being
	// assembled, forcing a connection error unrelated to stream 1's own bytes.
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             1
		window_size_increment: 100
	}).encode()

	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'GOAWAY mid-continuation: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut goaway_last_id := u32(0xFFFFFFFF)
	mut got_goaway := false
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		if f is H2GoawayFrame {
			goaway_last_id = f.last_stream_id
			got_goaway = true
			break
		}
	}
	assert got_goaway, 'GOAWAY mid-continuation: server did not send a GOAWAY'
	assert goaway_last_id == u32(1), 'GOAWAY mid-continuation: expected 1 (stream 1 was being acted on), got ${goaway_last_id}'
}

// RFC 9113 §6.10: a standalone CONTINUATION frame with no preceding HEADERS
// block lacking END_HEADERS is invalid, even on a stream id the server has
// already RST'd itself. Regression for Codex, #27589 pullrequestreview-4613845079
// ("Reject orphan CONTINUATION frames on reset streams"): before the fix,
// on_continuation's locally_reset branch accepted ANY CONTINUATION for a
// previously-reset stream id and fed it straight into the stateful HPACK
// decoder, regardless of whether a discard block was actually in progress.
//
// Setup: stream 1 is malformed (missing :scheme) with END_HEADERS already set
// on its one HEADERS frame, so the server RSTs it immediately and
// c.awaiting_cont never becomes 1. A lone CONTINUATION frame for stream 1 then
// arrives out of nowhere. It must be rejected as a connection error
// (GOAWAY PROTOCOL_ERROR), not silently decoded and discarded.
fn test_h2_server_rejects_orphan_continuation_on_reset_stream() {
	// Malformed: :method + :path only, no :scheme -> immediate RST(protocol_error).
	malformed_block := [u8(0x82), 0x84, 0x01, 0x09, 0x68, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c,
		0x65]

	mut enc := H2HpackEncoder{}
	stream3_block := enc.encode(valid_get_pseudo)
	mut out := preface_and_settings()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    malformed_block
		end_headers: true
		end_stream:  true
	}).encode()
	// Orphan CONTINUATION: no preceding HEADERS without END_HEADERS on stream 1.
	out << H2Frame(H2ContinuationFrame{
		stream_id:   1
		fragment:    [u8(0x40), 0x06, 0x78, 0x2d, 0x73, 0x79, 0x6e, 0x63, 0x01, 0x31]
		end_headers: true
	}).encode()
	// A valid, complete request on stream 3 follows. If the server incorrectly
	// treats the orphan CONTINUATION as an accepted discard-continuation instead
	// of a connection error, it keeps serving and answers this one with 200 — the
	// bug manifests as a response here rather than the frame reader blocking
	// forever waiting for a GOAWAY that never comes.
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    stream3_block
		end_headers: true
		end_stream:  true
	}).encode()

	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'orphan CONTINUATION: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut goaway_code := i64(-1)
	mut s3_status := 0
	mut dec := H2HpackDecoder{}
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		match f {
			H2GoawayFrame {
				goaway_code = i64(f.error_code)
			}
			H2HeadersFrame {
				if f.stream_id == 3 {
					for hf in dec.decode(f.fragment) or { []H2HeaderField{} } {
						if hf.name == ':status' {
							s3_status = hf.value.int()
						}
					}
				}
			}
			else {}
		}

		if goaway_code >= 0 || s3_status != 0 {
			break
		}
	}
	assert s3_status == 0, 'orphan CONTINUATION on reset stream: server kept serving after an illegal CONTINUATION (stream 3 got ${s3_status})'
	assert goaway_code == i64(u32(H2ErrorCode.protocol_error)), 'orphan CONTINUATION on reset stream: expected GOAWAY(PROTOCOL_ERROR), got ${goaway_code}'
}

// RFC 9113 §6.4: the locally_reset drain-tracking set must be bounded — an
// unbounded set lets a peer that keeps the connection open while sending an
// endless stream of malformed requests grow server memory without limit.
// Regression for Codex, #27589 pullrequestreview-4613845079 ("Bound
// locally_reset tracking for reset streams"): before the fix, c.locally_reset
// grew by one entry per malformed stream id forever. This is a white-box unit
// test of mark_locally_reset directly (no I/O) since the bound is an internal
// invariant, not something a black-box frame exchange can assert cleanly.
fn test_h2_server_locally_reset_tracking_is_bounded() {
	_, mut server_end := new_pipe()
	mut c := &H2ServerConn{
		transport: H2Transport(server_end)
	}
	for i in 0 .. h2_server_max_locally_reset_tracked + 10 {
		c.mark_locally_reset(u32(2 * i + 1))
	}
	assert c.locally_reset.len <= h2_server_max_locally_reset_tracked, 'locally_reset grew unbounded: len=${c.locally_reset.len}'
	assert c.locally_reset_order.len == c.locally_reset.len
	// The most recent id must survive eviction; an early one must be gone.
	last_id := u32(2 * (h2_server_max_locally_reset_tracked + 9) + 1)
	assert last_id in c.locally_reset, 'most recently reset stream was evicted'
	assert u32(1) !in c.locally_reset, 'oldest reset stream should have been evicted'
}

// RFC 9113 §6.4 / RFC 7541 §2.2: a HEADERS block arriving on a stream the
// server has already RST'd itself (locally_reset) must still be HPACK-decoded
// before being discarded — the dynamic table is connection-wide, so skipping
// the decode desyncs it for every later stream. Regression for Codex,
// #27589 discussion_r3493008528 ("Decode reset-stream HEADERS before ignoring
// them"): before the fix, on_headers took the "invalid client stream id" path
// for this in-flight second HEADERS block — a hard connection error (GOAWAY)
// instead of a silent, HPACK-synced drain.
//
// Setup: stream 1 is malformed (missing :scheme) so the server RSTs it
// immediately (protocol_error) and marks it locally_reset. A second HEADERS
// block then arrives on stream 1 — in-flight, as if sent by the client before
// it received the RST — carrying a literal-with-incremental-indexing entry
// ("x-sync: 1" → dynamic index 62). Stream 3 (a valid, complete request) then
// references that entry via 0xBE. If the block was skipped instead of decoded,
// 0xBE is unresolvable and the connection closes with GOAWAY instead of
// serving stream 3.
fn test_h2_server_hpack_sync_after_locally_reset_stream_headers() {
	// Malformed: :method + :path only, no :scheme -> h2_validate_request_pseudo
	// rejects it (RFC 9113 §8.1.2.3) and the server RSTs stream 1 immediately.
	malformed_block := [u8(0x82), 0x84, 0x01, 0x09, 0x68, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c,
		0x65]
	// Literal-with-incremental-indexing: adds "x-sync: 1" to the dynamic table.
	invalid_block := [u8(0x40), 0x06, 0x78, 0x2d, 0x73, 0x79, 0x6e, 0x63, 0x01, 0x31]
	// A complete, valid request (has :scheme) plus indexed(62) = "x-sync: 1".
	s3_block := [u8(0x82), 0x84, 0x87, 0x01, 0x09, 0x68, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c,
		0x65, 0xBE]

	mut out := preface_and_settings()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    malformed_block
		end_headers: true
		end_stream:  true
	}).encode()
	// In-flight second HEADERS on stream 1, arriving after the server already
	// sent RST_STREAM(1) but before the client would have received it.
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    invalid_block
		end_headers: true
		end_stream:  false
	}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    s3_block
		end_headers: true
		end_stream:  true
	}).encode()

	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'HPACK sync after local RST: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut dec := H2HpackDecoder{}
	mut rst1 := i64(-1)
	mut s3_status := 0
	mut goaway_code := i64(-1)
	for _ in 0 .. 64 {
		f := fr.next() or { break }
		match f {
			H2GoawayFrame {
				goaway_code = i64(f.error_code)
				break
			}
			H2RstStreamFrame {
				if f.stream_id == 1 {
					rst1 = i64(f.error_code)
				}
			}
			H2HeadersFrame {
				if f.stream_id == 3 {
					for hf in dec.decode(f.fragment) or { []H2HeaderField{} } {
						if hf.name == ':status' {
							s3_status = hf.value.int()
						}
					}
				}
			}
			else {}
		}

		if s3_status != 0 && rst1 >= 0 {
			break
		}
	}
	assert goaway_code == -1, 'HPACK sync after local RST: unexpected GOAWAY (code ${goaway_code}) — in-flight block was not decoded'
	assert rst1 == i64(u32(H2ErrorCode.protocol_error)), 'HPACK sync after local RST: expected RST_STREAM(PROTOCOL_ERROR) on stream 1, got ${rst1}'
	assert s3_status == 200, 'HPACK sync after local RST: stream 3 should get 200 (dynamic table intact), got ${s3_status}'
}

// RFC 9113 §6.9: a WINDOW_UPDATE with a zero increment on the connection
// (stream 0) is a connection error of type PROTOCOL_ERROR.
fn test_h2_server_zero_window_increment_on_connection_is_connection_error() {
	mut out := preface_and_settings()
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             0
		window_size_increment: 0
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_goaway_or_close(mut client_end, out, 'zero WINDOW_UPDATE on connection')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'zero conn WINDOW_UPDATE: expected GOAWAY(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §6.9: a WINDOW_UPDATE with a zero increment on a stream is a STREAM
// error of type PROTOCOL_ERROR — the stream is reset, the connection stays up.
fn test_h2_server_zero_window_increment_on_stream_is_stream_error() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	// Open stream 1 without END_STREAM so it stays open when the frame arrives.
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  false
	}).encode()
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             1
		window_size_increment: 0
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, out, 'zero WINDOW_UPDATE on stream')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'zero stream WINDOW_UPDATE: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §6.9.1: WINDOW_UPDATE growing the connection flow-control window
// past 2^31-1 is a connection error of type FLOW_CONTROL_ERROR.
fn test_h2_server_connection_window_overflow_is_flow_control_error() {
	mut out := preface_and_settings()
	// The connection send window starts at 65535, so one maximum increment
	// (2^31-1) pushes it past 2^31-1.
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             0
		window_size_increment: u32(0x7fff_ffff)
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_goaway_or_close(mut client_end, out, 'connection window overflow')
	assert code == i64(u32(H2ErrorCode.flow_control_error)), 'conn window overflow: expected GOAWAY(FLOW_CONTROL_ERROR), got ${code}'
}

// RFC 9113 §6.9.1: WINDOW_UPDATE growing a stream's flow-control window past
// 2^31-1 is a STREAM error of type FLOW_CONTROL_ERROR.
fn test_h2_server_stream_window_overflow_is_stream_error() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  false
	}).encode()
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             1
		window_size_increment: u32(0x7fff_ffff)
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, out, 'stream window overflow')
	assert code == i64(u32(H2ErrorCode.flow_control_error)), 'stream window overflow: expected RST_STREAM(FLOW_CONTROL_ERROR), got ${code}'
}

// RFC 9113 §6.5.2: SETTINGS_ENABLE_PUSH accepts only 0 or 1; any other value is
// a connection error of type PROTOCOL_ERROR.
fn test_h2_server_enable_push_out_of_range_is_connection_error() {
	mut out := []u8{}
	out << h2_client_preface.bytes()
	out << H2Frame(H2SettingsFrame{
		settings: [
			H2Setting{h2_settings_enable_push, 2},
		]
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_goaway_or_close(mut client_end, out, 'ENABLE_PUSH=2')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'ENABLE_PUSH=2: expected GOAWAY(PROTOCOL_ERROR), got ${code}'
}

// RFC 7540 §5.3.1: a HEADERS frame whose priority section depends on its own
// stream is a STREAM error of type PROTOCOL_ERROR. The header block must still
// be HPACK-decoded before the RST (RFC 7541 §2.2).
fn test_h2_server_headers_self_dependency_is_stream_error() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	out << H2Frame(H2HeadersFrame{
		stream_id:    1
		fragment:     enc.encode(valid_get_pseudo)
		end_headers:  true
		end_stream:   true
		has_priority: true
		stream_dep:   1
		weight:       15
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, out, 'HEADERS self-dependency')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'HEADERS self-dep: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

// RFC 7540 §5.3.1: a PRIORITY frame that depends on its own stream is a STREAM
// error of type PROTOCOL_ERROR.
fn test_h2_server_priority_self_dependency_is_stream_error() {
	mut out := preface_and_settings()
	out << H2Frame(H2PriorityFrame{
		stream_id:  1
		stream_dep: 1
		weight:     15
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, out, 'PRIORITY self-dependency')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'PRIORITY self-dep: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

// RFC 7540 §5.3.1 / RFC 9113 §5.1: a self-dependent PRIORITY frame for a stream
// that already completed NORMALLY (full request/response, deleted from
// c.streams by run_request, never touched locally_reset) must NOT trigger a
// fresh RST_STREAM — that stream is already closed, and RST_STREAM is not a
// PRIORITY frame, so sending one on an already-closed stream is itself the
// §5.1 violation the self-dependency guard exists to police. Regression for
// Codex P2 on #27627 (pullrequestreview-4620220448).
fn test_h2_server_priority_self_dep_after_normal_completion_is_ignored() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	// Stream 1: a normal, complete GET request/response -- deleted from
	// c.streams by run_request, never locally_reset.
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()
	// Self-dependent PRIORITY referencing the now normally-closed stream 1.
	out << H2Frame(H2PriorityFrame{
		stream_id:  1
		stream_dep: 1
		weight:     15
	}).encode()
	// A further valid request proves the connection stays usable.
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'PRIORITY self-dep after normal completion: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut rst1_count := 0
	mut resp1_count := 0
	mut got_response3 := false
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		match f {
			H2RstStreamFrame {
				if f.stream_id == 1 {
					rst1_count++
				}
			}
			H2HeadersFrame {
				if f.stream_id == 1 {
					resp1_count++
				}
				if f.stream_id == 3 {
					got_response3 = true
				}
			}
			else {}
		}

		if got_response3 {
			break
		}
	}
	assert resp1_count == 1, 'PRIORITY self-dep after normal completion: expected exactly 1 response on stream 1, got ${resp1_count}'
	assert rst1_count == 0, 'PRIORITY self-dep after normal completion: expected NO RST_STREAM on the already-closed stream 1, got ${rst1_count}'
	assert got_response3, 'PRIORITY self-dep after normal completion: connection should stay usable (stream 3 served)'
}

// RFC 9113 §6.4: a self-dependent PRIORITY on a never-opened (idle) stream is
// itself legal to RST (§5.3.1) — but the id it resets was never added to
// last_stream_id (only HEADERS advances that counter), so classify_stream must
// still recognise it as locally_reset (closed), not idle. Otherwise in-flight
// DATA for that id — sent by the client before it received our RST — is
// wrongly treated as a frame on an IDLE stream (a connection PROTOCOL_ERROR)
// instead of being silently drained per §6.4. Regression for Codex P2 on
// #27627 (pullrequestreview-4618166175).
fn test_h2_server_data_after_priority_self_dep_reset_is_drained() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	// Self-dependent PRIORITY on stream 5 -- never opened via HEADERS, so
	// last_stream_id stays 0. The server should RST(5, PROTOCOL_ERROR).
	out << H2Frame(H2PriorityFrame{
		stream_id:  5
		stream_dep: 5
		weight:     15
	}).encode()
	// DATA in flight for the same id -- must be drained silently (RFC 9113 §6.4),
	// not treated as DATA-on-idle-stream (a connection error).
	out << H2Frame(H2DataFrame{
		stream_id: 5
		data:      'x'.bytes()
	}).encode()
	// A valid request afterwards proves the connection survived (hang-proof: a
	// buggy server GOAWAYs before this response ever arrives).
	out << H2Frame(H2HeadersFrame{
		stream_id:   7
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'DATA after PRIORITY self-dep reset: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut got_goaway := false
	mut rst5_count := 0
	mut got_response7 := false
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		match f {
			H2GoawayFrame {
				// A buggy server GOAWAYs and then never writes again (the test pipe
				// has no explicit close signal) -- break immediately or the next
				// fr.next() blocks forever.
				got_goaway = true
			}
			H2RstStreamFrame {
				if f.stream_id == 5 {
					rst5_count++
				}
			}
			H2HeadersFrame {
				if f.stream_id == 7 {
					got_response7 = true
				}
			}
			else {}
		}

		if got_goaway || got_response7 {
			break
		}
	}
	assert !got_goaway, 'DATA after PRIORITY self-dep reset: connection should NOT GOAWAY (in-flight DATA must be drained, not treated as DATA-on-idle)'
	assert rst5_count == 1, 'DATA after PRIORITY self-dep reset: expected exactly 1 RST_STREAM on stream 5, got ${rst5_count}'
	assert got_response7, 'DATA after PRIORITY self-dep reset: connection should stay usable (stream 7 served)'
}

// RFC 7540 §5.3.1 / RFC 9113 §5.1.2: a HEADERS frame that is BOTH over the
// concurrency limit AND self-dependent must be reported as a malformed request
// (PROTOCOL_ERROR), not REFUSED_STREAM — REFUSED_STREAM tells the client the
// request is safe to retry as-is, but a self-dependent request will always be
// malformed, retry or not. Regression for Codex P2 on #27627
// (pullrequestreview-4620412384).
fn test_h2_server_self_dep_headers_over_concurrency_limit_is_protocol_error() {
	mut enc := H2HpackEncoder{}
	mut out := preface_and_settings()
	// Stream 1 stays open (no END_STREAM) so it occupies the single concurrency
	// slot (h2_server_max_concurrent_streams == 1) when stream 3 arrives.
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  false
	}).encode()
	// Stream 3: over the concurrency limit AND self-dependent.
	out << H2Frame(H2HeadersFrame{
		stream_id:    3
		fragment:     enc.encode(valid_get_pseudo)
		end_headers:  true
		end_stream:   true
		has_priority: true
		stream_dep:   3
		weight:       15
	}).encode()
	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'self-dep + over-limit HEADERS: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut code := i64(-1)
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		if f is H2RstStreamFrame {
			if f.stream_id == 3 {
				code = i64(f.error_code)
				break
			}
		}
	}
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'self-dep + over-limit: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

// A repeated self-dependent PRIORITY on the same id must produce exactly ONE
// RST_STREAM: after the first reset the stream is closed, and RFC 9113 §5.1
// forbids sending further non-PRIORITY frames (a second RST) on it.
fn test_h2_server_priority_self_dependency_rst_sent_once() {
	mut out := preface_and_settings()
	for _ in 0 .. 3 {
		out << H2Frame(H2PriorityFrame{
			stream_id:  1
			stream_dep: 1
			weight:     15
		}).encode()
	}
	// A valid request on stream 3 afterwards proves the connection survived and
	// bounds the read loop with a response.
	mut enc := H2HpackEncoder{}
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    enc.encode(valid_get_pseudo)
		end_headers: true
		end_stream:  true
	}).encode()
	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'repeated PRIORITY self-dep: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut rst_count := 0
	mut got_response := false
	for _ in 0 .. 32 {
		f := fr.next() or { break }
		match f {
			H2RstStreamFrame {
				if f.stream_id == 1 {
					rst_count++
				}
			}
			H2HeadersFrame {
				if f.stream_id == 3 {
					got_response = true
					break
				}
			}
			else {}
		}
	}
	assert rst_count == 1, 'repeated PRIORITY self-dep: expected exactly 1 RST_STREAM, got ${rst_count}'
	assert got_response, 'repeated PRIORITY self-dep: connection should stay usable (stream 3 served)'
}

// RFC 7540 §5.3.1 applies to any HEADERS carrying a priority section — a
// trailing HEADERS (trailer section) with a self-dependency is also a STREAM
// error PROTOCOL_ERROR, and its block is still HPACK-decoded before the RST.
fn test_h2_server_trailer_self_dependency_is_stream_error() {
	mut enc := H2HpackEncoder{}
	req_block := enc.encode([
		H2HeaderField{':method', 'POST'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':path', '/'},
		H2HeaderField{':authority', 'h.example'},
	])
	trailer_block := enc.encode([
		H2HeaderField{'x-checksum', 'abc'},
	])
	mut out := preface_and_settings()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    req_block
		end_headers: true
		end_stream:  false
	}).encode()
	out << H2Frame(H2DataFrame{
		stream_id: 1
		data:      'hi'.bytes()
	}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:    1
		fragment:     trailer_block
		end_headers:  true
		end_stream:   true
		has_priority: true
		stream_dep:   1
		weight:       15
	}).encode()
	mut client_end := spawn_h2_echo_server()
	code := drive_until_rst_or_response(mut client_end, out, 'trailer self-dependency')
	assert code == i64(u32(H2ErrorCode.protocol_error)), 'trailer self-dep: expected RST_STREAM(PROTOCOL_ERROR), got ${code}'
}

// RFC 9113 §5.1 / RFC 7541 §2.2: a HEADERS block arriving on a half-closed
// (remote) stream must be HPACK-decoded before the server sends
// RST_STREAM(STREAM_CLOSED). The HPACK dynamic table is connection-wide; skipping
// the decode desyncs it, so a subsequent request that uses an indexed reference to
// an entry from the skipped block gets GOAWAY(COMPRESSION_ERROR) instead of a 200.
//
// Setup: zero initial window stalls stream 1's response body, keeping stream 1 in
// c.streams while we inject a post-END_STREAM HEADERS for it. That block carries a
// literal-with-incremental-indexing entry ("x-sync: 1" → dynamic index 62). Stream
// 3 then references that entry via 0xBE. If the decode was skipped, 0xBE is
// unresolvable and the connection closes with COMPRESSION_ERROR.
//
// HPACK bytes used (RFC 7541 static table indices):
//   0x82 = indexed(2)  → :method: GET
//   0x84 = indexed(4)  → :path: /
//   0x87 = indexed(7)  → :scheme: https
//   0x01 0x09 ...      → :authority: h.example (literal-without-indexing, name idx 1)
//   0x40 0x06 x-sync 0x01 1 → literal-incremental "x-sync: 1" → dynamic index 62
//   0xBE = indexed(62) → x-sync: 1  (fails with COMPRESSION_ERROR if table desynced)
fn test_h2_server_hpack_sync_after_post_end_stream_headers() {
	// All-manual HPACK: no H2HpackEncoder so the dynamic table stays predictable.
	// Static-table references only for the base pseudo-headers; no new entries.
	base_block := [u8(0x82), 0x84, 0x87, 0x01, 0x09, 0x68, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70,
		0x6c, 0x65]
	// Literal-with-incremental-indexing: adds "x-sync: 1" to the dynamic table.
	invalid_block := [u8(0x40), 0x06, 0x78, 0x2d, 0x73, 0x79, 0x6e, 0x63, 0x01, 0x31]
	// Same pseudo-headers plus indexed(62) = "x-sync: 1" from the dynamic table.
	mut s3_block := base_block.clone()
	s3_block << u8(0xBE)

	mut out := []u8{}
	out << h2_client_preface.bytes()
	// Zero initial window → stream 1's response body cannot be sent; the server
	// parks in pump_for_window with stream 1 still in c.streams.
	out << H2Frame(H2SettingsFrame{
		settings: [H2Setting{h2_settings_initial_window_size, 0}]
	}).encode()
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    base_block
		end_headers: true
		end_stream:  true
	}).encode()
	// Post-END_STREAM HEADERS on stream 1 containing the HPACK incremental entry.
	out << H2Frame(H2HeadersFrame{
		stream_id:   1
		fragment:    invalid_block
		end_headers: true
		end_stream:  false
	}).encode()
	// Stream 3 references the entry from the invalid block (dynamic index 62).
	out << H2Frame(H2HeadersFrame{
		stream_id:   3
		fragment:    s3_block
		end_headers: true
		end_stream:  true
	}).encode()
	// Unblock stream 3's stalled response body.
	out << H2Frame(H2WindowUpdateFrame{
		stream_id:             3
		window_size_increment: 1000
	}).encode()

	mut client_end := spawn_h2_echo_server()
	client_end.write(out) or {
		assert false, 'HPACK sync: client write failed: ${err}'
		return
	}
	mut fr := FrameReader{
		end: client_end
	}
	mut dec := H2HpackDecoder{}
	mut rst1 := i64(-1)
	mut s3_status := 0
	mut goaway_code := i64(-1)
	for _ in 0 .. 64 {
		f := fr.next() or { break }
		match f {
			H2GoawayFrame {
				goaway_code = i64(f.error_code)
				break
			}
			H2RstStreamFrame {
				if f.stream_id == 1 {
					rst1 = i64(f.error_code)
				}
			}
			H2HeadersFrame {
				if f.stream_id == 3 {
					for hf in dec.decode(f.fragment) or { []H2HeaderField{} } {
						if hf.name == ':status' {
							s3_status = hf.value.int()
						}
					}
				}
			}
			else {}
		}

		if s3_status != 0 && rst1 >= 0 {
			break
		}
	}
	assert goaway_code == -1, 'HPACK sync: unexpected GOAWAY (code ${goaway_code}) — post-END_STREAM block was not decoded'
	assert rst1 == i64(u32(H2ErrorCode.stream_closed)), 'HPACK sync: expected RST_STREAM(STREAM_CLOSED) on stream 1, got ${rst1}'
	assert s3_status == 200, 'HPACK sync: stream 3 should get 200 (dynamic table intact), got ${s3_status}'
}

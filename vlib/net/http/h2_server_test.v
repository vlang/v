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

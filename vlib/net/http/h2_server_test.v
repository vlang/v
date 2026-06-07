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

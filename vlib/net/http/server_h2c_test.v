// End-to-end test for prior-knowledge cleartext HTTP/2 (RFC 9113 §3.4) on the
// plain (non-TLS) http.Server listener: a real TCP socket, driven with raw h2
// frames on one connection and a plain HTTP/1.1 request on another, against
// the same `enable_http2: true` server.
module http

import net
import time

const h2c_atimeout = 500 * time.millisecond

struct H2cEchoHandler {}

fn (mut h H2cEchoHandler) handle(req Request) Response {
	mut resp_header := new_header()
	resp_header.add_custom('content-type', 'text/plain') or {}
	return Response{
		status_code: 200
		header:      resp_header
		body:        'h2c: ${req.method} ${req.url}'
	}
}

// server_h2c_frame_reader reads HTTP/2 frames one at a time off a real TCP
// connection, buffering any leftover bytes between frames. Same shape as
// h2_server_test.v's FrameReader, backed by net.TcpConn instead of the
// hermetic in-memory PipeEnd.
struct ServerH2cFrameReader {
mut:
	conn &net.TcpConn
	buf  []u8
}

fn (mut r ServerH2cFrameReader) next() !H2Frame {
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
		n := r.conn.read(mut tmp)!
		r.buf << tmp[..n]
	}
	return error('unreachable')
}

// test_server_h2c_prior_knowledge drives a raw HTTP/2 client preface at a
// plain `enable_http2: true` listener and confirms it is served by the h2
// driver (try_serve_h2c in server_h2c.v), with no TLS involved at all.
fn test_server_h2c_prior_knowledge() {
	mut server := &Server{
		accept_timeout:       h2c_atimeout
		handler:              H2cEchoHandler{}
		addr:                 ''
		show_startup_message: false
		enable_http2:         true
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running() or {
		assert false, 'server did not start: ${err}'
		return
	}
	defer {
		server.close()
		t.wait()
	}

	mut conn := net.dial_tcp(server.addr)!
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(5 * time.second)
	conn.set_write_timeout(5 * time.second)

	mut enc := H2HpackEncoder{}
	block := enc.encode([
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':scheme', 'http'},
		H2HeaderField{':authority', server.addr},
		H2HeaderField{':path', '/h2c'},
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
	conn.write(out)!

	mut fr := ServerH2cFrameReader{
		conn: conn
	}
	mut dec := H2HpackDecoder{}
	mut status := 0
	mut body := []u8{}
	mut got_end := false
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
				body << f.data
				if f.end_stream {
					got_end = true
				}
			}
			else {}
		}
	}
	assert status == 200
	assert body.bytestr() == 'h2c: GET /h2c'
}

// test_server_h2c_does_not_break_http1 confirms enabling h2c on the plain
// listener does not disturb ordinary HTTP/1.1 traffic on the same listener —
// try_serve_h2c must be a true no-op (peek only, nothing consumed) whenever
// the connection is not an h2 preface.
fn test_server_h2c_does_not_break_http1() {
	mut server := &Server{
		accept_timeout:       h2c_atimeout
		handler:              H2cEchoHandler{}
		addr:                 ''
		show_startup_message: false
		enable_http2:         true
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running() or {
		assert false, 'server did not start: ${err}'
		return
	}
	defer {
		server.close()
		t.wait()
	}

	mut conn := net.dial_tcp(server.addr)!
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(5 * time.second)
	conn.set_write_timeout(5 * time.second)

	conn.write('GET /plain HTTP/1.1\r\nHost: ${server.addr}\r\nConnection: close\r\n\r\n'.bytes())!
	mut resp := []u8{}
	mut tmp := []u8{len: 4096}
	for {
		n := conn.read(mut tmp) or { break }
		if n <= 0 {
			break
		}
		resp << tmp[..n]
	}
	text := resp.bytestr()
	assert text.starts_with('HTTP/1.1 200')
	assert text.contains('h2c: GET /plain')
}

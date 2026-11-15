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

fn send_slow_h2c_prefix(addr string) {
	mut conn := net.dial_tcp(addr) or { return }
	defer { conn.close() or {} }
	for _ in 0 .. 12 {
		conn.write_string('G') or { break }
		time.sleep(150 * time.millisecond)
	}
}

fn send_slow_h2c_preface(addr string) {
	mut conn := net.dial_tcp(addr) or { return }
	defer { conn.close() or {} }
	conn.write_string('PRI ') or { return }
	rest := '* HTTP/2.0\r\n\r\nSM\r\n\r\n'
	for i in 0 .. 8 {
		time.sleep(150 * time.millisecond)
		conn.write_string(rest[i..i + 1]) or { break }
	}
}

fn test_server_h2c_sniff_uses_request_head_deadline() {
	mut server := &Server{
		accept_timeout:       h2c_atimeout
		read_timeout:         5 * time.second
		read_header_timeout:  300 * time.millisecond
		worker_num:           1
		handler:              H2cEchoHandler{}
		addr:                 '127.0.0.1:18206'
		enable_http2:         true
		show_startup_message: false
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running()!
	defer {
		server.stop()
		t.wait()
	}

	drip := spawn send_slow_h2c_prefix('127.0.0.1:18206')
	time.sleep(400 * time.millisecond)
	mut legitimate := net.dial_tcp('127.0.0.1:18206')!
	defer { legitimate.close() or {} }
	legitimate.set_read_timeout(1 * time.second)
	legitimate.write_string('GET /deadline HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n')!
	mut response := []u8{}
	mut buffer := []u8{len: 1024}
	for {
		n := legitimate.read(mut buffer) or { break }
		if n <= 0 {
			break
		}
		response << buffer[..n]
		if response.bytestr().contains('\r\n') {
			break
		}
	}
	assert response.bytestr().starts_with('HTTP/1.1 200 OK\r\n')
	drip.wait()

	slow_preface := spawn send_slow_h2c_preface('127.0.0.1:18206')
	time.sleep(400 * time.millisecond)
	mut after_preface := net.dial_tcp('127.0.0.1:18206')!
	defer { after_preface.close() or {} }
	after_preface.set_read_timeout(1 * time.second)
	after_preface.write_string('GET /after-preface HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n')!
	mut after_response := []u8{}
	for {
		n := after_preface.read(mut buffer) or { break }
		if n <= 0 {
			break
		}
		after_response << buffer[..n]
		if after_response.bytestr().contains('\r\n') {
			break
		}
	}
	assert after_response.bytestr().starts_with('HTTP/1.1 200 OK\r\n')
	slow_preface.wait()
}

struct H2cRemoteAddrHandler {}

fn (mut h H2cRemoteAddrHandler) handle(req Request) Response {
	return Response{
		status_code: 200
		body:        '${req.remote_addr}|${req.remote_ip()}|${req.header.get_custom('Remote-Addr') or { '<none>' }}'
	}
}

// test_server_h2c_sets_remote_addr confirms Request.remote_addr is filled in on
// the HTTP/2 path too, not just HTTP/1.1, and that a `remote-addr` field the
// client puts in its HPACK block cannot shadow the real peer address.
fn test_server_h2c_sets_remote_addr() {
	mut server := &Server{
		accept_timeout:       h2c_atimeout
		handler:              H2cRemoteAddrHandler{}
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
		H2HeaderField{':path', '/whoami'},
		H2HeaderField{'remote-addr', '6.6.6.6'},
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
	mut body := []u8{}
	mut got_end := false
	for !got_end {
		f := fr.next() or {
			assert false, 'frame read failed: ${err}'
			return
		}
		match f {
			H2HeadersFrame {
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
	parts := body.bytestr().split('|')
	assert parts.len == 3
	assert parts[0].starts_with('127.0.0.1:'), 'h2c remote_addr: ${parts[0]}'
	assert parts[1] == '127.0.0.1'
	assert parts[2] == '127.0.0.1'
}

module http

import net
import net.mbedtls
import time

// Regression coverage for vlang/v#27705: the one-shot vschannel HTTP/1.1
// client (vschannel_h1_do/vschannel_h1_on_open, thirdparty/vschannel/
// vschannel.c) used to have no way to know a response was complete other
// than the peer closing the connection or sending a TLS close_notify. HTTP/
// 1.1 keep-alive is the default -- a compliant server is free to leave the
// connection open after responding -- so a peer that does that stalled the
// client for as long as the peer chose to keep the socket open.

const vh1t_cert_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const vh1t_key_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'
const vh1t_body = 'hello from a keep-alive peer'

// vh1t_serve_one responds once, then holds the connection open for longer
// than the test's own completion-time bound before finally closing --
// exactly what a compliant HTTP/1.1 keep-alive peer is allowed to do. Bounded
// (rather than left open forever) so this test cannot hang indefinitely even
// if the fix regresses; the assertion below only needs the client to finish
// well before the server closes, not for the server to never close at all.
fn vh1t_serve_one(mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	conn.set_read_timeout(10 * time.second)
	mut buf := []u8{len: 4096}
	mut sofar := []u8{}
	for !sofar.bytestr().contains('\r\n\r\n') {
		n := conn.read(mut buf) or { return }
		if n <= 0 {
			return
		}
		sofar << buf[..n]
	}
	conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: ${vh1t_body.len}\r\n\r\n${vh1t_body}') or {
		return
	}
	time.sleep(3 * time.second)
}

fn test_vschannel_h1_one_shot_does_not_wait_for_keep_alive_peer_to_close() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or {
		assert false, 'port: ${err}'
		return
	}
	port := port_listener.addr() or {
		assert false, 'addr: ${err}'
		return
	}.port() or {
		assert false, 'addr.port: ${err}'
		return
	}
	port_listener.close() or {}
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     vh1t_cert_path
		cert_key: vh1t_key_path
		validate: false
	}) or {
		assert false, 'listener: ${err}'
		return
	}
	th := spawn fn (mut l mbedtls.SSLListener) {
		mut conn := l.accept() or { return }
		vh1t_serve_one(mut conn)
	}(mut listener)

	started := time.now()
	resp := fetch(
		url:          'https://127.0.0.1:${port}/'
		validate:     false
		enable_http2: false
	) or {
		assert false, 'fetch: ${err}'
		return
	}
	elapsed := time.now() - started

	assert resp.status_code == 200
	assert resp.body == vh1t_body
	assert elapsed < 1500 * time.millisecond, 'expected the one-shot client to stop reading once Content-Length bytes arrived, not wait for the keep-alive peer to close -- took ${elapsed}'

	listener.shutdown() or {}
	th.wait()
}

// vh1t_serve_trailing_ows_cl is vh1t_serve_one with trailing optional
// whitespace after the Content-Length value -- legal per RFC 7230's
// field-value grammar (OWS on both sides), and emitted by real servers. The
// completion detector must trim it rather than fail the strict digit parse
// and fall back to waiting for the peer to close (which would reintroduce
// the exact hang vlang/v#27705 fixed, for these servers only).
fn vh1t_serve_trailing_ows_cl(mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	conn.set_read_timeout(10 * time.second)
	mut buf := []u8{len: 4096}
	mut sofar := []u8{}
	for !sofar.bytestr().contains('\r\n\r\n') {
		n := conn.read(mut buf) or { return }
		if n <= 0 {
			return
		}
		sofar << buf[..n]
	}
	conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: ${vh1t_body.len} \t\r\n\r\n${vh1t_body}') or {
		return
	}
	time.sleep(3 * time.second)
}

fn test_vschannel_h1_content_length_with_trailing_whitespace_still_frames() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or {
		assert false, 'port: ${err}'
		return
	}
	port := port_listener.addr() or {
		assert false, 'addr: ${err}'
		return
	}.port() or {
		assert false, 'addr.port: ${err}'
		return
	}
	port_listener.close() or {}
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     vh1t_cert_path
		cert_key: vh1t_key_path
		validate: false
	}) or {
		assert false, 'listener: ${err}'
		return
	}
	th := spawn fn (mut l mbedtls.SSLListener) {
		mut conn := l.accept() or { return }
		vh1t_serve_trailing_ows_cl(mut conn)
	}(mut listener)

	started := time.now()
	resp := fetch(
		url:          'https://127.0.0.1:${port}/'
		validate:     false
		enable_http2: false
	) or {
		assert false, 'fetch: ${err}'
		return
	}
	elapsed := time.now() - started

	assert resp.status_code == 200
	assert resp.body == vh1t_body
	assert elapsed < 1500 * time.millisecond, 'expected trailing OWS after the Content-Length value to be trimmed, not to defeat completion detection and wait for the peer to close -- took ${elapsed}'

	listener.shutdown() or {}
	th.wait()
}

const vh1t_te_ext_early = '0\r\n\r\n'
const vh1t_te_ext_rest = 'REST-AFTER-FAKE-CHUNK-TERMINATOR'

// vh1t_serve_xchunked responds with `Transfer-Encoding: xchunked` -- an
// extension coding whose NAME merely contains the substring "chunked" -- and
// a raw body whose first bytes happen to look like a complete chunked
// terminator, with the remainder arriving shortly after. A client that
// matches transfer codings by substring instead of by comma-separated token
// (the way net.http's own parse_response/has_header_token does) wrongly
// applies chunk framing, sees the fake terminator, and stops reading --
// silently truncating the body. Unknown codings must fall back to
// read-until-close; the server closes right after the second write, so the
// correct path stays fast.
fn vh1t_serve_xchunked(mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	conn.set_read_timeout(10 * time.second)
	mut buf := []u8{len: 4096}
	mut sofar := []u8{}
	for !sofar.bytestr().contains('\r\n\r\n') {
		n := conn.read(mut buf) or { return }
		if n <= 0 {
			return
		}
		sofar << buf[..n]
	}
	conn.write_string('HTTP/1.1 200 OK\r\nTransfer-Encoding: xchunked\r\n\r\n${vh1t_te_ext_early}') or {
		return
	}
	time.sleep(400 * time.millisecond)
	conn.write_string(vh1t_te_ext_rest) or { return }
}

fn test_vschannel_h1_transfer_encoding_extension_token_is_not_chunked() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or {
		assert false, 'port: ${err}'
		return
	}
	port := port_listener.addr() or {
		assert false, 'addr: ${err}'
		return
	}.port() or {
		assert false, 'addr.port: ${err}'
		return
	}
	port_listener.close() or {}
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     vh1t_cert_path
		cert_key: vh1t_key_path
		validate: false
	}) or {
		assert false, 'listener: ${err}'
		return
	}
	th := spawn fn (mut l mbedtls.SSLListener) {
		mut conn := l.accept() or { return }
		vh1t_serve_xchunked(mut conn)
	}(mut listener)

	resp := fetch(
		url:          'https://127.0.0.1:${port}/'
		validate:     false
		enable_http2: false
	) or {
		assert false, 'fetch: ${err}'
		return
	}

	assert resp.status_code == 200
	// parse_response leaves an unknown transfer coding's body raw (its own
	// has_header_token only matches real comma-separated `chunked` tokens),
	// so the full raw bytes -- including everything after the fake chunk
	// terminator -- must survive to here.
	assert resp.body == vh1t_te_ext_early + vh1t_te_ext_rest, 'body truncated at a fake chunk terminator inside a non-chunked (extension-coded) response'

	listener.shutdown() or {}
	th.wait()
}

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

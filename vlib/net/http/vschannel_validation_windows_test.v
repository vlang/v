module http

import net
import net.mbedtls

const vschannel_test_cert_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const vschannel_test_key_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'

fn start_vschannel_test_https_server() !(int, thread) {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     vschannel_test_cert_path
		cert_key: vschannel_test_key_path
		validate: false
	})!
	return port, spawn serve_vschannel_test_https_server(mut listener)
}

// serve_vschannel_test_https_server accepts one connection, then returns
// (triggering the deferred listener.shutdown()). Every configuration --
// including native Windows without -d no_vschannel -- reuses the ALPN probe
// connection directly for an h2-enabled request that turns out to be
// h1-only (h2_dial_probe_vschannel/h2_dial_probe_ssl's vsc_probe/ssl_probe,
// #27879), so exactly one connection is ever made here.
//
// Deliberately bounded rather than an unbounded loop: a client that aborts
// mid-handshake (exactly what the certificate-rejection test below does)
// fails INSIDE accept() itself (which performs the full TCP accept AND
// handshake as one blocking call, per SSLListener.accept's implementation)
// -- listener.shutdown() only frees the LISTENING socket, so it cannot
// unblock an accept() already stuck processing one specific connection's
// handshake. That test's failure happens on the very first accept() call
// and returns immediately regardless of the bound.
const vschannel_test_https_max_accepts = 1

fn serve_vschannel_test_https_server(mut listener mbedtls.SSLListener) {
	defer {
		listener.shutdown() or {}
	}
	for _ in 0 .. vschannel_test_https_max_accepts {
		mut conn := listener.accept() or { return }
		serve_vschannel_test_https_conn(mut conn)
	}
}

fn serve_vschannel_test_https_conn(mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	mut request_buf := []u8{len: 2048}
	_ = conn.read(mut request_buf) or { return }
	conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok') or {
		return
	}
}

fn test_vschannel_accepts_self_signed_certificate_when_validation_is_disabled() {
	port, server := start_vschannel_test_https_server()!
	resp := fetch(
		url:      'https://127.0.0.1:${port}/'
		validate: false
	)!
	server.wait()
	assert resp.status_code == 200
	assert resp.body == 'ok'
}

fn test_vschannel_rejects_self_signed_certificate_when_validation_is_enabled() {
	port, server := start_vschannel_test_https_server()!
	fetch(
		url:      'https://127.0.0.1:${port}/'
		validate: true
	) or {
		server.wait()
		return
	}
	server.wait()
	assert false, 'expected certificate validation to reject the self-signed certificate'
}

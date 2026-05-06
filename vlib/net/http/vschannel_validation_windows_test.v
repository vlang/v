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
	return port, spawn serve_vschannel_test_https_once(mut listener)
}

fn serve_vschannel_test_https_once(mut listener mbedtls.SSLListener) {
	defer {
		listener.shutdown() or {}
	}
	mut conn := listener.accept() or { return }
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

// vtest vflags: -d use_openssl
// vtest build: !windows
module http

import net
import net.mbedtls

const openssl_validation_cert_path = @VEXEROOT + '/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const openssl_validation_key_path = @VEXEROOT + '/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'

fn start_openssl_validation_server() !(int, thread) {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert: openssl_validation_cert_path
		cert_key: openssl_validation_key_path
		validate: false
	})!
	return port, spawn serve_openssl_validation_once(mut listener)
}

fn serve_openssl_validation_once(mut listener mbedtls.SSLListener) {
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

fn test_openssl_accepts_self_signed_certificate_when_validation_is_disabled() {
	port, server := start_openssl_validation_server()!
	resp := fetch(
		url: 'https://127.0.0.1:${port}/'
		validate: false
	)!
	server.wait()
	assert resp.status_code == 200
	assert resp.body == 'ok'
}

// Before peer verification was enabled on SSL_CTX, SSL_get_verify_result
// returned X509_V_OK without checking the chain and this request succeeded.
fn test_openssl_rejects_self_signed_certificate_when_validation_is_enabled() {
	port, server := start_openssl_validation_server()!
	fetch(
		url: 'https://127.0.0.1:${port}/'
		validate: true
	) or {
		server.wait()
		return
	}
	server.wait()
	assert false, 'expected OpenSSL to reject the self-signed certificate'
}

// The fixture is explicitly trusted as a CA, but it has no identity matching
// 127.0.0.1. Chain verification alone would incorrectly accept this request.
fn test_openssl_rejects_trusted_certificate_for_wrong_host() {
	port, server := start_openssl_validation_server()!
	fetch(
		url: 'https://127.0.0.1:${port}/'
		validate: true
		verify: openssl_validation_cert_path
	) or {
		server.wait()
		return
	}
	server.wait()
	assert false, 'expected OpenSSL to reject the certificate for the wrong host'
}

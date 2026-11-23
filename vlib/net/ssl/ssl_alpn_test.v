// vtest build: !sanitize-memory-clang
module main

import net
import net.ssl
import net.mbedtls

// Hermetic ALPN test for the net.ssl facade. A local mbedtls TLS server
// advertises a protocol list, and clients created via ssl.new_ssl_conn verify
// the negotiated result with SSLConn.negotiated_alpn(). The client exercises
// whichever backend net.ssl is built with (mbedtls by default, OpenSSL under
// -d use_openssl). No network access is required.

const alpn_test_cert = $embed_file('../testdata/localhost_test_cert.pem').to_string()

const alpn_test_key = $embed_file('../testdata/localhost_test_key.pem').to_string()

fn start_alpn_server(server_protos []string) !(&mbedtls.SSLListener, int) {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:                   alpn_test_cert
		cert_key:               alpn_test_key
		validate:               false
		in_memory_verification: true
		alpn_protocols:         server_protos
	})!
	return listener, port
}

fn accept_one(mut listener mbedtls.SSLListener) {
	mut conn := listener.accept() or { return }
	mut buf := []u8{len: 64}
	_ := conn.read(mut buf) or { 0 }
	conn.write_string('ok') or {}
	conn.shutdown() or {}
}

fn dial_and_get_alpn(port int, client_protos []string) !string {
	mut client := ssl.new_ssl_conn(
		validate:       false
		alpn_protocols: client_protos
	)!
	client.dial('127.0.0.1', port)!
	selected := client.negotiated_alpn()
	client.write_string('hi') or {}
	client.shutdown() or {}
	return selected
}

fn test_alpn_negotiates_h2() {
	mut listener, port := start_alpn_server(['h2', 'http/1.1'])!
	defer {
		listener.shutdown() or {}
	}
	spawn accept_one(mut listener)
	selected := dial_and_get_alpn(port, ['h2', 'http/1.1'])!
	assert selected == 'h2'
}

fn test_alpn_falls_back_to_http1() {
	mut listener, port := start_alpn_server(['h2', 'http/1.1'])!
	defer {
		listener.shutdown() or {}
	}
	spawn accept_one(mut listener)
	selected := dial_and_get_alpn(port, ['http/1.1'])!
	assert selected == 'http/1.1'
}

fn test_alpn_empty_when_not_advertised() {
	mut listener, port := start_alpn_server(['h2', 'http/1.1'])!
	defer {
		listener.shutdown() or {}
	}
	spawn accept_one(mut listener)
	// Client advertises no ALPN protocols -> no extension -> empty result.
	selected := dial_and_get_alpn(port, []string{})!
	assert selected == ''
}

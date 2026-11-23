// vtest build: !sanitize-memory-clang
module main

import net
import net.ssl
import net.mbedtls
import time

// Hermetic read-timeout test for the net.ssl facade. A local mbedtls TLS
// server answers one message and then goes silent while keeping the
// connection open; the client (built with whichever backend net.ssl uses:
// mbedtls by default, OpenSSL under -d use_openssl) must get its data back
// promptly, and a subsequent read on the silent connection must fail with a
// timeout after set_read_timeout(), instead of blocking until the server
// closes (vlang/v#28506: OpenSSL's SSL_read on a blocking socket ignored the
// configured timeout entirely).

const timeout_test_cert = $embed_file('../testdata/localhost_test_cert.pem').to_string()

const timeout_test_key = $embed_file('../testdata/localhost_test_key.pem').to_string()

// Bound how long the silent server waits if the client ignores its read timeout.
const server_silence = 6 * time.second

const client_read_timeout = 300 * time.millisecond

fn start_silent_server() !(&mbedtls.SSLListener, int) {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:                   timeout_test_cert
		cert_key:               timeout_test_key
		validate:               false
		in_memory_verification: true
	})!
	return listener, port
}

// accept_answer_then_stall answers the first message and stays silent until
// the client test releases it or the fallback deadline expires.
fn accept_answer_then_stall(mut listener mbedtls.SSLListener, release chan bool) {
	mut conn := listener.accept() or { return }
	mut buf := []u8{len: 64}
	_ := conn.read(mut buf) or { 0 }
	conn.write_string('ok') or {}
	select {
		<-release {
		}
		server_silence {
		}
	}
	conn.shutdown() or {}
}

fn test_read_honors_set_read_timeout_on_a_silent_peer() {
	mut listener, port := start_silent_server()!
	release := chan bool{}
	server := spawn accept_answer_then_stall(mut listener, release)
	defer {
		release.close()
		// Accepted TLS connections borrow the listener's configuration and certificates.
		server.wait()
		listener.shutdown() or {}
	}

	mut client := ssl.new_ssl_conn(validate: false)!
	client.dial('127.0.0.1', port)!
	defer {
		client.shutdown() or {}
	}
	client.set_read_timeout(client_read_timeout)

	// Normal traffic still flows: the answer to the first message arrives.
	client.write_string('hi')!
	mut buf := []u8{len: 64}
	n := client.read(mut buf)!
	assert buf[..n].bytestr() == 'ok'

	// The peer now sends nothing. The read must fail with a timeout after
	// roughly client_read_timeout, not block until the server closes.
	start := time.now()
	mut timed_out := false
	client.read(mut buf) or { timed_out = true }
	elapsed := time.since(start)
	assert timed_out, 'read on a silent peer returned data instead of timing out'
	assert elapsed < server_silence / 2, 'read took ${elapsed} — the read timeout of ${client_read_timeout} was not applied'
}

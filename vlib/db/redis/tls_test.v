module redis

import net
import net.mbedtls
import time

const redis_tls_ca = $embed_file('@VEXEROOT/examples/ssl_server/cert/ca.crt').to_string()
const redis_tls_client_certificate = $embed_file('@VEXEROOT/examples/ssl_server/cert/client.crt').to_string()
const redis_tls_client_key = $embed_file('@VEXEROOT/examples/ssl_server/cert/client.key').to_string()
const redis_tls_certificate = $embed_file('@VEXEROOT/examples/ssl_server/cert/server.crt').to_string()
const redis_tls_private_key = $embed_file('@VEXEROOT/examples/ssl_server/cert/server.key').to_string()

fn redis_tls_listener() !(&mbedtls.SSLListener, u16) {
	mut reservation := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := reservation.addr()!.port()!
	reservation.close()!
	listener := mbedtls.new_ssl_listener('127.0.0.1:${port}',
		cert:                   redis_tls_certificate
		cert_key:               redis_tls_private_key
		verify:                 redis_tls_ca
		validate:               true
		in_memory_verification: true
	)!
	return listener, port
}

fn redis_tls_expect(mut transport mbedtls.SSLConn, expected string) ! {
	mut data := []u8{len: expected.len}
	mut offset := 0
	for offset < data.len {
		mut chunk := []u8{len: data.len - offset}
		n := transport.read(mut chunk)!
		if n <= 0 { return error('TLS fixture received EOF') }
		for i in 0 .. n { data[offset + i] = chunk[i] }
		offset += n
	}
	assert data.bytestr() == expected
}

fn redis_tls_serve(mut listener mbedtls.SSLListener) {
	mut transport := listener.accept_with_timeouts(time.second, time.second) or { return }
	defer { transport.close() or {} }
	redis_tls_expect(mut transport, '*2\r\n$5\r\nHELLO\r\n$1\r\n3\r\n') or { return }
	transport.write_string('%0\r\n') or { return }
	redis_tls_expect(mut transport, '*1\r\n$4\r\nPING\r\n') or { return }
	transport.write_string('+PONG\r\n') or {}
}

fn test_tls_custom_ca_and_server_name() {
	mut listener, port := redis_tls_listener()!
	defer { listener.shutdown() or {} }
	worker := spawn redis_tls_serve(mut listener)
	mut db := connect(
		host:            '127.0.0.1'
		port:            port
		tls:             true
		tls_validate:    true
		tls_ca:          redis_tls_ca
		tls_in_memory:   true
		tls_server_name: 'localhost'
		tls_cert:        redis_tls_client_certificate
		tls_key:         redis_tls_client_key
		read_timeout:    time.second
	)!
	defer { db.close() or {} }
	assert db.ping()! == 'PONG'
	assert db.ssl_conn.config.validate
	assert db.ssl_conn.config.cert == redis_tls_client_certificate
	assert db.ssl_conn.config.cert_key == redis_tls_client_key
	assert !db.conn.is_blocking
	worker.wait()
}

fn test_tls_rejects_wrong_server_name() {
	mut listener, port := redis_tls_listener()!
	defer { listener.shutdown() or {} }
	worker := spawn redis_tls_serve(mut listener)
	connect(
		host:            '127.0.0.1'
		port:            port
		tls:             true
		tls_validate:    true
		tls_ca:          redis_tls_ca
		tls_in_memory:   true
		tls_server_name: 'wrong.invalid'
		tls_cert:        redis_tls_client_certificate
		tls_key:         redis_tls_client_key
		read_timeout:    time.second
	) or {
		assert err is ConnectionError
		worker.wait()
		return
	}
	worker.wait()
	assert false, 'TLS accepted a certificate for another host'
}

fn test_tls_client_credentials_are_used_without_server_validation() {
	mut listener, port := redis_tls_listener()!
	defer { listener.shutdown() or {} }
	worker := spawn redis_tls_serve(mut listener)
	mut db := connect(
		host:          '127.0.0.1'
		port:          port
		tls:           true
		tls_in_memory: true
		tls_cert:      redis_tls_client_certificate
		tls_key:       redis_tls_client_key
		read_timeout:  time.second
	)!
	defer { db.close() or {} }
	assert db.ping()! == 'PONG'
	worker.wait()
}

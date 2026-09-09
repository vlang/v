module ssl

import io
import net
import net.mbedtls

const issue_16557_server_cert = @VMODROOT + '/examples/ssl_server/cert/server.crt'
const issue_16557_server_key = @VMODROOT + '/examples/ssl_server/cert/server.key'
const issue_16557_request = 'ping\r\n'
const issue_16557_response_chunk = 'hello over tls'
const issue_16557_response_repeat = 2_000

fn issue_16557_serve_once(mut listener mbedtls.SSLListener) {
	defer {
		listener.shutdown() or {}
	}
	mut conn := listener.accept() or { panic(err) }
	defer {
		conn.shutdown() or {}
	}
	mut request_buf := []u8{len: issue_16557_request.len}
	_ = conn.read(mut request_buf) or { panic(err) }
	response := issue_16557_expected_response()
	mut start := 0
	for start < response.len {
		end := if start + 257 > response.len { response.len } else { start + 257 }
		conn.write_string(response[start..end]) or { panic(err) }
		start = end
	}
}

fn issue_16557_expected_response() string {
	return issue_16557_response_chunk.repeat(issue_16557_response_repeat)
}

fn test_io_read_all_reads_ssl_conn_until_eof() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or { panic(err) }
	port := port_listener.addr() or { panic(err) }.port() or { panic(err) }
	port_listener.close() or { panic(err) }

	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     issue_16557_server_cert
		cert_key: issue_16557_server_key
		validate: false
	}) or { panic(err) }
	server := spawn issue_16557_serve_once(mut listener)

	mut client := new_ssl_conn(validate: false) or { panic(err) }
	defer {
		client.shutdown() or {}
	}
	client.dial('127.0.0.1', port) or { panic(err) }
	client.write_string(issue_16557_request) or { panic(err) }

	bytes := io.read_all(reader: client) or { panic(err) }
	server.wait()

	assert bytes == issue_16557_expected_response().bytes()
}

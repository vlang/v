module http

import net
import net.mbedtls
import time

const handshake_fallback_cert = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const handshake_fallback_key = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'

fn test_vschannel_fallback_is_only_for_invalid_token_during_handshake() {
	status := -2146893048
	assert vschannel_handshake_retry_allowed(vschannel_handshake_error(status))
	assert vschannel_handshake_error(status).code() == status
	assert vschannel_handshake_error(status).msg() == vschannel_request_error(status).msg()
	// The same status while sending or reading application data is not replayable.
	assert !vschannel_handshake_retry_allowed(vschannel_request_error(status))
	for other in [0, -2146893019, -2146893022, -2146893042, -2146893044] {
		assert !vschannel_handshake_retry_allowed(vschannel_handshake_error(other))
	}
}

fn handshake_fallback_server(mut listener mbedtls.SSLListener) !string {
	defer {
		listener.shutdown() or {}
	}
	mut conn := listener.accept_with_timeouts(5 * time.second, 5 * time.second)!
	defer {
		conn.shutdown() or {}
	}
	conn.set_read_timeout(5 * time.second)
	mut data := []u8{}
	mut buf := []u8{len: 4096}
	for {
		n := conn.read(mut buf)!
		if n <= 0 {
			return error('request closed')
		}
		data << buf[..n]
		if data.bytestr().contains('\r\n\r\npayload') {
			break
		}
	}
	conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok')!
	return data.bytestr()
}

fn test_vschannel_handshake_retry_preserves_request_and_validation() {
	for validate in [false, true] {
		mut reserved := net.listen_tcp(.ip, '127.0.0.1:0')!
		port := reserved.addr()!.port()!
		reserved.close()!
		mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
			cert:     handshake_fallback_cert
			cert_key: handshake_fallback_key
			validate: false
		})!
		server := spawn handshake_fallback_server(mut listener)
		mut req := Request{
			validate:     validate
			enable_http2: false
			max_retries:  1
			read_timeout: 5 * time.second
		}
		req.header.add_custom('X-Fallback-Probe', 'preserved')!
		response := vschannel_retry_handshake(&req, port, .post, '127.0.0.1', '/probe?q=1',
			'payload', req.header, vschannel_handshake_error(-2146893048)) or {
			server.wait() or {}
			assert validate, 'validation disabled must allow the local self-signed certificate: ${err}'
			continue
		}
		assert !validate, 'fallback must retain certificate validation'
		assert response.body == 'ok'
		request := server.wait()!
		assert request.starts_with('POST /probe?q=1 HTTP/1.1\r\n')
		assert request.contains('X-Fallback-Probe: preserved\r\n')
		assert request.ends_with('\r\n\r\npayload')
	}
}

fn test_vschannel_retry_does_not_replay_post_send_or_certificate_errors() {
	for err in [vschannel_request_error(-2146893048), vschannel_handshake_error(-2146893019)] {
		vschannel_retry_handshake(&Request{ max_retries: 1 }, 1, .post, '127.0.0.1', '/',
			'payload', Header{}, err) or {
			assert err.code() in [-2146893048, -2146893019]
			continue
		}
		assert false, 'a non-handshake invalid token or certificate error must propagate'
	}
}

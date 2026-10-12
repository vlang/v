module websocket

// test_parse_client_handshake_rejects_colon_less_header checks that a header
// line without a colon is rejected with an error instead of panicking on
// keys[1]. Such bytes arrive straight off the socket, so indexing without a
// length check is a remote denial of service.
// See https://github.com/vlang/v/issues/30168
fn test_parse_client_handshake_rejects_colon_less_header() {
	mut s := new_server(.ip, 18765, '')
	mut c := Client{}
	bad := 'GET / HTTP/1.1\r\nHost: x\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key\r\n\r\n'
	if _, _ := s.parse_client_handshake(bad, mut c) {
		assert false, 'server must reject a colon-less header line'
	}
	// a well-formed handshake still parses
	good := 'GET / HTTP/1.1\r\nHost: x\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n'
	_, _ := s.parse_client_handshake(good, mut c) or {
		assert false, 'server must accept a well-formed handshake: ${err}'
		return
	}
}

// test_check_handshake_response_rejects_colon_less_header is the client-side
// mirror: a response line without a colon must error, not panic on keys[1].
fn test_check_handshake_response_rejects_colon_less_header() {
	seckey := 'dGhlIHNhbXBsZSBub25jZQ=='
	challenge := create_key_challenge_response(seckey)!
	mut ws := Client{}
	bad := 'HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept\r\n\r\n'
	if _ := ws.check_handshake_response(bad, seckey) {
		assert false, 'client must reject a colon-less header line'
	}
	// a well-formed response still validates
	mut ws2 := Client{}
	good := 'HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${challenge}\r\n\r\n'
	ws2.check_handshake_response(good, seckey) or {
		assert false, 'client must accept a well-formed response: ${err}'
		return
	}
}

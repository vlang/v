module websocket

fn test_malformed_handshake_header_does_not_panic() {
	mut s := new_server(.ip, 0, '/', ServerOpt{})
	mut c := Client{}
	// a header line without a ':' used to index keys[1] of a one element split,
	// which panicked with 'array.get: index out of range (i,a.len):1, 1'
	malformed := 'GET / HTTP/1.1\r\nHost: x\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key\r\n\r\n'
	if _, _ := s.parse_client_handshake(malformed, mut c) {
		assert false, 'a colon-less header line must be rejected'
	} else {
		assert err.code() == 8
	}
	// a well formed handshake still parses
	valid := 'GET / HTTP/1.1\r\nHost: x\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n'
	if _, _ := s.parse_client_handshake(valid, mut c) {
		assert true
	} else {
		assert false, 'a valid handshake must parse: ${err}'
	}
}

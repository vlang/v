[manualfree]
module websocket

import encoding.base64
import strings

// handshake manages the websocket handshake process
fn (mut ws Client) handshake() ? {
	nonce := get_nonce(ws.nonce_size)
	seckey := base64.encode_str(nonce)
	mut sb := strings.new_builder(1024)
	defer {
		unsafe { sb.free() }
	}
	sb.write_string('GET ')
	sb.write_string(ws.uri.resource)
	sb.write_string(ws.uri.querystring)
	sb.write_string(' HTTP/1.1\r\nHost: ')
	sb.write_string(ws.uri.hostname)
	sb.write_string(':')
	sb.write_string(ws.uri.port)
	sb.write_string('\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n')
	sb.write_string('Sec-WebSocket-Key: ')
	sb.write_string(seckey)
	sb.write_string('\r\nSec-WebSocket-Version: 13')
	for key in ws.header.keys() {
		val := ws.header.custom_values(key).join(',')
		sb.write_string('\r\n$key:$val')
	}
	sb.write_string('\r\n\r\n')
	handshake := sb.str()
	defer {
		unsafe { handshake.free() }
	}
	handshake_bytes := handshake.bytes()
	ws.debug_log('sending handshake: $handshake')
	ws.socket_write(handshake_bytes) ?
	ws.read_handshake(seckey) ?
	unsafe { handshake_bytes.free() }
}

// handle_server_handshake manages websocket server handshake process
fn (mut s Server) handle_server_handshake(mut c Client) ?(string, &ServerClient) {
	msg := c.read_handshake_str() ?
	handshake_response, client := s.parse_client_handshake(msg, mut c) ?
	unsafe { msg.free() }
	return handshake_response, client
}

// parse_client_handshake parses result from handshake process
fn (mut s Server) parse_client_handshake(client_handshake string, mut c Client) ?(string, &ServerClient) {
	s.logger.debug('server-> client handshake:\n$client_handshake')
	lines := client_handshake.split_into_lines()
	get_tokens := lines[0].split(' ')
	if get_tokens.len < 3 {
		return error_with_code('unexpected get operation, $get_tokens', 1)
	}
	if get_tokens[0].trim_space() != 'GET' {
		return error_with_code("unexpected request '${get_tokens[0]}', expected 'GET'",
			2)
	}
	if get_tokens[2].trim_space() != 'HTTP/1.1' {
		return error_with_code("unexpected request $get_tokens, expected 'HTTP/1.1'",
			3)
	}
	mut seckey := ''
	mut flags := []Flag{}
	mut key := ''
	for i in 1 .. lines.len {
		if lines[i].len <= 0 || lines[i] == '\r\n' {
			continue
		}
		keys := lines[i].split(':')
		match keys[0] {
			'Upgrade', 'upgrade' {
				flags << .has_upgrade
			}
			'Connection', 'connection' {
				flags << .has_connection
			}
			'Sec-WebSocket-Key', 'sec-websocket-key' {
				key = keys[1].trim_space()
				s.logger.debug('server-> got key: $key')
				seckey = create_key_challenge_response(key) ?
				s.logger.debug('server-> challenge: $seckey, response: ${keys[1]}')
				flags << .has_accept
			}
			else {
				// we ignore other headers like protocol for now
			}
		}
		unsafe { keys.free() }
	}
	if flags.len < 3 {
		return error_with_code('invalid client handshake, $client_handshake', 4)
	}
	server_handshake := 'HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: $seckey\r\n\r\n'
	server_client := &ServerClient{
		resource_name: get_tokens[1]
		client_key: key
		client: c
		server: s
	}
	unsafe {
		lines.free()
		flags.free()
		get_tokens.free()
		seckey.free()
		key.free()
	}
	return server_handshake, server_client
}

// read_handshake_str returns the handshake response
fn (mut ws Client) read_handshake_str() ?string {
	mut total_bytes_read := 0
	mut msg := [1024]byte{}
	mut buffer := [1]byte{}
	for total_bytes_read < 1024 {
		bytes_read := ws.socket_read_ptr(&buffer[0], 1) ?
		if bytes_read == 0 {
			return error_with_code('unexpected no response from handshake', 5)
		}
		msg[total_bytes_read] = buffer[0]
		total_bytes_read++
		if total_bytes_read > 5 && msg[total_bytes_read - 1] == `\n`
			&& msg[total_bytes_read - 2] == `\r` && msg[total_bytes_read - 3] == `\n`
			&& msg[total_bytes_read - 4] == `\r` {
			break
		}
	}
	res := msg[..total_bytes_read].bytestr()
	return res
}

// read_handshake reads the handshake result and check if valid
fn (mut ws Client) read_handshake(seckey string) ? {
	mut msg := ws.read_handshake_str() ?
	ws.check_handshake_response(msg, seckey) ?
	unsafe { msg.free() }
}

// check_handshake_response checks the response from handshake and returns
// the response and secure key provided by the websocket client
fn (mut ws Client) check_handshake_response(handshake_response string, seckey string) ? {
	ws.debug_log('handshake response:\n$handshake_response')
	lines := handshake_response.split_into_lines()
	header := lines[0]
	if !header.starts_with('HTTP/1.1 101') && !header.starts_with('HTTP/1.0 101') {
		return error_with_code('handshake_handler: invalid HTTP status response code, $header',
			6)
	}
	for i in 1 .. lines.len {
		if lines[i].len <= 0 || lines[i] == '\r\n' {
			continue
		}
		keys := lines[i].split(':')
		match keys[0] {
			'Upgrade', 'upgrade' {
				ws.flags << .has_upgrade
			}
			'Connection', 'connection' {
				ws.flags << .has_connection
			}
			'Sec-WebSocket-Accept', 'sec-websocket-accept' {
				ws.debug_log('seckey: $seckey')
				challenge := create_key_challenge_response(seckey) ?
				ws.debug_log('challenge: $challenge, response: ${keys[1]}')
				if keys[1].trim_space() != challenge {
					return error_with_code('handshake_handler: Sec-WebSocket-Accept header does not match computed sha1/base64 response.',
						7)
				}
				ws.flags << .has_accept
				unsafe { challenge.free() }
			}
			else {}
		}
		unsafe { keys.free() }
	}
	unsafe { lines.free() }
	if ws.flags.len < 3 {
		ws.close(1002, 'invalid websocket HTTP headers') ?
		return error_with_code('invalid websocket HTTP headers', 8)
	}
}

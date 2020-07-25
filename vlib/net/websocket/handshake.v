module websocket

fn (mut ws Client) read_handshake(seckey string) {
	ws.log.debug('reading handshake...')
	mut bytes_read := 0
	max_buffer := 1024
	buffer_size := 1
	mut buffer := malloc(max_buffer)
	for bytes_read < max_buffer - 1 {
		mut res := 0
		unsafe {
			res = ws.read_from_server(buffer + bytes_read, buffer_size)
		}
		if res == 0 || res == -1 {
			ws.log.fatal('read_handshake: Failed to read handshake.')
		}
		if unsafe {buffer[bytes_read] == `\n` &&
			buffer[bytes_read - 1] == `\r` && buffer[bytes_read - 2] == `\n` &&
			buffer[bytes_read - 3] == `\r`} {
			break
		}
		bytes_read += buffer_size
	}
	unsafe {
		buffer[max_buffer - 1] = `\0`
	}
	ws.handshake_handler(string(byteptr(buffer)), seckey)
}

fn (mut ws Client) handshake_handler(handshake_response, seckey string) {
	ws.log.debug('handshake_handler:\r\n$handshake_response')
	lines := handshake_response.split_into_lines()
	header := lines[0]
	if !header.starts_with('HTTP/1.1 101') && !header.starts_with('HTTP/1.0 101') {
		ws.log.fatal('handshake_handler: invalid HTTP status response code')
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
				ws.log.debug('comparing hashes')
				ws.log.debug('seckey: $seckey')
				challenge := create_key_challenge_response(seckey)
				ws.log.debug('challenge: $challenge')
				ws.log.debug('response: ${keys[1]}')
				if keys[1].trim_space() != challenge {
					ws.log.error('handshake_handler: Sec-WebSocket-Accept header does not match computed sha1/base64 response.')
				}
				ws.flags << .has_accept
				unsafe {
					challenge.free()
				}
			}
			else {}
		}
		unsafe {
			keys.free()
		}
	}
	if ws.flags.len < 3 {
		ws.close(1002, 'invalid websocket HTTP headers')
		ws.log.error('invalid websocket HTTP headers')
	}
	ws.log.info('handshake successful!')
	unsafe {
		handshake_response.free()
		lines.free()
		header.free()
	}
}

module v3

// Server-side HTTP/3 frame handlers and request processing.

fn (mut s Server) handle_headers_frame(mut conn ServerConnection, stream_id u64, payload []u8) ! {
	conn.mu.lock()
	headers := conn.decoder.decode(payload) or {
		conn.mu.unlock()
		return err
	}
	conn.mu.unlock()

	conn.mu.lock()
	mut stream := conn.streams[stream_id] or {
		new_stream := &ServerStream{
			id: stream_id
		}
		conn.streams[stream_id] = new_stream
		new_stream
	}
	stream.headers << headers
	local_headers := stream.headers.clone()
	conn.mu.unlock()

	mut method := ''
	for h in local_headers {
		if h.name == ':method' {
			method = h.value
			break
		}
	}

	has_body := method == 'POST' || method == 'PUT' || method == 'PATCH'
	if !has_body {
		conn.mu.lock()
		already_done := stream.request_complete
		if !already_done {
			stream.request_complete = true
		}
		conn.mu.unlock()

		if !already_done {
			s.process_request(mut conn, stream)!
		}
	}
}

fn (mut s Server) handle_data_frame(mut conn ServerConnection, stream_id u64, payload []u8) ! {
	conn.mu.lock()
	mut stream := conn.streams[stream_id] or {
		conn.mu.unlock()
		return error('stream ${stream_id} not found')
	}
	stream.data << payload
	already_done := stream.request_complete
	if !already_done {
		stream.request_complete = true
	}
	conn.mu.unlock()

	if !already_done {
		s.process_request(mut conn, stream)!
	}
}

fn (mut s Server) handle_settings_frame(mut conn ServerConnection, payload []u8) ! {
	mut idx := 0
	for idx < payload.len {
		setting_id, bytes_read := decode_varint(payload[idx..])!
		idx += bytes_read

		setting_value, bytes_read2 := decode_varint(payload[idx..])!
		idx += bytes_read2

		match setting_id {
			0x01 { conn.settings.qpack_max_table_capacity = setting_value }
			0x06 { conn.settings.max_field_section_size = setting_value }
			0x07 { conn.settings.qpack_blocked_streams = setting_value }
			else {}
		}
	}

	$if debug {
		eprintln('Received SETTINGS from client')
	}
}

fn (mut s Server) process_request(mut conn ServerConnection, stream &ServerStream) ! {
	mut method := ''
	mut path := ''
	mut headers := map[string]string{}

	for header in stream.headers {
		if header.name == ':method' {
			method = header.value
		} else if header.name == ':path' {
			path = header.value
		} else if !header.name.starts_with(':') {
			headers[header.name] = header.value
		}
	}

	request := ServerRequest{
		method:    method
		path:      path
		headers:   headers
		body:      stream.data
		stream_id: stream.id
	}

	$if debug {
		eprintln('[HTTP/3] ${method} ${path}')
	}

	response := s.config.handler(request)

	s.send_response(mut conn, stream.id, response)!
}

fn (mut s Server) send_response(mut conn ServerConnection, stream_id u64, response ServerResponse) ! {
	mut resp_headers := []HeaderField{cap: 2 + response.headers.len}
	resp_headers << HeaderField{':status', response.status_code.str()}

	for key, value in response.headers {
		resp_headers << HeaderField{key, value}
	}

	if 'content-length' !in response.headers && response.body.len > 0 {
		resp_headers << HeaderField{'content-length', response.body.len.str()}
	}

	conn.mu.lock()
	encoded_headers := conn.encoder.encode(resp_headers)
	conn.mu.unlock()

	estimated_size := 20 + encoded_headers.len + response.body.len
	mut frame_data := []u8{cap: estimated_size}

	frame_data << encode_varint(u64(FrameType.headers))!
	frame_data << encode_varint(u64(encoded_headers.len))!
	frame_data << encoded_headers

	if response.body.len > 0 {
		frame_data << encode_varint(u64(FrameType.data))!
		frame_data << encode_varint(u64(response.body.len))!
		frame_data << response.body
	}

	base_iv := if conn.crypto_ctx.tx_iv.len == 12 {
		conn.crypto_ctx.tx_iv
	} else {
		[]u8{len: 12}
	}

	conn.mu.lock()
	pkt_num := conn.tx_packet_number
	conn.tx_packet_number++
	conn.mu.unlock()

	encrypted := conn.crypto_ctx.encrypt_packet(frame_data, []u8{}, base_iv, pkt_num) or {
		return error('failed to encrypt response: ${err}')
	}

	conn.quic_conn.send_with_crypto(stream_id, encrypted, &conn.crypto_ctx) or {
		return error('failed to send response: ${err}')
	}

	$if debug {
		eprintln('[HTTP/3] Response sent: ${response.status_code}')
	}
}

fn default_server_handler(req ServerRequest) ServerResponse {
	body := 'Hello from HTTP/3 server!\nPath: ${req.path}\nMethod: ${req.method}\nProtocol: HTTP/3 (QUIC)'.bytes()

	return ServerResponse{
		status_code: 200
		headers:     {
			'content-type': 'text/plain'
			'server':       'V HTTP/3 Server'
		}
		body:        body
	}
}

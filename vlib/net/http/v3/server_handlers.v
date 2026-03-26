// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// handle_headers_frame handles a HEADERS frame.
// It stores the decoded headers on the stream but does NOT call process_request
// immediately: a request with a body will have DATA frames following the
// HEADERS frame, and process_request is called only once — after the DATA
// frame(s) — to avoid double-processing (Issue #7).
// For requests without a body (GET, HEAD, etc.) process_request is called here
// after verifying that the stream has not already been processed.
fn (mut s Server) handle_headers_frame(mut conn ServerConnection, stream_id u64, payload []u8) ! {
	// Decode headers using QPACK Decoder for proper static/dynamic table support
	conn.mu.lock()
	headers := conn.decoder.decode(payload) or {
		conn.mu.unlock()
		return err
	}
	conn.mu.unlock()

	// Acquire lock before modifying shared streams map (Issue #31)
	conn.mu.lock()
	mut stream := conn.streams[stream_id] or {
		new_stream := &ServerStream{
			id: stream_id
		}
		conn.streams[stream_id] = new_stream
		new_stream
	}
	stream.headers << headers
	// Copy headers under the lock to avoid a data race when reading them below
	// (Issue M2: stream.headers must not be read outside the mutex).
	local_headers := stream.headers.clone()
	conn.mu.unlock()

	// Determine whether a body is expected by inspecting the :method header.
	// Methods that carry a body in HTTP/3 are POST, PUT, and PATCH.
	// For all other methods we process the request immediately.
	mut method := ''
	for h in local_headers {
		if h.name == ':method' {
			method = h.value
			break
		}
	}

	has_body := method == 'POST' || method == 'PUT' || method == 'PATCH'
	if !has_body {
		// No DATA frame expected — process request immediately.
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
	// For methods with a body, process_request is deferred to handle_data_frame.
}

// handle_data_frame handles a DATA frame.
// Appends the payload to the stream's body buffer and calls process_request
// exactly once when data arrives, guarded by the request_complete flag (Issue #7).
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

// handle_settings_frame handles a SETTINGS frame
fn (mut s Server) handle_settings_frame(mut conn ServerConnection, payload []u8) ! {
	// Parse settings
	mut idx := 0
	for idx < payload.len {
		// Decode setting ID
		setting_id, bytes_read := decode_varint(payload[idx..])!
		idx += bytes_read

		// Decode setting value
		setting_value, bytes_read2 := decode_varint(payload[idx..])!
		idx += bytes_read2

		// Update settings
		match setting_id {
			0x01 { conn.settings.qpack_max_table_capacity = setting_value }
			0x06 { conn.settings.max_field_section_size = setting_value }
			0x07 { conn.settings.qpack_blocked_streams = setting_value }
			else {}
		}
	}

	// Send SETTINGS ACK (simplified)
	$if debug {
		eprintln('Received SETTINGS from client')
	}
}

// process_request processes a complete HTTP/3 request
fn (mut s Server) process_request(mut conn ServerConnection, stream &ServerStream) ! {
	// Build request from headers and data
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

	// Call handler
	response := s.config.handler(request)

	// Send response
	s.send_response(mut conn, stream.id, response)!
}

// send_response sends an HTTP/3 response using the connection's QPACK Encoder
// for proper header compression (RFC 9204).
fn (mut s Server) send_response(mut conn ServerConnection, stream_id u64, response ServerResponse) ! {
	// Build response headers with capacity
	mut resp_headers := []HeaderField{cap: 2 + response.headers.len}
	resp_headers << HeaderField{':status', response.status_code.str()}

	for key, value in response.headers {
		resp_headers << HeaderField{key, value}
	}

	// Add content-length if not present
	if 'content-length' !in response.headers && response.body.len > 0 {
		resp_headers << HeaderField{'content-length', response.body.len.str()}
	}

	// Encode headers using QPACK Encoder (Issue: was using simplified encode_headers)
	conn.mu.lock()
	encoded_headers := conn.encoder.encode(resp_headers)
	conn.mu.unlock()

	// Pre-allocate frame data with estimated size
	estimated_size := 20 + encoded_headers.len + response.body.len
	mut frame_data := []u8{cap: estimated_size}

	// Build HEADERS frame
	frame_data << encode_varint(u64(FrameType.headers))!
	frame_data << encode_varint(u64(encoded_headers.len))!
	frame_data << encoded_headers

	// Build DATA frame if body exists
	if response.body.len > 0 {
		frame_data << encode_varint(u64(FrameType.data))!
		frame_data << encode_varint(u64(response.body.len))!
		frame_data << response.body
	}

	// Use derived tx_iv for nonce derivation; fall back to zeros if not derived yet.
	base_iv := if conn.crypto_ctx.tx_iv.len == 12 {
		conn.crypto_ctx.tx_iv
	} else {
		[]u8{len: 12}
	}

	// Use monotonically increasing tx_packet_number (RFC 9000 §17.1) instead
	// of stream_id, which is NOT the QUIC packet number.
	conn.mu.lock()
	pkt_num := conn.tx_packet_number
	conn.tx_packet_number++
	conn.mu.unlock()

	encrypted := conn.crypto_ctx.encrypt_packet(frame_data, []u8{}, base_iv, pkt_num) or {
		return error('failed to encrypt response: ${err}')
	}

	// Send via QUIC
	conn.quic_conn.send_with_crypto(stream_id, encrypted, &conn.crypto_ctx) or {
		return error('failed to send response: ${err}')
	}

	$if debug {
		eprintln('[HTTP/3] Response sent: ${response.status_code}')
	}
}

// default_server_handler is the default request handler
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

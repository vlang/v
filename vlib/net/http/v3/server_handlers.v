module v3

import net.http.common

// Server-side HTTP/3 frame handlers and request processing.

// decode_and_validate_headers decodes QPACK headers from the payload and
// validates them per RFC 9114 §4.1.2 and §4.2.
fn decode_and_validate_headers(mut conn ServerConnection, payload []u8) ![]HeaderField {
	conn.mu.lock()
	headers := conn.decoder.decode(payload) or {
		conn.mu.unlock()
		return err
	}
	conn.mu.unlock()

	// RFC 9114 §4.2: reject header field names containing uppercase letters
	validate_header_names_lowercase(headers)!
	// RFC 9114 §4.1.2: validate pseudo-headers and forbidden headers
	validate_h3_request_headers(headers)!

	return headers
}

fn (mut s Server) handle_headers_frame(mut conn ServerConnection, stream_id u64, payload []u8) ! {
	headers := decode_and_validate_headers(mut conn, payload)!

	conn.mu.lock()
	// RFC 9114 §4.6: enforce MAX_CONCURRENT_STREAMS limit
	if stream_id !in conn.streams && conn.streams.len >= int(s.config.max_concurrent_streams) {
		conn.mu.unlock()
		return error('H3_ID_ERROR: MAX_CONCURRENT_STREAMS limit reached (${s.config.max_concurrent_streams})')
	}
	mut stream := conn.streams[stream_id] or {
		new_stream := &ServerStream{
			id: stream_id
		}
		conn.streams[stream_id] = new_stream
		new_stream
	}
	stream.headers << headers
	stream.headers_received = true
	// Ensure QUIC-level stream exists so process_stream_fin_events can
	// propagate FIN flags recorded by the ngtcp2 recv_stream_data callback.
	conn.quic_conn.ensure_stream(stream_id)
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
		return error('H3_FRAME_UNEXPECTED: DATA received on stream ${stream_id} before HEADERS (RFC 9114 §4.1)')
	}
	if !stream.headers_received {
		conn.mu.unlock()
		return error('H3_FRAME_UNEXPECTED: DATA before HEADERS on stream ${stream_id} (RFC 9114 §4.1)')
	}
	max_body := s.config.max_request_body_size
	if max_body > 0 && stream.data.len + payload.len > max_body {
		conn.mu.unlock()
		return error('H3_EXCESSIVE_LOAD: request body exceeds max size (${max_body})')
	}
	stream.data << payload
	conn.mu.unlock()
}

fn (mut s Server) handle_settings_frame(mut conn ServerConnection, payload []u8) ! {
	mut idx := 0
	mut seen_ids := []u64{cap: 8}
	for idx < payload.len {
		setting_id, bytes_read := decode_varint(payload[idx..])!
		idx += bytes_read

		if setting_id in seen_ids {
			return error('H3_SETTINGS_ERROR: duplicate setting ID 0x${setting_id:02x} (RFC 9114 §7.2.4)')
		}
		seen_ids << setting_id

		// RFC 9114 §7.2.4.1: HTTP/2 setting identifiers are forbidden in HTTP/3
		if setting_id >= 0x02 && setting_id <= 0x05 {
			return error('H3_SETTINGS_ERROR: HTTP/2 setting identifier 0x${setting_id:02x} is forbidden in HTTP/3 (RFC 9114 §7.2.4.1)')
		}

		setting_value, bytes_read2 := decode_varint(payload[idx..])!
		idx += bytes_read2

		match setting_id {
			0x01 {
				conn.settings.qpack_max_table_capacity = setting_value
				conn.encoder.set_peer_max_table_capacity(int(setting_value))
			}
			0x06 {
				conn.settings.max_field_section_size = setting_value
			}
			0x07 {
				conn.settings.qpack_blocked_streams = setting_value
			}
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
	mut host := ''
	mut header := common.new_header()

	for field in stream.headers {
		if field.name == ':method' {
			method = field.value
		} else if field.name == ':path' {
			path = field.value
		} else if field.name == ':authority' {
			host = field.value
		} else if !field.name.starts_with(':') {
			header.add_custom(field.name, field.value) or {}
		}
	}
	if host != '' && !header.contains(.host) {
		header.set(.host, host)
	}

	request := common.ServerRequest{
		method:    common.method_from_str(method)
		path:      path
		host:      host
		header:    header
		body:      stream.data
		version:   .v3_0
		stream_id: stream.id
	}

	$if debug {
		eprintln('[HTTP/3] ${method} ${path}')
	}

	response := s.config.handler(request)

	s.send_response(mut conn, stream.id, response)!
}

// assemble_response_frames builds HTTP/3 HEADERS + optional DATA frames from
// the encoded headers and response body.
fn assemble_response_frames(encoded_headers []u8, body []u8) ![]u8 {
	estimated_size := 20 + encoded_headers.len + body.len
	mut frame_data := []u8{cap: estimated_size}

	frame_data << encode_varint(u64(FrameType.headers))!
	frame_data << encode_varint(u64(encoded_headers.len))!
	frame_data << encoded_headers

	if body.len > 0 {
		frame_data << encode_varint(u64(FrameType.data))!
		frame_data << encode_varint(u64(body.len))!
		frame_data << body
	}

	return frame_data
}

fn (mut s Server) send_response(mut conn ServerConnection, stream_id u64, response common.ServerResponse) ! {
	resp_entries := response.header.entries()
	mut resp_headers := []HeaderField{cap: 2 + resp_entries.len}
	resp_headers << HeaderField{':status', response.status_code.str()}

	for entry in resp_entries {
		resp_headers << HeaderField{entry.key, entry.value}
	}

	if !response.header.contains_custom('content-length') && response.body.len > 0 {
		resp_headers << HeaderField{'content-length', response.body.len.str()}
	}

	conn.mu.lock()
	encoded_headers := conn.encoder.encode(resp_headers)
	conn.mu.unlock()

	frame_data := assemble_response_frames(encoded_headers, response.body)!

	base_iv := if conn.crypto_ctx.tx_iv.len == 12 {
		conn.crypto_ctx.tx_iv
	} else {
		[]u8{len: 12}
	}

	conn.mu.lock()
	pkt_num := conn.tx_packet_number
	conn.tx_packet_number++
	encrypted := conn.crypto_ctx.encrypt_packet(frame_data, []u8{}, base_iv, pkt_num) or {
		conn.mu.unlock()
		return error('failed to encrypt response: ${err}')
	}
	conn.mu.unlock()

	// Coalesce FIN with the response data to reduce packet count.
	// The client already uses send_frame_with_fin for the last DATA frame;
	// the server mirrors that pattern here.
	conn.quic_conn.send_with_fin(stream_id, encrypted) or {
		return error('failed to send response: ${err}')
	}

	$if debug {
		eprintln('[HTTP/3] Response sent: ${response.status_code}')
	}
}

// check_fin_completions checks the specified streams for completable requests.
// Only streams in check_ids are examined, changing cost from O(all_streams)
// to O(check_ids) per packet. check_ids is the union of FIN event IDs from
// drain_stream_events and stream IDs that had frames dispatched this packet.
// Handles: C1 (FIN arrives in separate packet), H2 (empty-body POST/PUT/PATCH).
fn (mut s Server) check_fin_completions(mut conn ServerConnection, check_ids []u64) {
	conn.mu.lock()
	mut completable := []u64{}
	for sid in check_ids {
		stream := conn.streams[sid] or { continue }
		if stream.headers_received && !stream.request_complete && conn.quic_conn.stream_has_fin(sid) {
			completable << sid
		}
	}
	for sid in completable {
		mut stream := conn.streams[sid] or { continue }
		stream.request_complete = true
	}
	conn.mu.unlock()

	for sid in completable {
		stream := conn.streams[sid] or { continue }
		s.process_request(mut conn, stream) or {
			close_on_h3_error(mut conn, err)
		}
	}
}

fn default_server_handler(req common.ServerRequest) common.ServerResponse {
	return common.ServerResponse{
		status_code: 200
		header:      common.from_map({
			'content-type': 'text/plain'
			'server':       'V HTTP/3 Server'
		})
		body:        'Hello from HTTP/3 server!\nPath: ${req.path}\nMethod: ${req.method}\nProtocol: HTTP/3 (QUIC)'.bytes()
	}
}

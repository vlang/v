module v2

// Server-side frame handlers: SETTINGS, HEADERS/DATA response, PING, and frame I/O.

fn (mut s Server) write_settings(mut conn ServerConn) ! {
	mut payload := []u8{cap: 18}

	payload << [u8(0), u8(3)]
	payload << [u8(s.config.max_concurrent_streams >> 24), u8(s.config.max_concurrent_streams >> 16),
		u8(s.config.max_concurrent_streams >> 8), u8(s.config.max_concurrent_streams)]

	payload << [u8(0), u8(4)]
	payload << [u8(s.config.initial_window_size >> 24), u8(s.config.initial_window_size >> 16),
		u8(s.config.initial_window_size >> 8), u8(s.config.initial_window_size)]

	payload << [u8(0), u8(5)]
	payload << [u8(s.config.max_frame_size >> 24), u8(s.config.max_frame_size >> 16),
		u8(s.config.max_frame_size >> 8), u8(s.config.max_frame_size)]

	frame := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .settings
			flags:      0
			stream_id:  0
		}
		payload: payload
	}

	s.write_frame(mut conn, frame)!
	$if debug {
		eprintln('[HTTP/2] Sent SETTINGS')
	}
}

fn (mut s Server) handle_settings(mut conn ServerConn, frame Frame, mut client_settings ClientSettings, mut ctx ConnContext) ! {
	if frame.header.flags & u8(FrameFlags.ack) != 0 {
		$if debug {
			eprintln('[HTTP/2] Received SETTINGS ACK')
		}
		return
	}

	$if debug {
		eprintln('[HTTP/2] Received SETTINGS')
	}

	pairs := parse_settings_payload(frame.payload)!

	old_header_table_size := client_settings.header_table_size
	old_initial_window := client_settings.initial_window_size
	for pair in pairs {
		apply_setting_pair(pair, mut client_settings)!
	}
	if client_settings.header_table_size != old_header_table_size {
		ctx.encoder.set_max_table_size(int(client_settings.header_table_size))
	}
	if client_settings.initial_window_size != old_initial_window {
		ctx.flow.check_initial_window_overflow(old_initial_window, client_settings.initial_window_size)!
		ctx.flow.adjust_initial_window_size(old_initial_window, client_settings.initial_window_size)
	}

	s.write_frame(mut conn, new_settings_ack_frame())!
	$if debug {
		eprintln('[HTTP/2] Sent SETTINGS ACK')
	}
}

fn apply_setting_pair(pair SettingPair, mut settings ClientSettings) ! {
	validate_setting_value(pair.id, pair.value)!
	match pair.id {
		.header_table_size {
			settings.header_table_size = pair.value
		}
		.max_concurrent_streams {
			settings.max_concurrent_streams = pair.value
		}
		.initial_window_size {
			settings.initial_window_size = pair.value
		}
		.max_frame_size {
			settings.max_frame_size = pair.value
		}
		.max_header_list_size {
			settings.max_header_list_size = pair.value
		}
		.enable_push {}
	}
}

fn build_response_headers(response ServerResponse) []HeaderField {
	resp_entries := response.header.entries()
	mut resp_headers := []HeaderField{cap: 2 + resp_entries.len}
	resp_headers << HeaderField{
		name:  ':status'
		value: response.status_code.str()
	}
	for entry in resp_entries {
		resp_headers << HeaderField{
			name:  entry.key
			value: entry.value
		}
	}
	if response.body.len > 0 && !response.header.contains_custom('content-length') {
		resp_headers << HeaderField{
			name:  'content-length'
			value: response.body.len.str()
		}
	}
	return resp_headers
}

fn (mut s Server) send_response(mut conn ServerConn, stream_id u32, response ServerResponse, mut encoder Encoder, mut flow OutboundFlowControl) ! {
	resp_headers := build_response_headers(response)

	encoded := encoder.encode(resp_headers)

	headers_flags := if response.body.len == 0 {
		u8(FrameFlags.end_headers) | u8(FrameFlags.end_stream)
	} else {
		u8(FrameFlags.end_headers)
	}

	headers_frame := Frame{
		header:  FrameHeader{
			length:     u32(encoded.len)
			frame_type: .headers
			flags:      headers_flags
			stream_id:  stream_id
		}
		payload: encoded
	}

	s.write_frame(mut conn, headers_frame)!

	if response.body.len > 0 {
		s.send_data_with_flow_control(mut conn, stream_id, response.body, mut flow, s.config.max_frame_size)!
	}

	$if debug {
		eprintln('[HTTP/2] Response sent: ${response.status_code} (${response.body.len} bytes)')
	}
}

fn (mut s Server) send_data_with_flow_control(mut conn ServerConn, stream_id u32, body []u8, mut flow OutboundFlowControl, max_frame_size u32) ! {
	window := flow.available_window(stream_id)
	if window <= 0 && body.len > 0 {
		return error('flow control window exhausted for stream ${stream_id}')
	}
	chunks := split_data_for_window(body, window, max_frame_size)
	if chunks.len == 0 && body.len > 0 {
		return error('flow control window exhausted for stream ${stream_id}')
	}
	for i, chunk in chunks {
		is_last := i == chunks.len - 1
		data_flags := if is_last { u8(FrameFlags.end_stream) } else { u8(0) }
		data_frame := Frame{
			header:  FrameHeader{
				length:     u32(chunk.len)
				frame_type: .data
				flags:      data_flags
				stream_id:  stream_id
			}
			payload: chunk
		}
		s.write_frame(mut conn, data_frame)!
		flow.consume(stream_id, i64(chunk.len))
	}
	if chunks.len == 0 {
		empty_frame := Frame{
			header:  FrameHeader{
				length:     0
				frame_type: .data
				flags:      u8(FrameFlags.end_stream)
				stream_id:  stream_id
			}
			payload: []u8{}
		}
		s.write_frame(mut conn, empty_frame)!
	}
}

fn (mut s Server) handle_ping(mut conn ServerConn, frame Frame) ! {
	pf := PingFrame.from_frame(frame)!
	ack_pf := PingFrame{
		ack:  true
		data: pf.data
	}
	s.write_frame(mut conn, ack_pf.to_frame())!
	$if debug {
		eprintln('[HTTP/2] PING/PONG')
	}
}

// handle_priority parses a PRIORITY frame per RFC 7540 §6.3.
// Priority is advisory (RFC 7540 §5.3) and this implementation
// does not use it for stream scheduling. Requests are dispatched in arrival order.
fn handle_priority(frame Frame) {
	pf := PriorityFrame.from_frame(frame) or {
		$if debug {
			eprintln('[HTTP/2] Invalid PRIORITY frame: ${err}')
		}
		return
	}
	$if debug {
		eprintln('[HTTP/2] PRIORITY: stream=${pf.stream_id} dep=${pf.stream_dependency} exclusive=${pf.exclusive} weight=${pf.weight}')
	}
}

fn send_rst_stream(mut conn ServerConn, stream_id u32, error_code ErrorCode) ! {
	rst := RstStreamFrame{
		stream_id:  stream_id
		error_code: error_code
	}
	frame_bytes := rst.to_frame().encode()
	conn.write(frame_bytes)!
}

fn (mut s Server) read_frame(mut conn ServerConn) !Frame {
	return read_frame_from(mut conn, s.config.max_frame_size)
}

fn (mut s Server) write_frame(mut conn ServerConn, frame Frame) ! {
	data := frame.encode()
	conn.write(data)!
}

fn send_window_update(mut conn ServerConn, stream_id u32, increment u32) ! {
	data := new_window_update_frame(stream_id, increment).encode()
	conn.write(data)!
}

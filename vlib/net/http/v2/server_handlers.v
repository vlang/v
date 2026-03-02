// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net

// write_settings sends the server's initial SETTINGS frame with a pre-allocated buffer.
fn (mut s Server) write_settings(mut conn net.TcpConn) ! {
	// Pre-allocate payload with exact size (3 settings * 6 bytes = 18 bytes)
	mut payload := []u8{cap: 18}

	// SETTINGS_MAX_CONCURRENT_STREAMS (0x03)
	payload << [u8(0), u8(3)]
	payload << [u8(s.config.max_concurrent_streams >> 24), u8(s.config.max_concurrent_streams >> 16),
		u8(s.config.max_concurrent_streams >> 8), u8(s.config.max_concurrent_streams)]

	// SETTINGS_INITIAL_WINDOW_SIZE (0x04)
	payload << [u8(0), u8(4)]
	payload << [u8(s.config.initial_window_size >> 24), u8(s.config.initial_window_size >> 16),
		u8(s.config.initial_window_size >> 8), u8(s.config.initial_window_size)]

	// SETTINGS_MAX_FRAME_SIZE (0x05)
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

// handle_settings processes a SETTINGS frame from the client.
// If the frame is an ACK, it is silently accepted.
// Otherwise, each setting key-value pair is parsed and stored in client_settings,
// and a SETTINGS ACK is sent back per RFC 7540 §6.5.
fn (mut s Server) handle_settings(mut conn net.TcpConn, frame Frame, mut client_settings ClientSettings) ! {
	// Check for ACK
	if frame.header.flags & u8(FrameFlags.ack) != 0 {
		$if debug {
			eprintln('[HTTP/2] Received SETTINGS ACK')
		}
		return
	}

	$if debug {
		eprintln('[HTTP/2] Received SETTINGS')
	}

	// Parse and apply client settings (each setting is 6 bytes: 2-byte ID + 4-byte value)
	payload := frame.payload
	mut i := 0
	for i + 6 <= payload.len {
		id := (u16(payload[i]) << 8) | u16(payload[i + 1])
		val := (u32(payload[i + 2]) << 24) | (u32(payload[i + 3]) << 16) | (u32(payload[i + 4]) << 8) | u32(payload[
			i + 5])
		i += 6

		match id {
			u16(SettingId.header_table_size) {
				client_settings.header_table_size = val
			}
			u16(SettingId.max_concurrent_streams) {
				client_settings.max_concurrent_streams = val
			}
			u16(SettingId.initial_window_size) {
				client_settings.initial_window_size = val
			}
			u16(SettingId.max_frame_size) {
				client_settings.max_frame_size = val
			}
			u16(SettingId.max_header_list_size) {
				client_settings.max_header_list_size = val
			}
			else {
				// Unknown setting identifiers must be ignored per RFC 7540 §6.5
			}
		}
	}

	// Send SETTINGS ACK
	ack := Frame{
		header:  FrameHeader{
			length:     0
			frame_type: .settings
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: []u8{}
	}

	s.write_frame(mut conn, ack)!
	$if debug {
		eprintln('[HTTP/2] Sent SETTINGS ACK')
	}
}

// handle_headers processes a HEADERS frame, decodes the header block,
// invokes the request handler, and sends the response.
fn (mut s Server) handle_headers(mut conn net.TcpConn, frame Frame, mut decoder Decoder, mut encoder Encoder) ! {
	stream_id := frame.header.stream_id

	if stream_id == 0 {
		return error('HEADERS on stream 0')
	}

	// Decode headers
	headers := decoder.decode(frame.payload)!

	// Build request
	mut method := ''
	mut path := ''
	mut header_map := map[string]string{}

	for h in headers {
		match h.name {
			':method' { method = h.value }
			':path' { path = h.value }
			else { header_map[h.name] = h.value }
		}
	}

	request := ServerRequest{
		method:    method
		path:      path
		headers:   header_map
		body:      []u8{}
		stream_id: stream_id
	}

	$if debug {
		eprintln('[HTTP/2] Request: ${method} ${path}')
	}

	// Call handler
	h := s.handler or {
		error_response := ServerResponse{
			status_code: 500
			headers:     {
				'content-type': 'text/plain'
			}
			body:        'no handler configured'.bytes()
		}
		s.send_response(mut conn, stream_id, error_response, mut encoder) or {
			eprintln('[HTTP/2] Failed to send error response: ${err}')
		}
		return
	}

	response := h(request)

	// Send response
	s.send_response(mut conn, stream_id, response, mut encoder)!
}

// send_response encodes and sends an HTTP/2 response (HEADERS + optional DATA frame).
fn (mut s Server) send_response(mut conn net.TcpConn, stream_id u32, response ServerResponse, mut encoder Encoder) ! {
	// Pre-allocate headers array with capacity
	mut resp_headers := []HeaderField{cap: 2 + response.headers.len}
	resp_headers << HeaderField{
		name:  ':status'
		value: response.status_code.str()
	}

	for key, value in response.headers {
		resp_headers << HeaderField{
			name:  key
			value: value
		}
	}

	if response.body.len > 0 && 'content-length' !in response.headers {
		resp_headers << HeaderField{
			name:  'content-length'
			value: response.body.len.str()
		}
	}

	// Encode headers
	encoded := encoder.encode(resp_headers)

	// Send HEADERS frame
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

	// Send DATA frame if needed
	if response.body.len > 0 {
		data_frame := Frame{
			header:  FrameHeader{
				length:     u32(response.body.len)
				frame_type: .data
				flags:      u8(FrameFlags.end_stream)
				stream_id:  stream_id
			}
			payload: response.body
		}

		s.write_frame(mut conn, data_frame)!
	}

	$if debug {
		eprintln('[HTTP/2] Response sent: ${response.status_code} (${response.body.len} bytes)')
	}
}

// handle_ping processes a PING frame and sends a PING ACK per RFC 7540 §6.7.
fn (mut s Server) handle_ping(mut conn net.TcpConn, frame Frame) ! {
	// Send PING ACK
	ack := Frame{
		header:  FrameHeader{
			length:     frame.header.length
			frame_type: .ping
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: frame.payload.clone()
	}

	s.write_frame(mut conn, ack)!
	$if debug {
		eprintln('[HTTP/2] PING/PONG')
	}
}

// read_frame reads a complete HTTP/2 frame from the connection.
// Uses read_exact to ensure all bytes of both the 9-byte header and
// the variable-length payload are fully received before returning.
fn (mut s Server) read_frame(mut conn net.TcpConn) !Frame {
	// Read header (9 bytes) — use read_exact to handle partial TCP reads
	mut header_buf := []u8{len: frame_header_size}
	conn.set_read_timeout(s.config.read_timeout)

	read_exact_tcp(mut conn, mut header_buf, frame_header_size) or {
		return error('read header: ${err}')
	}

	header := parse_frame_header(header_buf) or {
		// Per RFC 7540 §4.1: unknown frame type — discard this frame
		return error('unknown frame type, frame discarded')
	}

	// Read payload — use read_exact to handle partial TCP reads
	mut payload := []u8{len: int(header.length)}
	if header.length > 0 {
		read_exact_tcp(mut conn, mut payload, int(header.length)) or {
			return error('read payload: ${err}')
		}
	}

	return Frame{
		header:  header
		payload: payload
	}
}

// write_frame encodes a frame and writes it to the connection.
fn (mut s Server) write_frame(mut conn net.TcpConn, frame Frame) ! {
	data := encode_frame(frame)
	conn.set_write_timeout(s.config.write_timeout)
	conn.write(data)!
}

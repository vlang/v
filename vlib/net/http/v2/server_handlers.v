// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net
import time

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

	// Parse and apply client settings using shared parser
	pairs := parse_settings_payload(frame.payload)!

	for pair in pairs {
		apply_setting_pair(pair, mut client_settings)
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

// apply_setting_pair applies a single settings key-value pair to client settings.
fn apply_setting_pair(pair SettingPair, mut settings ClientSettings) {
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

// build_response_headers assembles the response header fields including the
// :status pseudo-header, user-provided headers, and content-length if needed.
fn build_response_headers(response ServerResponse) []HeaderField {
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
	return resp_headers
}

// send_response encodes and sends an HTTP/2 response (HEADERS + optional DATA frame).
fn (mut s Server) send_response(mut conn net.TcpConn, stream_id u32, response ServerResponse, mut encoder Encoder) ! {
	resp_headers := build_response_headers(response)

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

// handle_priority processes a PRIORITY frame per RFC 7540 §6.3.
// The frame is parsed and logged (if trace is enabled) but priority scheduling
// is not implemented. RFC 7540 requires PRIORITY frames to be accepted.
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

// send_rst_stream sends an RST_STREAM frame with the given error code.
fn send_rst_stream(mut conn net.TcpConn, stream_id u32, error_code ErrorCode) ! {
	rst := RstStreamFrame{
		stream_id:  stream_id
		error_code: error_code
	}
	frame_bytes := rst.to_frame().encode()
	conn.write(frame_bytes)!
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
	data := frame.encode()
	conn.set_write_timeout(s.config.write_timeout)
	conn.write(data)!
}

// send_window_update_tcp sends a WINDOW_UPDATE frame over a plain TCP connection.
// Used by the server to replenish the peer's flow control window (RFC 7540 §6.9).
fn send_window_update_tcp(mut conn net.TcpConn, write_timeout time.Duration, stream_id u32, increment u32) ! {
	payload := [
		u8(increment >> 24) & 0x7f,
		u8(increment >> 16),
		u8(increment >> 8),
		u8(increment),
	]
	frame := Frame{
		header:  FrameHeader{
			length:     4
			frame_type: .window_update
			flags:      0
			stream_id:  stream_id
		}
		payload: payload
	}
	data := frame.encode()
	conn.set_write_timeout(write_timeout)
	conn.write(data)!
}

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net.ssl

// Connection represents an HTTP/2 connection with full duplex streaming over TLS
pub struct Connection {
mut:
	ssl_conn           &ssl.SSLConn = unsafe { nil }
	encoder            Encoder
	decoder            Decoder
	streams            map[u32]&Stream
	next_stream_id     u32 = 1
	settings           Settings
	remote_settings    Settings
	window_size        i64 = 65535
	remote_window_size i64 = 65535
	last_stream_id     u32
	closed             bool
	// Flow control: track inbound data to replenish the receive window
	recv_window          i64 = 65535
	recv_window_consumed i64
}

// write_settings sends a SETTINGS frame to configure connection parameters
pub fn (mut c Connection) write_settings() ! {
	// Pre-allocate payload with exact size (5 settings * 6 bytes each = 30 bytes)
	mut payload := []u8{cap: 30}

	// Helper function to encode a setting
	encode_setting := fn (mut payload []u8, id SettingId, value u32) {
		payload << u8(u16(id) >> 8)
		payload << u8(u16(id))
		payload << u8(value >> 24)
		payload << u8(value >> 16)
		payload << u8(value >> 8)
		payload << u8(value)
	}

	// Encode each setting
	encode_setting(mut payload, .header_table_size, c.settings.header_table_size)
	encode_setting(mut payload, .enable_push, if c.settings.enable_push { u32(1) } else { u32(0) })
	encode_setting(mut payload, .max_concurrent_streams, c.settings.max_concurrent_streams)
	encode_setting(mut payload, .initial_window_size, c.settings.initial_window_size)
	encode_setting(mut payload, .max_frame_size, c.settings.max_frame_size)

	frame := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .settings
			flags:      0
			stream_id:  0
		}
		payload: payload
	}

	c.write_frame(frame)!
}

// read_settings reads and processes a SETTINGS frame from the server,
// skipping over non-SETTINGS frames (e.g. WINDOW_UPDATE) that may precede it.
pub fn (mut c Connection) read_settings() ! {
	// Limit the number of frames to read before receiving SETTINGS
	// to prevent infinite loop if the server never sends one.
	max_frames := 10
	for frame_count := 0; frame_count < max_frames; frame_count++ {
		frame := c.read_frame()!

		match frame.header.frame_type {
			.settings {
				if frame.header.has_flag(.ack) {
					return
				}
				pairs := parse_settings_payload(frame.payload)!
				c.apply_remote_settings(pairs)
				c.write_settings_ack()!
				return
			}
			.window_update {
				// WINDOW_UPDATE on stream 0 adjusts the connection window
				if frame.header.stream_id == 0 && frame.payload.len >= 4 {
					increment := (u32(frame.payload[0]) << 24) | (u32(frame.payload[1]) << 16) | (u32(frame.payload[2]) << 8) | u32(frame.payload[3])
					c.remote_window_size += i64(increment & 0x7fffffff)
				}
				continue
			}
			.goaway {
				return error(extract_goaway_error(frame.payload))
			}
			else {
				// Skip unexpected frames during setup
				continue
			}
		}
	}
	return error('did not receive SETTINGS frame within ${max_frames} frames')
}

// apply_remote_settings applies received setting pairs to the connection's remote settings.
fn (mut c Connection) apply_remote_settings(pairs []SettingPair) {
	for pair in pairs {
		match pair.id {
			.header_table_size { c.remote_settings.header_table_size = pair.value }
			.enable_push { c.remote_settings.enable_push = pair.value != 0 }
			.max_concurrent_streams { c.remote_settings.max_concurrent_streams = pair.value }
			.initial_window_size { c.remote_settings.initial_window_size = pair.value }
			.max_frame_size { c.remote_settings.max_frame_size = pair.value }
			.max_header_list_size { c.remote_settings.max_header_list_size = pair.value }
		}
	}
}

// write_settings_ack sends a SETTINGS ACK frame to acknowledge received settings.
fn (mut c Connection) write_settings_ack() ! {
	ack_frame := Frame{
		header:  FrameHeader{
			length:     0
			frame_type: .settings
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: []u8{}
	}
	c.write_frame(ack_frame)!
}

// extract_goaway_error builds an error message from a GOAWAY frame payload.
fn extract_goaway_error(payload []u8) string {
	mut error_code := u32(0)
	if payload.len >= 8 {
		error_code = (u32(payload[4]) << 24) | (u32(payload[5]) << 16) | (u32(payload[6]) << 8) | u32(payload[7])
	}
	debug_data := if payload.len > 8 {
		payload[8..].bytestr()
	} else {
		''
	}
	return 'server sent GOAWAY (error code: ${error_code}, debug: ${debug_data})'
}

// write_frame writes an HTTP/2 frame to the TLS connection
pub fn (mut c Connection) write_frame(frame Frame) ! {
	data := frame.encode()
	$if trace_http2 ? {
		eprintln('[HTTP/2] write frame: type=${frame.header.frame_type} len=${frame.header.length} flags=0x${frame.header.flags:02x} stream=${frame.header.stream_id} raw_len=${data.len}')
	}
	c.ssl_conn.write(data)!
}

// read_frame reads an HTTP/2 frame from the TLS connection
pub fn (mut c Connection) read_frame() !Frame {
	mut header_buf := []u8{len: frame_header_size}
	read_exact(mut c.ssl_conn, mut header_buf)!

	header := parse_frame_header(header_buf) or {
		// Per RFC 7540 §4.1: unknown frame type — discard this frame
		return error('unknown frame type, frame discarded')
	}

	mut payload := []u8{len: int(header.length)}
	if header.length > 0 {
		read_exact(mut c.ssl_conn, mut payload)!
	}

	$if trace_http2 ? {
		eprintln('[HTTP/2] read frame: type=${header.frame_type} len=${header.length} flags=0x${header.flags:02x} stream=${header.stream_id}')
	}

	return Frame{
		header:  header
		payload: payload
	}
}

// read_exact reads exactly buf.len bytes from the SSL connection.
// SSL read may return fewer bytes than requested, so this loops until full.
fn read_exact(mut conn ssl.SSLConn, mut buf []u8) ! {
	mut total := 0
	for total < buf.len {
		n := conn.read(mut buf[total..]) or {
			if total == 0 {
				return err
			}
			return error('unexpected EOF after ${total} of ${buf.len} bytes')
		}
		if n == 0 {
			return error('unexpected EOF after ${total} of ${buf.len} bytes')
		}
		total += n
	}
}

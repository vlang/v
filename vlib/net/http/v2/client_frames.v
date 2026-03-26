// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// handle_response_frame dispatches a single frame during response reading.
// Handles both stream-level frames (HEADERS, DATA) and connection-level frames
// (SETTINGS, PING, WINDOW_UPDATE) that may arrive while waiting for the response.
fn (mut c Client) handle_response_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	match frame.header.frame_type {
		.headers {
			c.handle_headers_frame(frame, mut stream, stream_id)!
		}
		.continuation {
			c.handle_continuation_frame(frame, mut stream, stream_id)!
		}
		.data {
			c.handle_data_frame(frame, mut stream, stream_id)!
		}
		.settings {
			// SETTINGS ACK or unsolicited SETTINGS during response
			if !frame.header.has_flag(.ack) {
				c.conn.write_settings_ack()!
			}
		}
		.ping {
			c.handle_ping_frame(frame)!
		}
		.goaway {
			return error('connection closed by server (GOAWAY)')
		}
		.rst_stream {
			c.handle_rst_stream_frame(frame, stream_id)!
		}
		.window_update {
			// Update remote window size when server sends WINDOW_UPDATE (RFC 7540 §6.9)
			c.conn.apply_window_update(frame) or {
				$if trace_http2 ? {
					eprintln('[HTTP/2] failed to apply WINDOW_UPDATE: ${err}')
				}
			}
		}
		.push_promise {
			// RFC 7540 §8.2: A client that has set SETTINGS_ENABLE_PUSH=0
			// MUST treat receipt of a PUSH_PROMISE frame as a connection error.
			return error('received PUSH_PROMISE but push is disabled (RFC 7540 §8.2)')
		}
		else {} // Ignore unknown frame types per RFC 7540
	}
}

// handle_headers_frame processes a HEADERS frame for a stream.
// When END_HEADERS is not set, the raw header block fragment is stored in
// stream.raw_header_block; subsequent CONTINUATION frames will append to it
// until END_HEADERS is seen, at which point the complete block is decoded.
fn (mut c Client) handle_headers_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	hf := HeadersFrame.from_frame(frame)!

	if hf.end_headers {
		headers := c.conn.decoder.decode(hf.headers)!
		stream.headers << headers
		stream.end_headers = true
	} else {
		stream.raw_header_block << hf.headers
	}

	if hf.end_stream {
		stream.end_stream = true
		stream.state = .half_closed_remote
	}
}

// handle_continuation_frame processes CONTINUATION frames that extend a HEADERS block.
// Per RFC 7540 §6.10, CONTINUATION frames must follow a HEADERS frame (or another
// CONTINUATION frame) on the same stream until END_HEADERS is set.
fn (mut c Client) handle_continuation_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	stream.raw_header_block << frame.payload

	if frame.header.has_flag(.end_headers) {
		// All fragments collected — decode the complete header block now.
		headers := c.conn.decoder.decode(stream.raw_header_block)!
		stream.headers << headers
		stream.raw_header_block = []u8{}
		stream.end_headers = true
	}
}

// handle_data_frame processes DATA frame for a stream.
// Tracks consumed bytes and sends WINDOW_UPDATE when the receive window is
// half-depleted to prevent flow control deadlock (RFC 7540 §6.9).
fn (mut c Client) handle_data_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	data_len := i64(frame.payload.len)
	stream.data << frame.payload

	// Update connection-level receive window tracking
	c.conn.recv_window_consumed += data_len

	// Send WINDOW_UPDATE at connection level when half the window is consumed
	threshold := c.conn.recv_window / 2
	if c.conn.recv_window_consumed >= threshold && threshold > 0 {
		increment := u32(c.conn.recv_window_consumed)
		c.conn.send_window_update(0, increment) or {
			// Non-fatal: log and continue
			$if trace_http2 ? {
				eprintln('[HTTP/2] failed to send connection WINDOW_UPDATE: ${err}')
			}
		}
		c.conn.recv_window_consumed = 0
	}

	// Send WINDOW_UPDATE at stream level when half the stream window is consumed
	if data_len > 0 {
		stream_threshold := stream.window_size / 2
		if data_len >= stream_threshold && stream_threshold > 0 {
			c.conn.send_window_update(stream_id, u32(data_len)) or {
				$if trace_http2 ? {
					eprintln('[HTTP/2] failed to send stream WINDOW_UPDATE: ${err}')
				}
			}
		}
	}

	if frame.header.has_flag(.end_stream) {
		stream.end_stream = true
		stream.state = .closed
	}
}

// handle_ping_frame responds to PING frame with ACK.
fn (mut c Client) handle_ping_frame(frame Frame) ! {
	pf := PingFrame.from_frame(frame)!
	ack_pf := PingFrame{
		ack:  true
		data: pf.data
	}
	c.conn.write_frame(ack_pf.to_frame())!
}

// handle_rst_stream_frame handles RST_STREAM frame.
fn (c Client) handle_rst_stream_frame(frame Frame, stream_id u32) ! {
	if frame.header.stream_id == stream_id {
		rf := RstStreamFrame.from_frame(frame)!
		return error('stream reset by server (RST_STREAM, error_code=${rf.error_code})')
	}
}

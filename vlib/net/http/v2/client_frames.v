// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// handle_headers_frame processes a HEADERS frame for a stream.
// When END_HEADERS is not set, the raw header block fragment is stored in
// stream.raw_header_block; subsequent CONTINUATION frames will append to it
// until END_HEADERS is seen, at which point the complete block is decoded.
fn (mut c Client) handle_headers_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	if frame.header.has_flag(.end_headers) {
		// All header data is present in this single frame — decode immediately.
		headers := c.conn.decoder.decode(frame.payload)!
		stream.headers << headers
		stream.end_headers = true
	} else {
		// Header block is fragmented across CONTINUATION frames; accumulate raw bytes.
		stream.raw_header_block << frame.payload
	}

	if frame.header.has_flag(.end_stream) {
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

// handle_ping_frame responds to PING frame with ACK
fn (mut c Client) handle_ping_frame(frame Frame) ! {
	pong := Frame{
		header:  FrameHeader{
			length:     u32(frame.payload.len)
			frame_type: .ping
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: frame.payload
	}
	c.conn.write_frame(pong)!
}

// handle_rst_stream_frame handles RST_STREAM frame
fn (c Client) handle_rst_stream_frame(frame Frame, stream_id u32) ! {
	if frame.header.stream_id == stream_id {
		return error('stream reset by server (RST_STREAM)')
	}
}

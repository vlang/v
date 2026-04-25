module v2

// Client-side frame dispatch during response reading.

fn (mut c Client) handle_response_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id == stream_id {
		// RFC 7540 §5.1: enforce stream state only for known frame types.
		// Unknown frame types are silently ignored per RFC 7540 §5.5.
		if frame_type_from_byte(u8(frame.header.frame_type)) != none {
			if !stream.state.can_recv(frame.header.frame_type) {
				return error('PROTOCOL_ERROR: received ${frame.header.frame_type} in state ${stream.state}')
			}
		}
	}

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
			if !frame.header.has_flag(.ack) {
				pairs := parse_settings_payload(frame.payload)!
				c.conn.apply_remote_settings(pairs)!
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
			c.conn.apply_window_update(frame) or {
				$if trace_http2 ? {
					eprintln('[HTTP/2] failed to apply WINDOW_UPDATE: ${err}')
				}
			}
		}
		.push_promise {
			// Per RFC 7540 §8.2: Client sends ENABLE_PUSH=0, server MUST NOT send PUSH_PROMISE.
			// Server push is intentionally not implemented; receiving PUSH_PROMISE is a protocol violation.
			return error('PROTOCOL_ERROR: received PUSH_PROMISE but ENABLE_PUSH is disabled (RFC 7540 §8.2)')
		}
		else {}
	}
}

fn (mut c Client) handle_headers_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	hf := HeadersFrame.from_frame(frame)!

	if hf.end_headers {
		headers := c.conn.decoder.decode(hf.headers)!
		validate_response_headers(headers)!
		stream.headers << headers
		stream.end_headers = true
	} else {
		stream.raw_header_block << hf.headers
	}

	stream.state = stream.state.next_on_recv(.headers, hf.end_stream)
	if hf.end_stream {
		stream.end_stream = true
	}
}

fn (mut c Client) handle_continuation_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	stream.continuation_count++
	if stream.continuation_count > max_continuation_frames {
		return error('ENHANCE_YOUR_CALM: exceeded ${max_continuation_frames} CONTINUATION frames')
	}
	if stream.raw_header_block.len + frame.payload.len > max_header_block_size {
		return error('ENHANCE_YOUR_CALM: header block size exceeds ${max_header_block_size} bytes')
	}

	stream.raw_header_block << frame.payload

	if frame.header.has_flag(.end_headers) {
		headers := c.conn.decoder.decode(stream.raw_header_block)!
		validate_response_headers(headers)!
		stream.headers << headers
		stream.raw_header_block = []u8{}
		stream.continuation_count = 0
		stream.end_headers = true
	}
}

fn (mut c Client) handle_data_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	data_len := i64(frame.payload.len)
	stream.data << frame.payload

	c.conn.recv_window_consumed += data_len

	threshold := c.conn.recv_window / 2
	if c.conn.recv_window_consumed >= threshold && threshold > 0 {
		increment := u32(c.conn.recv_window_consumed)
		c.conn.send_window_update(0, increment) or {
			$if trace_http2 ? {
				eprintln('[HTTP/2] failed to send connection WINDOW_UPDATE: ${err}')
			}
		}
		c.conn.recv_window_consumed = 0
	}

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
		stream.state = stream.state.next_on_recv(.data, true)
	}
}

fn (mut c Client) handle_ping_frame(frame Frame) ! {
	pf := PingFrame.from_frame(frame)!
	ack_pf := PingFrame{
		ack:  true
		data: pf.data
	}
	c.conn.write_frame(ack_pf.to_frame())!
}

fn (c Client) handle_rst_stream_frame(frame Frame, stream_id u32) ! {
	if frame.header.stream_id == stream_id {
		rf := RstStreamFrame.from_frame(frame)!
		return error('stream reset by server (RST_STREAM, error_code=${rf.error_code})')
	}
}

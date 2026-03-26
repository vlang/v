module v2

// Client-side frame dispatch during response reading.

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
			c.conn.apply_window_update(frame) or {
				$if trace_http2 ? {
					eprintln('[HTTP/2] failed to apply WINDOW_UPDATE: ${err}')
				}
			}
		}
		.push_promise {
			return error('received PUSH_PROMISE but push is disabled (RFC 7540 §8.2)')
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

fn (mut c Client) handle_continuation_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	stream.raw_header_block << frame.payload

	if frame.header.has_flag(.end_headers) {
		headers := c.conn.decoder.decode(stream.raw_header_block)!
		stream.headers << headers
		stream.raw_header_block = []u8{}
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
		stream.state = .closed
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

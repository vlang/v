module v2

// HTTP/2 flow control: WINDOW_UPDATE sending, applying, and data splitting (RFC 7540 §6.9).

// send_window_update sends a WINDOW_UPDATE frame to increment the receive window.
pub fn (mut c Connection) send_window_update(stream_id u32, increment u32) ! {
	c.write_frame(new_window_update_frame(stream_id, increment))!
}

// apply_window_update parses a WINDOW_UPDATE frame and updates the remote window size.
pub fn (mut c Connection) apply_window_update(frame Frame) ! {
	wf := WindowUpdateFrame.from_frame(frame)!
	if wf.window_increment == 0 {
		return error('PROTOCOL_ERROR: WINDOW_UPDATE increment must not be 0 (RFC 7540 §6.9.1)')
	}
	if wf.stream_id == 0 {
		new_size := c.remote_window_size + i64(wf.window_increment)
		if new_size > 0x7fffffff {
			return error('FLOW_CONTROL_ERROR: connection window size exceeds 2^31-1 (RFC 7540 §6.9.1)')
		}
		c.remote_window_size = new_size
	} else {
		if mut stream := c.streams[wf.stream_id] {
			new_size := stream.window_size + i64(wf.window_increment)
			if new_size > 0x7fffffff {
				return error('FLOW_CONTROL_ERROR: stream window size exceeds 2^31-1 (RFC 7540 §6.9.1)')
			}
			stream.window_size = new_size
		}
	}
}

fn split_data_for_window(data []u8, window i64, max_frame_size u32) [][]u8 {
	if data.len == 0 || window <= 0 {
		return [][]u8{}
	}
	chunk_limit := if i64(max_frame_size) < window { int(max_frame_size) } else { int(window) }
	mut chunks := [][]u8{cap: (data.len + chunk_limit - 1) / chunk_limit}
	mut offset := 0
	for offset < data.len {
		end := if offset + chunk_limit > data.len { data.len } else { offset + chunk_limit }
		chunks << data[offset..end]
		offset = end
	}
	return chunks
}

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// send_window_update sends a WINDOW_UPDATE frame to increment the receive window.
// stream_id=0 applies to the connection level; a non-zero stream_id applies to
// the specified stream (RFC 7540 §6.9).
pub fn (mut c Connection) send_window_update(stream_id u32, increment u32) ! {
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
	c.write_frame(frame)!
}

// apply_window_update parses a WINDOW_UPDATE frame and updates the remote window size.
// For stream_id=0, updates the connection-level remote_window_size.
// For a specific stream_id, updates the stream's window_size (RFC 7540 §6.9.1).
// Returns PROTOCOL_ERROR if increment is 0, FLOW_CONTROL_ERROR if the new window
// exceeds 2^31-1 (RFC 7540 §6.9.1).
pub fn (mut c Connection) apply_window_update(frame Frame) ! {
	wf := WindowUpdateFrame.from_frame(frame)!
	// RFC 7540 §6.9.1: A receiver MUST treat a WINDOW_UPDATE with increment 0
	// as a connection error of type PROTOCOL_ERROR (stream 0) or stream error.
	if wf.window_increment == 0 {
		return error('PROTOCOL_ERROR: WINDOW_UPDATE increment must not be 0 (RFC 7540 §6.9.1)')
	}
	if wf.stream_id == 0 {
		new_size := c.remote_window_size + i64(wf.window_increment)
		// RFC 7540 §6.9.1: window size cannot exceed 2^31-1
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

// split_data_for_window splits data into chunks that fit within the flow control window
// and max frame size per RFC 7540 §6.9. Returns empty if window is zero.
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

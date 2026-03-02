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

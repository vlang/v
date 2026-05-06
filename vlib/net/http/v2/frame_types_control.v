module v2

// Control frame type definitions: GoAway, WindowUpdate, Priority, RstStream.

// error_code_from_u32 converts a raw u32 to an ErrorCode enum value.
pub fn error_code_from_u32(code u32) ErrorCode {
	return match code {
		0x0 { .no_error }
		0x1 { .protocol_error }
		0x2 { .internal_error }
		0x3 { .flow_control_error }
		0x4 { .settings_timeout }
		0x5 { .stream_closed }
		0x6 { .frame_size_error }
		0x7 { .refused_stream }
		0x8 { .cancel }
		0x9 { .compression_error }
		0xa { .connect_error }
		0xb { .enhance_your_calm }
		0xc { .inadequate_security }
		0xd { .http_1_1_required }
		else { .internal_error }
	}
}

// GoAwayFrame represents the payload of an HTTP/2 GOAWAY frame (RFC 7540 §6.8).
pub struct GoAwayFrame {
pub mut:
	last_stream_id u32
	error_code     ErrorCode
	debug_data     []u8
}

// from_frame converts a generic Frame to a GoAwayFrame.
pub fn GoAwayFrame.from_frame(f Frame) !GoAwayFrame {
	if f.header.frame_type != .goaway {
		return error('expected GOAWAY frame, got ${f.header.frame_type}')
	}
	if f.payload.len < 8 {
		return error('GOAWAY frame payload must be at least 8 bytes, got ${f.payload.len}')
	}
	last_stream_id := ((u32(f.payload[0]) << 24) | (u32(f.payload[1]) << 16) | (u32(f.payload[2]) << 8) | u32(f.payload[3])) & 0x7fffffff
	raw_error := (u32(f.payload[4]) << 24) | (u32(f.payload[5]) << 16) | (u32(f.payload[6]) << 8) | u32(f.payload[7])
	debug_data := if f.payload.len > 8 { f.payload[8..] } else { []u8{} }
	return GoAwayFrame{
		last_stream_id: last_stream_id
		error_code:     error_code_from_u32(raw_error)
		debug_data:     debug_data
	}
}

// to_frame converts a GoAwayFrame back to a generic Frame.
pub fn (gf GoAwayFrame) to_frame() Frame {
	mut payload := []u8{cap: 8 + gf.debug_data.len}
	payload << u8((gf.last_stream_id >> 24) & 0x7f)
	payload << u8(gf.last_stream_id >> 16)
	payload << u8(gf.last_stream_id >> 8)
	payload << u8(gf.last_stream_id)
	ec := u32(gf.error_code)
	payload << u8(ec >> 24)
	payload << u8(ec >> 16)
	payload << u8(ec >> 8)
	payload << u8(ec)
	payload << gf.debug_data
	return Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .goaway
			flags:      0
			stream_id:  0
		}
		payload: payload
	}
}

// WindowUpdateFrame represents the payload of an HTTP/2 WINDOW_UPDATE frame (RFC 7540 §6.9).
pub struct WindowUpdateFrame {
pub mut:
	stream_id        u32
	window_increment u32
}

// from_frame converts a generic Frame to a WindowUpdateFrame.
pub fn WindowUpdateFrame.from_frame(f Frame) !WindowUpdateFrame {
	if f.header.frame_type != .window_update {
		return error('expected WINDOW_UPDATE frame, got ${f.header.frame_type}')
	}
	if f.payload.len != 4 {
		return error('WINDOW_UPDATE frame payload must be 4 bytes, got ${f.payload.len}')
	}
	increment := ((u32(f.payload[0]) << 24) | (u32(f.payload[1]) << 16) | (u32(f.payload[2]) << 8) | u32(f.payload[3])) & 0x7fffffff
	return WindowUpdateFrame{
		stream_id:        f.header.stream_id
		window_increment: increment
	}
}

// to_frame converts a WindowUpdateFrame back to a generic Frame.
pub fn (wf WindowUpdateFrame) to_frame() Frame {
	mut payload := []u8{len: 4}
	payload[0] = u8((wf.window_increment >> 24) & 0x7f)
	payload[1] = u8(wf.window_increment >> 16)
	payload[2] = u8(wf.window_increment >> 8)
	payload[3] = u8(wf.window_increment)
	return Frame{
		header:  FrameHeader{
			length:     4
			frame_type: .window_update
			flags:      0
			stream_id:  wf.stream_id
		}
		payload: payload
	}
}

// new_window_update_frame creates a WINDOW_UPDATE frame for the given stream and increment.
pub fn new_window_update_frame(stream_id u32, increment u32) Frame {
	mut payload := []u8{len: 4}
	payload[0] = u8((increment >> 24) & 0x7f)
	payload[1] = u8((increment >> 16) & 0xff)
	payload[2] = u8((increment >> 8) & 0xff)
	payload[3] = u8(increment & 0xff)
	return Frame{
		header:  FrameHeader{
			length:     4
			frame_type: .window_update
			flags:      0
			stream_id:  stream_id
		}
		payload: payload
	}
}

// PriorityFrame represents an HTTP/2 PRIORITY frame per RFC 7540 §6.3.
// This implementation parses priority but does not use it for scheduling;
// priority is advisory per RFC 7540 §5.3 and requests are dispatched in arrival order.
pub struct PriorityFrame {
pub mut:
	stream_id         u32
	exclusive         bool
	stream_dependency u32
	weight            u8
}

// from_frame converts a generic Frame to a PriorityFrame.
pub fn PriorityFrame.from_frame(f Frame) !PriorityFrame {
	if f.header.frame_type != .priority {
		return error('expected PRIORITY frame, got ${f.header.frame_type}')
	}
	if f.payload.len != 5 {
		return error('PRIORITY frame payload must be 5 bytes, got ${f.payload.len}')
	}
	raw_dep := (u32(f.payload[0]) << 24) | (u32(f.payload[1]) << 16) | (u32(f.payload[2]) << 8) | u32(f.payload[3])
	return PriorityFrame{
		stream_id:         f.header.stream_id
		exclusive:         (raw_dep & 0x80000000) != 0
		stream_dependency: raw_dep & 0x7fffffff
		weight:            f.payload[4]
	}
}

// to_frame converts a PriorityFrame back to a generic Frame.
pub fn (pf PriorityFrame) to_frame() Frame {
	mut raw_dep := pf.stream_dependency & 0x7fffffff
	if pf.exclusive {
		raw_dep |= 0x80000000
	}
	payload := [u8(raw_dep >> 24), u8(raw_dep >> 16), u8(raw_dep >> 8), u8(raw_dep), pf.weight]
	return Frame{
		header:  FrameHeader{
			length:     5
			frame_type: .priority
			flags:      0
			stream_id:  pf.stream_id
		}
		payload: payload
	}
}

// RstStreamFrame represents the payload of an HTTP/2 RST_STREAM frame (RFC 7540 §6.4).
pub struct RstStreamFrame {
pub mut:
	stream_id  u32
	error_code ErrorCode
}

// from_frame converts a generic Frame to a RstStreamFrame.
pub fn RstStreamFrame.from_frame(f Frame) !RstStreamFrame {
	if f.header.frame_type != .rst_stream {
		return error('expected RST_STREAM frame, got ${f.header.frame_type}')
	}
	if f.payload.len != 4 {
		return error('RST_STREAM frame payload must be 4 bytes, got ${f.payload.len}')
	}
	raw_error := (u32(f.payload[0]) << 24) | (u32(f.payload[1]) << 16) | (u32(f.payload[2]) << 8) | u32(f.payload[3])
	return RstStreamFrame{
		stream_id:  f.header.stream_id
		error_code: error_code_from_u32(raw_error)
	}
}

// to_frame converts a RstStreamFrame back to a generic Frame.
pub fn (rf RstStreamFrame) to_frame() Frame {
	ec := u32(rf.error_code)
	payload := [u8(ec >> 24), u8(ec >> 16), u8(ec >> 8), u8(ec)]
	return Frame{
		header:  FrameHeader{
			length:     4
			frame_type: .rst_stream
			flags:      0
			stream_id:  rf.stream_id
		}
		payload: payload
	}
}

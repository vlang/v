// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Structured frame type definitions and their conversion methods.

// error_code_from_u32 converts a raw u32 to an ErrorCode enum value.
// Unknown error codes default to internal_error.
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

// DataFrame represents the payload of an HTTP/2 DATA frame (RFC 7540 §6.1).
pub struct DataFrame {
pub mut:
	stream_id  u32
	data       []u8
	end_stream bool
	padded     bool
	pad_length u8
}

// from_frame converts a generic Frame to a DataFrame.
pub fn DataFrame.from_frame(f Frame) !DataFrame {
	if f.header.frame_type != .data {
		return error('expected DATA frame, got ${f.header.frame_type}')
	}
	return DataFrame{
		stream_id:  f.header.stream_id
		data:       f.payload
		end_stream: f.header.has_flag(.end_stream)
		padded:     f.header.has_flag(.padded)
	}
}

// to_frame converts a DataFrame back to a generic Frame.
pub fn (df DataFrame) to_frame() Frame {
	mut flags := u8(0)
	if df.end_stream {
		flags |= u8(FrameFlags.end_stream)
	}
	if df.padded {
		flags |= u8(FrameFlags.padded)
	}
	return Frame{
		header:  FrameHeader{
			length:     u32(df.data.len)
			frame_type: .data
			flags:      flags
			stream_id:  df.stream_id
		}
		payload: df.data
	}
}

// HeadersFrame represents the payload of an HTTP/2 HEADERS frame (RFC 7540 §6.2).
pub struct HeadersFrame {
pub mut:
	stream_id   u32
	headers     []u8 // Encoded header block
	end_stream  bool
	end_headers bool
	padded      bool
	priority    bool
	pad_length  u8
	stream_dep  u32
	weight      u8
	exclusive   bool
}

// from_frame converts a generic Frame to a HeadersFrame.
pub fn HeadersFrame.from_frame(f Frame) !HeadersFrame {
	if f.header.frame_type != .headers {
		return error('expected HEADERS frame, got ${f.header.frame_type}')
	}
	return HeadersFrame{
		stream_id:   f.header.stream_id
		headers:     f.payload
		end_stream:  f.header.has_flag(.end_stream)
		end_headers: f.header.has_flag(.end_headers)
		padded:      f.header.has_flag(.padded)
		priority:    f.header.has_flag(.priority_flag)
	}
}

// to_frame converts a HeadersFrame back to a generic Frame.
pub fn (hf HeadersFrame) to_frame() Frame {
	mut flags := u8(0)
	if hf.end_stream {
		flags |= u8(FrameFlags.end_stream)
	}
	if hf.end_headers {
		flags |= u8(FrameFlags.end_headers)
	}
	if hf.padded {
		flags |= u8(FrameFlags.padded)
	}
	if hf.priority {
		flags |= u8(FrameFlags.priority_flag)
	}
	return Frame{
		header:  FrameHeader{
			length:     u32(hf.headers.len)
			frame_type: .headers
			flags:      flags
			stream_id:  hf.stream_id
		}
		payload: hf.headers
	}
}

// SettingsFrame represents the payload of an HTTP/2 SETTINGS frame (RFC 7540 §6.5).
pub struct SettingsFrame {
pub mut:
	ack      bool
	settings map[u16]u32
}

// from_frame converts a generic Frame to a SettingsFrame.
pub fn SettingsFrame.from_frame(f Frame) !SettingsFrame {
	if f.header.frame_type != .settings {
		return error('expected SETTINGS frame, got ${f.header.frame_type}')
	}
	is_ack := f.header.has_flag(.ack)
	mut settings := map[u16]u32{}
	if !is_ack {
		pairs := parse_settings_payload(f.payload)!
		for pair in pairs {
			settings[u16(pair.id)] = pair.value
		}
	}
	return SettingsFrame{
		ack:      is_ack
		settings: settings
	}
}

// to_frame converts a SettingsFrame back to a generic Frame.
pub fn (sf SettingsFrame) to_frame() Frame {
	mut payload := []u8{cap: sf.settings.len * 6}
	for id, value in sf.settings {
		payload << u8(id >> 8)
		payload << u8(id)
		payload << u8(value >> 24)
		payload << u8(value >> 16)
		payload << u8(value >> 8)
		payload << u8(value)
	}
	mut flags := u8(0)
	if sf.ack {
		flags = u8(FrameFlags.ack)
	}
	return Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .settings
			flags:      flags
			stream_id:  0
		}
		payload: payload
	}
}

// PingFrame represents the payload of an HTTP/2 PING frame (RFC 7540 §6.7).
pub struct PingFrame {
pub mut:
	ack  bool
	data [8]u8
}

// from_frame converts a generic Frame to a PingFrame.
pub fn PingFrame.from_frame(f Frame) !PingFrame {
	if f.header.frame_type != .ping {
		return error('expected PING frame, got ${f.header.frame_type}')
	}
	if f.payload.len != 8 {
		return error('PING frame payload must be 8 bytes, got ${f.payload.len}')
	}
	mut data := [8]u8{}
	for i in 0 .. 8 {
		data[i] = f.payload[i]
	}
	return PingFrame{
		ack:  f.header.has_flag(.ack)
		data: data
	}
}

// to_frame converts a PingFrame back to a generic Frame.
pub fn (pf PingFrame) to_frame() Frame {
	mut flags := u8(0)
	if pf.ack {
		flags = u8(FrameFlags.ack)
	}
	mut payload := []u8{len: 8}
	for i in 0 .. 8 {
		payload[i] = pf.data[i]
	}
	return Frame{
		header:  FrameHeader{
			length:     8
			frame_type: .ping
			flags:      flags
			stream_id:  0
		}
		payload: payload
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

// PriorityFrame represents the payload of an HTTP/2 PRIORITY frame (RFC 7540 §6.3).
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

// SettingPair holds a single parsed HTTP/2 setting key-value pair from a SETTINGS frame.
pub struct SettingPair {
pub:
	id    SettingId
	value u32
}

// parse_settings_payload parses the 6-byte key-value pairs from a SETTINGS frame payload.
// Unknown setting IDs are silently skipped per RFC 7540 §6.5.2.
pub fn parse_settings_payload(payload []u8) ![]SettingPair {
	if payload.len % 6 != 0 {
		return error('invalid SETTINGS frame: incomplete setting (${payload.len} bytes is not a multiple of 6)')
	}

	mut pairs := []SettingPair{cap: payload.len / 6}
	mut idx := 0
	for idx + 6 <= payload.len {
		raw_id := (u16(payload[idx]) << 8) | u16(payload[idx + 1])
		value := (u32(payload[idx + 2]) << 24) | (u32(payload[idx + 3]) << 16) | (u32(payload[idx +
			4]) << 8) | u32(payload[idx + 5])
		idx += 6

		setting_id := setting_id_from_u16(raw_id) or {
			// Per RFC 7540 §6.5.2, unknown settings must be ignored
			continue
		}
		pairs << SettingPair{
			id:    setting_id
			value: value
		}
	}
	return pairs
}

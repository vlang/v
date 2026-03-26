module v2

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
// Strips padding bytes when the PADDED flag is set per RFC 7540 §6.1.
pub fn DataFrame.from_frame(f Frame) !DataFrame {
	if f.header.frame_type != .data {
		return error('expected DATA frame, got ${f.header.frame_type}')
	}
	if !f.header.has_flag(.padded) {
		return DataFrame{
			stream_id:  f.header.stream_id
			data:       f.payload
			end_stream: f.header.has_flag(.end_stream)
			padded:     false
		}
	}
	return parse_padded_data_frame(f)
}

fn parse_padded_data_frame(f Frame) !DataFrame {
	if f.payload.len < 1 {
		return error('PROTOCOL_ERROR: padded DATA frame has empty payload')
	}
	pad_length := f.payload[0]
	if int(pad_length) >= f.payload.len {
		return error('PROTOCOL_ERROR: pad_length ${pad_length} exceeds payload size ${f.payload.len}')
	}
	data_end := f.payload.len - int(pad_length)
	return DataFrame{
		stream_id:  f.header.stream_id
		data:       f.payload[1..data_end]
		end_stream: f.header.has_flag(.end_stream)
		padded:     true
		pad_length: pad_length
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
	headers     []u8
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
// Strips padding and parses priority fields per RFC 7540 §6.2.
pub fn HeadersFrame.from_frame(f Frame) !HeadersFrame {
	if f.header.frame_type != .headers {
		return error('expected HEADERS frame, got ${f.header.frame_type}')
	}
	is_padded := f.header.has_flag(.padded)
	is_priority := f.header.has_flag(.priority_flag)
	if !is_padded && !is_priority {
		return HeadersFrame{
			stream_id:   f.header.stream_id
			headers:     f.payload
			end_stream:  f.header.has_flag(.end_stream)
			end_headers: f.header.has_flag(.end_headers)
			padded:      false
			priority:    false
		}
	}
	return parse_complex_headers_frame(f, is_padded, is_priority)
}

fn parse_complex_headers_frame(f Frame, is_padded bool, is_priority bool) !HeadersFrame {
	mut offset := 0
	mut pad_length := u8(0)
	if is_padded {
		if f.payload.len < 1 {
			return error('PROTOCOL_ERROR: padded HEADERS frame has empty payload')
		}
		pad_length = f.payload[0]
		offset = 1
	}
	mut exclusive := false
	mut stream_dep := u32(0)
	mut weight := u8(0)
	if is_priority {
		if f.payload.len < offset + 5 {
			return error('PROTOCOL_ERROR: HEADERS frame too short for priority fields')
		}
		raw_dep := (u32(f.payload[offset]) << 24) | (u32(f.payload[offset + 1]) << 16) | (u32(f.payload[
			offset + 2]) << 8) | u32(f.payload[offset + 3])
		exclusive = (raw_dep & 0x80000000) != 0
		stream_dep = raw_dep & 0x7fffffff
		weight = f.payload[offset + 4]
		offset += 5
	}
	data_end := f.payload.len - int(pad_length)
	if data_end < offset {
		return error('PROTOCOL_ERROR: pad_length ${pad_length} exceeds available header block space')
	}
	return HeadersFrame{
		stream_id:   f.header.stream_id
		headers:     f.payload[offset..data_end]
		end_stream:  f.header.has_flag(.end_stream)
		end_headers: f.header.has_flag(.end_headers)
		padded:      is_padded
		priority:    is_priority
		pad_length:  pad_length
		stream_dep:  stream_dep
		weight:      weight
		exclusive:   exclusive
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

// SettingPair holds a single parsed HTTP/2 setting key-value pair.
pub struct SettingPair {
pub:
	id    SettingId
	value u32
}

// parse_settings_payload parses the 6-byte key-value pairs from a SETTINGS frame payload.
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
			// Unknown settings are silently skipped per RFC 7540 §6.5.2
			continue
		}
		pairs << SettingPair{
			id:    setting_id
			value: value
		}
	}
	return pairs
}

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import encoding.binary

// HTTP/2 Frame Types (RFC 7540 Section 6)
pub enum FrameType as u8 {
	data          = 0x0
	headers       = 0x1
	priority      = 0x2
	rst_stream    = 0x3
	settings      = 0x4
	push_promise  = 0x5
	ping          = 0x6
	goaway        = 0x7
	window_update = 0x8
	continuation  = 0x9
}

// HTTP/2 Frame Flags
@[_allow_multiple_values]
pub enum FrameFlags as u8 {
	none          = 0x0
	ack           = 0x1  // SETTINGS, PING
	end_stream    = 0x1  // DATA, HEADERS
	end_headers   = 0x4  // HEADERS, PUSH_PROMISE, CONTINUATION
	padded        = 0x8  // DATA, HEADERS, PUSH_PROMISE
	priority_flag = 0x20 // HEADERS
}

// HTTP/2 Error Codes (RFC 7540 Section 7)
pub enum ErrorCode as u32 {
	no_error            = 0x0
	protocol_error      = 0x1
	internal_error      = 0x2
	flow_control_error  = 0x3
	settings_timeout    = 0x4
	stream_closed       = 0x5
	frame_size_error    = 0x6
	refused_stream      = 0x7
	cancel              = 0x8
	compression_error   = 0x9
	connect_error       = 0xa
	enhance_your_calm   = 0xb
	inadequate_security = 0xc
	http_1_1_required   = 0xd
}

// Frame header constants
pub const frame_header_size = 9
pub const max_frame_size = 16777215 // 2^24 - 1

pub const default_frame_size = 16384 // 16KB

pub const preface = 'PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n'

// FrameHeader represents the 9-byte frame header
pub struct FrameHeader {
pub mut:
	length     u32       // 24-bit length
	frame_type FrameType // 8-bit type
	flags      u8        // 8-bit flags
	stream_id  u32       // 31-bit stream identifier (R bit reserved)
}

// Frame represents a complete HTTP/2 frame
pub struct Frame {
pub mut:
	header  FrameHeader
	payload []u8
}

// parse_frame_header parses the 9-byte frame header
pub fn parse_frame_header(data []u8) !FrameHeader {
	if data.len < frame_header_size {
		return error('frame header too short: ${data.len} bytes')
	}

	// Parse 24-bit length (big-endian)
	length := (u32(data[0]) << 16) | (u32(data[1]) << 8) | u32(data[2])

	// Parse frame type
	frame_type := unsafe { FrameType(data[3]) }

	// Parse flags
	flags := data[4]

	// Parse 31-bit stream ID (ignore reserved bit)
	stream_id := binary.big_endian_u32(data[5..9]) & 0x7fffffff

	return FrameHeader{
		length:     length
		frame_type: frame_type
		flags:      flags
		stream_id:  stream_id
	}
}

// encode encodes the frame header to bytes
pub fn (h FrameHeader) encode() []u8 {
	mut buf := []u8{len: frame_header_size}

	// Encode 24-bit length
	buf[0] = u8(h.length >> 16)
	buf[1] = u8(h.length >> 8)
	buf[2] = u8(h.length)

	// Encode type and flags
	buf[3] = u8(h.frame_type)
	buf[4] = h.flags

	// Encode 31-bit stream ID
	binary.big_endian_put_u32(mut buf[5..9], h.stream_id & 0x7fffffff)

	return buf
}

// has_flag checks if a specific flag is set
pub fn (h FrameHeader) has_flag(flag FrameFlags) bool {
	return (h.flags & u8(flag)) != 0
}

// DataFrame represents an HTTP/2 DATA frame
pub struct DataFrame {
pub mut:
	stream_id  u32
	data       []u8
	end_stream bool
	padded     bool
	pad_length u8
}

// HeadersFrame represents an HTTP/2 HEADERS frame
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

// SettingsFrame represents an HTTP/2 SETTINGS frame
pub struct SettingsFrame {
pub mut:
	ack      bool
	settings map[u16]u32
}

// Settings identifiers (RFC 7540 Section 6.5.2)
pub enum SettingsId as u16 {
	header_table_size      = 0x1
	enable_push            = 0x2
	max_concurrent_streams = 0x3
	initial_window_size    = 0x4
	max_frame_size         = 0x5
	max_header_list_size   = 0x6
}

// PingFrame represents an HTTP/2 PING frame
pub struct PingFrame {
pub mut:
	ack  bool
	data [8]u8
}

// GoAwayFrame represents an HTTP/2 GOAWAY frame
pub struct GoAwayFrame {
pub mut:
	last_stream_id u32
	error_code     ErrorCode
	debug_data     []u8
}

// WindowUpdateFrame represents an HTTP/2 WINDOW_UPDATE frame
pub struct WindowUpdateFrame {
pub mut:
	stream_id        u32
	window_increment u32
}

// RstStreamFrame represents an HTTP/2 RST_STREAM frame
pub struct RstStreamFrame {
pub mut:
	stream_id  u32
	error_code ErrorCode
}

// parse_frame parses a complete frame from bytes
pub fn parse_frame(data []u8) !Frame {
	header := parse_frame_header(data)!

	if data.len < frame_header_size + int(header.length) {
		return error('incomplete frame: expected ${frame_header_size + header.length} bytes, got ${data.len}')
	}

	payload := data[frame_header_size..frame_header_size + header.length]

	return Frame{
		header:  header
		payload: payload
	}
}

// encode encodes a frame to bytes
pub fn (f Frame) encode() []u8 {
	mut buf := f.header.encode()
	buf << f.payload
	return buf
}

// validate validates the frame according to HTTP/2 spec
pub fn (f Frame) validate() ! {
	// Check frame size
	if f.header.length > max_frame_size {
		return error('frame size exceeds maximum: ${f.header.length}')
	}

	// Stream 0 validation
	if f.header.stream_id == 0 {
		match f.header.frame_type {
			.data, .headers, .priority, .rst_stream, .push_promise, .continuation {
				return error('${f.header.frame_type} frame cannot use stream 0')
			}
			else {}
		}
	}

	// Non-zero stream validation
	if f.header.stream_id != 0 {
		match f.header.frame_type {
			.settings, .ping, .goaway {
				return error('${f.header.frame_type} frame must use stream 0')
			}
			else {}
		}
	}
}

// encode_frame encodes a frame to bytes
pub fn encode_frame(frame Frame) []u8 {
	mut result := []u8{len: frame_header_size + frame.payload.len}

	// Encode header (9 bytes)
	result[0] = u8(frame.header.length >> 16)
	result[1] = u8(frame.header.length >> 8)
	result[2] = u8(frame.header.length)
	result[3] = u8(frame.header.frame_type)
	result[4] = frame.header.flags
	result[5] = u8((frame.header.stream_id >> 24) & 0x7f)
	result[6] = u8(frame.header.stream_id >> 16)
	result[7] = u8(frame.header.stream_id >> 8)
	result[8] = u8(frame.header.stream_id)

	// Copy payload using bulk copy
	if frame.payload.len > 0 {
		unsafe {
			vmemcpy(&result[frame_header_size], frame.payload.data, frame.payload.len)
		}
	}

	return result
}

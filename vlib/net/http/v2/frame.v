module v2

// HTTP/2 frame definitions, parsing, encoding, and validation (RFC 7540 §4).
import encoding.binary

// FrameType represents HTTP/2 frame types per RFC 7540 Section 6.
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

// FrameFlags represents HTTP/2 frame flags per RFC 7540 Section 4.1.
@[_allow_multiple_values]
pub enum FrameFlags as u8 {
	none          = 0x0
	ack           = 0x1  // SETTINGS, PING
	end_stream    = 0x1  // DATA, HEADERS
	end_headers   = 0x4  // HEADERS, PUSH_PROMISE, CONTINUATION
	padded        = 0x8  // DATA, HEADERS, PUSH_PROMISE
	priority_flag = 0x20 // HEADERS
}

// ErrorCode represents HTTP/2 error codes per RFC 7540 Section 7.
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

// frame_header_size is the HTTP/2 frame header size in bytes.
pub const frame_header_size = 9

// max_frame_size is the maximum allowed frame size (2^24 - 1).
pub const max_frame_size = 16777215

// default_frame_size is the default maximum frame size (16KB).
pub const default_frame_size = 16384

// max_continuation_frames limits CONTINUATION frames per header block (CVE-2024-27316 mitigation).
pub const max_continuation_frames = 10

// max_header_block_size limits the accumulated header block size in bytes.
pub const max_header_block_size = 65536

// preface is the HTTP/2 connection preface string.
pub const preface = 'PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n'

// FrameHeader represents the 9-byte HTTP/2 frame header.
pub struct FrameHeader {
pub mut:
	length     u32       // 24-bit payload length
	frame_type FrameType // 8-bit frame type
	flags      u8        // 8-bit flags
	stream_id  u32       // 31-bit stream ID (1 bit reserved)
}

// Frame represents a complete HTTP/2 frame with header and payload.
pub struct Frame {
pub mut:
	header  FrameHeader
	payload []u8
}

// SettingId represents setting identifiers per RFC 7540 Section 6.5.2.
pub enum SettingId as u16 {
	header_table_size      = 0x1
	enable_push            = 0x2
	max_concurrent_streams = 0x3
	initial_window_size    = 0x4
	max_frame_size         = 0x5
	max_header_list_size   = 0x6
}

// parse_frame_header parses the 9-byte HTTP/2 frame header from raw bytes.
// Returns none if the data is too short or the frame type is unknown.
pub fn parse_frame_header(data []u8) ?FrameHeader {
	if data.len < frame_header_size {
		return none
	}

	length := (u32(data[0]) << 16) | (u32(data[1]) << 8) | u32(data[2])
	frame_type := frame_type_from_byte(data[3]) or { return none }
	flags := data[4]
	stream_id := binary.big_endian_u32(data[5..9]) & 0x7fffffff

	return FrameHeader{
		length:     length
		frame_type: frame_type
		flags:      flags
		stream_id:  stream_id
	}
}

// encode encodes the frame header to 9 bytes.
pub fn (h FrameHeader) encode() []u8 {
	mut buf := []u8{len: frame_header_size}

	buf[0] = u8(h.length >> 16)
	buf[1] = u8(h.length >> 8)
	buf[2] = u8(h.length)
	buf[3] = u8(h.frame_type)
	buf[4] = h.flags
	binary.big_endian_put_u32(mut buf[5..9], h.stream_id & 0x7fffffff)

	return buf
}

// has_flag checks if a specific flag is set in the frame header.
@[inline]
pub fn (h FrameHeader) has_flag(flag FrameFlags) bool {
	return (h.flags & u8(flag)) != 0
}

// parse_frame parses a complete HTTP/2 frame from raw bytes.
// Returns none if the data is too short or the frame type is unknown.
pub fn parse_frame(data []u8) ?Frame {
	header := parse_frame_header(data) or { return none }

	expected_len := frame_header_size + int(header.length)
	if data.len < expected_len {
		return none
	}

	payload := data[frame_header_size..expected_len]

	return Frame{
		header:  header
		payload: payload
	}
}

// encode encodes a frame to bytes (header + payload).
pub fn (f Frame) encode() []u8 {
	mut buf := f.header.encode()
	buf << f.payload
	return buf
}

// read_frame_from reads and parses an HTTP/2 frame from any connection.
// It reads the 9-byte header, validates the frame size, reads the payload,
// and returns the assembled Frame. Unknown frame types are silently skipped
// per RFC 7540 §5.5.
pub fn read_frame_from(mut conn ServerConn, max_frame_size u32) !Frame {
	for {
		mut header_buf := []u8{len: frame_header_size}
		read_exact(mut conn, mut header_buf, frame_header_size)!

		raw_length := (u32(header_buf[0]) << 16) | (u32(header_buf[1]) << 8) | u32(header_buf[2])

		if raw_length > max_frame_size {
			return error('frame size ${raw_length} exceeds max_frame_size ${max_frame_size}')
		}

		header := parse_frame_header(header_buf) or {
			// Unknown frame type: read and discard payload, then continue to next frame.
			$if trace_http2 ? {
				eprintln('[HTTP/2] Skipping unknown frame type byte 0x${header_buf[3]:02x}, discarding ${raw_length} bytes')
			}
			if raw_length > 0 {
				mut discard := []u8{len: int(raw_length)}
				read_exact(mut conn, mut discard, int(raw_length))!
			}
			continue
		}

		mut payload := []u8{len: int(header.length)}
		if header.length > 0 {
			read_exact(mut conn, mut payload, int(header.length))!
		}

		return Frame{
			header:  header
			payload: payload
		}
	}
	return error('unreachable')
}

// validate validates frame constraints per RFC 7540.
pub fn (f Frame) validate() ! {
	if f.header.length > max_frame_size {
		return error('frame size ${f.header.length} exceeds maximum ${max_frame_size}')
	}

	if f.header.stream_id == 0 {
		match f.header.frame_type {
			.data, .headers, .priority, .rst_stream, .push_promise, .continuation {
				return error('${f.header.frame_type} frame cannot use stream 0')
			}
			else {}
		}
	} else {
		match f.header.frame_type {
			.settings, .ping, .goaway {
				return error('${f.header.frame_type} frame must use stream 0')
			}
			else {}
		}
	}
}

// encode_frame_to_buffer encodes a frame into a pre-allocated buffer.
// Provides buffer-reuse optimization over Frame.encode().
pub fn encode_frame_to_buffer(frame Frame, mut buf []u8) []u8 {
	required_size := frame_header_size + frame.payload.len
	if buf.len < required_size {
		buf = []u8{len: required_size}
	}

	buf[0] = u8(frame.header.length >> 16)
	buf[1] = u8(frame.header.length >> 8)
	buf[2] = u8(frame.header.length)
	buf[3] = u8(frame.header.frame_type)
	buf[4] = frame.header.flags
	buf[5] = u8((frame.header.stream_id >> 24) & 0x7f)
	buf[6] = u8(frame.header.stream_id >> 16)
	buf[7] = u8(frame.header.stream_id >> 8)
	buf[8] = u8(frame.header.stream_id)

	if frame.payload.len > 0 {
		copy(mut buf[frame_header_size..], frame.payload)
	}

	return buf[..required_size]
}

// frame_type_from_byte converts a byte to a FrameType enum value.
// Returns none for unrecognized frame types per RFC 7540 §4.1.
pub fn frame_type_from_byte(b u8) ?FrameType {
	return match b {
		0x0 { FrameType.data }
		0x1 { FrameType.headers }
		0x2 { FrameType.priority }
		0x3 { FrameType.rst_stream }
		0x4 { FrameType.settings }
		0x5 { FrameType.push_promise }
		0x6 { FrameType.ping }
		0x7 { FrameType.goaway }
		0x8 { FrameType.window_update }
		0x9 { FrameType.continuation }
		else { none }
	}
}

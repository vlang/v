// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// This file implements the HTTP/2 binary framing layer (RFC 7540 Sections 4
// and 6). It is self-contained and does not touch the rest of net.http; the
// connection layer (added separately) drives it.

// HTTP/2 frame types (RFC 7540 Section 6).
pub const h2_frame_data = u8(0x0)
pub const h2_frame_headers = u8(0x1)
pub const h2_frame_priority = u8(0x2)
pub const h2_frame_rst_stream = u8(0x3)
pub const h2_frame_settings = u8(0x4)
pub const h2_frame_push_promise = u8(0x5)
pub const h2_frame_ping = u8(0x6)
pub const h2_frame_goaway = u8(0x7)
pub const h2_frame_window_update = u8(0x8)
pub const h2_frame_continuation = u8(0x9)

// HTTP/2 frame flags (RFC 7540 Section 6). The same bit is reused across
// frame types with different meanings.
pub const h2_flag_end_stream = u8(0x1)
pub const h2_flag_ack = u8(0x1)
pub const h2_flag_end_headers = u8(0x4)
pub const h2_flag_padded = u8(0x8)
pub const h2_flag_priority = u8(0x20)

// h2_frame_header_len is the fixed size of a frame header in bytes.
pub const h2_frame_header_len = 9

// h2_default_max_frame_size is the initial SETTINGS_MAX_FRAME_SIZE
// (RFC 7540 Section 6.5.2), and the smallest value a peer may set it to.
pub const h2_default_max_frame_size = u32(16384)

// h2_max_max_frame_size is the largest permitted SETTINGS_MAX_FRAME_SIZE
// (RFC 7540 Section 6.5.2): 2^24-1 bytes.
pub const h2_max_max_frame_size = u32(16777215)

// HTTP/2 setting identifiers (RFC 7540 Section 6.5.2).
pub const h2_settings_header_table_size = u16(0x1)
pub const h2_settings_enable_push = u16(0x2)
pub const h2_settings_max_concurrent_streams = u16(0x3)
pub const h2_settings_initial_window_size = u16(0x4)
pub const h2_settings_max_frame_size = u16(0x5)
pub const h2_settings_max_header_list_size = u16(0x6)

// H2FrameHeader is the 9-byte header that precedes every HTTP/2 frame.
pub struct H2FrameHeader {
pub:
	length    u32 // 24-bit payload length
	typ       u8
	flags     u8
	stream_id u32 // 31-bit; the reserved bit is ignored on read
}

pub struct H2DataFrame {
pub:
	stream_id  u32
	data       []u8 // payload with any padding stripped
	end_stream bool
	// flow_size is the full received payload length (pad-length byte + data +
	// padding) that counts toward flow control (RFC 7540 6.9.1). For outgoing
	// frames it is left 0 and unused; the parser sets it for received frames.
	flow_size int
}

pub struct H2HeadersFrame {
pub:
	stream_id   u32
	fragment    []u8 // HPACK header block fragment
	end_stream  bool
	end_headers bool
	// Priority information, present only when the PRIORITY flag is set.
	has_priority bool
	exclusive    bool
	stream_dep   u32
	weight       u8
}

pub struct H2PriorityFrame {
pub:
	stream_id  u32
	exclusive  bool
	stream_dep u32
	weight     u8
}

pub struct H2RstStreamFrame {
pub:
	stream_id  u32
	error_code u32
}

pub struct H2Setting {
pub:
	id    u16
	value u32
}

pub struct H2SettingsFrame {
pub:
	ack      bool
	settings []H2Setting
}

pub struct H2PushPromiseFrame {
pub:
	stream_id          u32
	promised_stream_id u32
	fragment           []u8
	end_headers        bool
}

pub struct H2PingFrame {
pub:
	ack  bool
	data []u8 // 8 opaque bytes
}

pub struct H2GoawayFrame {
pub:
	last_stream_id u32
	error_code     u32
	debug_data     []u8
}

pub struct H2WindowUpdateFrame {
pub:
	stream_id             u32
	window_size_increment u32
}

pub struct H2ContinuationFrame {
pub:
	stream_id   u32
	fragment    []u8
	end_headers bool
}

// H2UnknownFrame preserves a frame of an unrecognised type, which receivers
// must ignore (RFC 7540 Section 4.1) but may want to inspect or forward.
pub struct H2UnknownFrame {
pub:
	header  H2FrameHeader
	payload []u8
}

// H2Frame is any HTTP/2 frame.
pub type H2Frame = H2ContinuationFrame
	| H2DataFrame
	| H2GoawayFrame
	| H2HeadersFrame
	| H2PingFrame
	| H2PriorityFrame
	| H2PushPromiseFrame
	| H2RstStreamFrame
	| H2SettingsFrame
	| H2UnknownFrame
	| H2WindowUpdateFrame

// --- Big-endian helpers ---

fn h2_be_u16(b []u8, o int) u16 {
	return (u16(b[o]) << 8) | u16(b[o + 1])
}

fn h2_be_u24(b []u8, o int) u32 {
	return (u32(b[o]) << 16) | (u32(b[o + 1]) << 8) | u32(b[o + 2])
}

fn h2_be_u32(b []u8, o int) u32 {
	return (u32(b[o]) << 24) | (u32(b[o + 1]) << 16) | (u32(b[o + 2]) << 8) | u32(b[o + 3])
}

fn h2_put_u16(mut b []u8, v u16) {
	b << u8(v >> 8)
	b << u8(v)
}

fn h2_put_u24(mut b []u8, v u32) {
	b << u8(v >> 16)
	b << u8(v >> 8)
	b << u8(v)
}

fn h2_put_u32(mut b []u8, v u32) {
	b << u8(v >> 24)
	b << u8(v >> 16)
	b << u8(v >> 8)
	b << u8(v)
}

// --- Frame header ---

// h2_parse_frame_header parses the 9-byte frame header at the start of `buf`.
pub fn h2_parse_frame_header(buf []u8) !H2FrameHeader {
	if buf.len < h2_frame_header_len {
		return error('h2: frame header truncated')
	}
	return H2FrameHeader{
		length:    h2_be_u24(buf, 0)
		typ:       buf[3]
		flags:     buf[4]
		stream_id: h2_be_u32(buf, 5) & 0x7fff_ffff
	}
}

// --- Decoding ---

// h2_read_frame parses one frame (header + payload) from the start of `buf`,
// returning the frame and the number of bytes consumed. The caller is
// responsible for enforcing the negotiated SETTINGS_MAX_FRAME_SIZE.
pub fn h2_read_frame(buf []u8) !(H2Frame, int) {
	header := h2_parse_frame_header(buf)!
	total := h2_frame_header_len + int(header.length)
	if buf.len < total {
		return error('h2: frame payload truncated (need ${total}, have ${buf.len})')
	}
	payload := buf[h2_frame_header_len..total]
	frame := h2_parse_frame(header, payload)!
	return frame, total
}

// h2_strip_padding removes the optional pad-length prefix byte and the
// trailing padding from a frame payload (RFC 7540 Section 6.1).
fn h2_strip_padding(payload []u8, padded bool) ![]u8 {
	if !padded {
		return payload
	}
	if payload.len < 1 {
		return error('h2: padded frame missing pad length')
	}
	pad_len := int(payload[0])
	if 1 + pad_len > payload.len {
		return error('h2: pad length exceeds frame payload')
	}
	return payload[1..payload.len - pad_len]
}

// h2_parse_frame decodes a frame from an already-parsed header and its payload.
pub fn h2_parse_frame(header H2FrameHeader, payload []u8) !H2Frame {
	match header.typ {
		h2_frame_data {
			if header.stream_id == 0 {
				return error('h2: DATA frame on stream 0')
			}
			body := h2_strip_padding(payload, header.flags & h2_flag_padded != 0)!
			return H2DataFrame{
				stream_id:  header.stream_id
				data:       body.clone()
				end_stream: header.flags & h2_flag_end_stream != 0
				flow_size:  payload.len
			}
		}
		h2_frame_headers {
			if header.stream_id == 0 {
				return error('h2: HEADERS frame on stream 0')
			}
			mut body := h2_strip_padding(payload, header.flags & h2_flag_padded != 0)!
			mut has_priority := false
			mut exclusive := false
			mut stream_dep := u32(0)
			mut weight := u8(0)
			if header.flags & h2_flag_priority != 0 {
				if body.len < 5 {
					return error('h2: HEADERS priority section truncated')
				}
				dep := h2_be_u32(body, 0)
				has_priority = true
				exclusive = dep & 0x8000_0000 != 0
				stream_dep = dep & 0x7fff_ffff
				weight = body[4]
				body = unsafe { body[5..] }
			}
			return H2HeadersFrame{
				stream_id:    header.stream_id
				fragment:     body.clone()
				end_stream:   header.flags & h2_flag_end_stream != 0
				end_headers:  header.flags & h2_flag_end_headers != 0
				has_priority: has_priority
				exclusive:    exclusive
				stream_dep:   stream_dep
				weight:       weight
			}
		}
		h2_frame_priority {
			if header.stream_id == 0 {
				return error('h2: PRIORITY frame on stream 0')
			}
			if payload.len != 5 {
				return error('h2: PRIORITY frame must be 5 bytes')
			}
			// Note: a stream depending on itself (stream_dep == stream_id) is a
			// stream error (RFC 7540 Section 5.3.1), and a zero stream
			// dependency is otherwise valid. These are semantic checks left to
			// the connection layer, which must respond with RST_STREAM on the
			// affected stream rather than tearing down the whole connection.
			dep := h2_be_u32(payload, 0)
			return H2PriorityFrame{
				stream_id:  header.stream_id
				exclusive:  dep & 0x8000_0000 != 0
				stream_dep: dep & 0x7fff_ffff
				weight:     payload[4]
			}
		}
		h2_frame_rst_stream {
			if header.stream_id == 0 {
				return error('h2: RST_STREAM frame on stream 0')
			}
			if payload.len != 4 {
				return error('h2: RST_STREAM frame must be 4 bytes')
			}
			return H2RstStreamFrame{
				stream_id:  header.stream_id
				error_code: h2_be_u32(payload, 0)
			}
		}
		h2_frame_settings {
			if header.stream_id != 0 {
				return error('h2: SETTINGS frame on non-zero stream')
			}
			ack := header.flags & h2_flag_ack != 0
			if ack {
				if payload.len != 0 {
					return error('h2: SETTINGS ACK must have empty payload')
				}
				return H2SettingsFrame{
					ack: true
				}
			}
			if payload.len % 6 != 0 {
				return error('h2: SETTINGS payload not a multiple of 6')
			}
			mut settings := []H2Setting{cap: payload.len / 6}
			for i := 0; i < payload.len; i += 6 {
				settings << H2Setting{
					id:    h2_be_u16(payload, i)
					value: h2_be_u32(payload, i + 2)
				}
			}
			return H2SettingsFrame{
				ack:      false
				settings: settings
			}
		}
		h2_frame_push_promise {
			if header.stream_id == 0 {
				return error('h2: PUSH_PROMISE frame on stream 0')
			}
			mut body := h2_strip_padding(payload, header.flags & h2_flag_padded != 0)!
			if body.len < 4 {
				return error('h2: PUSH_PROMISE missing promised stream id')
			}
			promised := h2_be_u32(body, 0) & 0x7fff_ffff
			return H2PushPromiseFrame{
				stream_id:          header.stream_id
				promised_stream_id: promised
				fragment:           body[4..].clone()
				end_headers:        header.flags & h2_flag_end_headers != 0
			}
		}
		h2_frame_ping {
			if header.stream_id != 0 {
				return error('h2: PING frame on non-zero stream')
			}
			if payload.len != 8 {
				return error('h2: PING frame must be 8 bytes')
			}
			return H2PingFrame{
				ack:  header.flags & h2_flag_ack != 0
				data: payload.clone()
			}
		}
		h2_frame_goaway {
			if header.stream_id != 0 {
				return error('h2: GOAWAY frame on non-zero stream')
			}
			if payload.len < 8 {
				return error('h2: GOAWAY frame too short')
			}
			return H2GoawayFrame{
				last_stream_id: h2_be_u32(payload, 0) & 0x7fff_ffff
				error_code:     h2_be_u32(payload, 4)
				debug_data:     payload[8..].clone()
			}
		}
		h2_frame_window_update {
			if payload.len != 4 {
				return error('h2: WINDOW_UPDATE frame must be 4 bytes')
			}
			// Note: a zero increment is an error (RFC 7540 Section 6.9) — a
			// stream error on a stream, but a connection error on stream 0.
			// That stream-vs-connection distinction is the connection layer's
			// responsibility, so it is not rejected here.
			return H2WindowUpdateFrame{
				stream_id:             header.stream_id
				window_size_increment: h2_be_u32(payload, 0) & 0x7fff_ffff
			}
		}
		h2_frame_continuation {
			if header.stream_id == 0 {
				return error('h2: CONTINUATION frame on stream 0')
			}
			return H2ContinuationFrame{
				stream_id:   header.stream_id
				fragment:    payload.clone()
				end_headers: header.flags & h2_flag_end_headers != 0
			}
		}
		else {
			// Unknown frame types must be ignored (RFC 7540 Section 4.1);
			// preserve them so the caller can decide.
			return H2UnknownFrame{
				header:  header
				payload: payload.clone()
			}
		}
	}
}

// --- Encoding ---

// h2_frame_bytes builds a complete frame from its parts.
fn h2_frame_bytes(typ u8, flags u8, stream_id u32, payload []u8) []u8 {
	mut b := []u8{cap: h2_frame_header_len + payload.len}
	h2_put_u24(mut b, u32(payload.len))
	b << typ
	b << flags
	h2_put_u32(mut b, stream_id & 0x7fff_ffff)
	b << payload
	return b
}

// encode serialises a frame to its on-the-wire bytes. The encoder never emits
// padding.
pub fn (f H2Frame) encode() []u8 {
	match f {
		H2DataFrame {
			flags := if f.end_stream { h2_flag_end_stream } else { u8(0) }
			return h2_frame_bytes(h2_frame_data, flags, f.stream_id, f.data)
		}
		H2HeadersFrame {
			mut flags := u8(0)
			if f.end_stream {
				flags |= h2_flag_end_stream
			}
			if f.end_headers {
				flags |= h2_flag_end_headers
			}
			mut payload := []u8{}
			if f.has_priority {
				flags |= h2_flag_priority
				mut dep := f.stream_dep & 0x7fff_ffff
				if f.exclusive {
					dep |= 0x8000_0000
				}
				h2_put_u32(mut payload, dep)
				payload << f.weight
			}
			payload << f.fragment
			return h2_frame_bytes(h2_frame_headers, flags, f.stream_id, payload)
		}
		H2PriorityFrame {
			mut payload := []u8{cap: 5}
			mut dep := f.stream_dep & 0x7fff_ffff
			if f.exclusive {
				dep |= 0x8000_0000
			}
			h2_put_u32(mut payload, dep)
			payload << f.weight
			return h2_frame_bytes(h2_frame_priority, 0, f.stream_id, payload)
		}
		H2RstStreamFrame {
			mut payload := []u8{cap: 4}
			h2_put_u32(mut payload, f.error_code)
			return h2_frame_bytes(h2_frame_rst_stream, 0, f.stream_id, payload)
		}
		H2SettingsFrame {
			if f.ack {
				return h2_frame_bytes(h2_frame_settings, h2_flag_ack, 0, [])
			}
			mut payload := []u8{cap: f.settings.len * 6}
			for s in f.settings {
				h2_put_u16(mut payload, s.id)
				h2_put_u32(mut payload, s.value)
			}
			return h2_frame_bytes(h2_frame_settings, 0, 0, payload)
		}
		H2PushPromiseFrame {
			mut flags := u8(0)
			if f.end_headers {
				flags |= h2_flag_end_headers
			}
			mut payload := []u8{cap: 4 + f.fragment.len}
			h2_put_u32(mut payload, f.promised_stream_id & 0x7fff_ffff)
			payload << f.fragment
			return h2_frame_bytes(h2_frame_push_promise, flags, f.stream_id, payload)
		}
		H2PingFrame {
			flags := if f.ack { h2_flag_ack } else { u8(0) }
			mut payload := []u8{len: 8}
			for i in 0 .. 8 {
				payload[i] = if i < f.data.len { f.data[i] } else { u8(0) }
			}
			return h2_frame_bytes(h2_frame_ping, flags, 0, payload)
		}
		H2GoawayFrame {
			mut payload := []u8{cap: 8 + f.debug_data.len}
			h2_put_u32(mut payload, f.last_stream_id & 0x7fff_ffff)
			h2_put_u32(mut payload, f.error_code)
			payload << f.debug_data
			return h2_frame_bytes(h2_frame_goaway, 0, 0, payload)
		}
		H2WindowUpdateFrame {
			mut payload := []u8{cap: 4}
			h2_put_u32(mut payload, f.window_size_increment & 0x7fff_ffff)
			return h2_frame_bytes(h2_frame_window_update, 0, f.stream_id, payload)
		}
		H2ContinuationFrame {
			flags := if f.end_headers { h2_flag_end_headers } else { u8(0) }
			return h2_frame_bytes(h2_frame_continuation, flags, f.stream_id, f.fragment)
		}
		H2UnknownFrame {
			return h2_frame_bytes(f.header.typ, f.header.flags, f.header.stream_id, f.payload)
		}
	}
}

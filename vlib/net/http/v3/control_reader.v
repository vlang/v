module v3

// control_reader.v — background control stream reader for HTTP/3 (RFC 9114 §6.2.1).
// Both endpoints MUST open a control stream and send SETTINGS as the first frame.
// This module reads frames from the peer's control stream and applies settings.

// ControlFrameType distinguishes known control frames from unknown ones.
pub enum ControlFrameType {
	settings
	goaway
	unknown
}

// ControlFrameResult holds the parsed result from a single control stream frame.
pub struct ControlFrameResult {
pub:
	frame_type ControlFrameType
	settings   ?Settings
	goaway_id  ?u64
}

// ControlStreamReader reads frames from the peer's control stream.
pub struct ControlStreamReader {
pub mut:
	stream_id         i64 = -1
	settings_received bool
}

// new_control_reader creates a ControlStreamReader ready to parse frames.
pub fn new_control_reader() ControlStreamReader {
	return ControlStreamReader{}
}

// read_control_frame parses a single frame from varint-encoded control stream data.
// The first frame MUST be SETTINGS per RFC 9114 §6.2.1 — error if not.
// Unknown frame types are silently ignored per RFC 9114 §7.2.8.
pub fn (mut r ControlStreamReader) read_control_frame(data []u8) !ControlFrameResult {
	frame_type_val, bt := decode_varint(data)!
	frame_len, bl := decode_varint(data[bt..])!
	payload_start := bt + bl
	payload_end := payload_start + int(frame_len)
	if payload_end > data.len {
		return error('incomplete control frame')
	}
	payload := data[payload_start..payload_end]

	known_type := frame_type_from_u64(frame_type_val)

	if !r.settings_received {
		return r.handle_first_frame(known_type, frame_type_val, payload)
	}
	return r.handle_subsequent_frame(known_type, payload)
}

// handle_first_frame enforces that the first frame is SETTINGS.
fn (mut r ControlStreamReader) handle_first_frame(known_type ?FrameType, raw_type u64, payload []u8) !ControlFrameResult {
	ft := known_type or {
		return error('H3_MISSING_SETTINGS: first frame must be SETTINGS, got unknown type 0x${raw_type:02x} (RFC 9114 §6.2.1)')
	}

	if ft != .settings {
		return error('H3_MISSING_SETTINGS: first frame must be SETTINGS, got ${ft} (RFC 9114 §6.2.1)')
	}
	r.settings_received = true
	settings := parse_peer_settings(payload)!
	return ControlFrameResult{
		frame_type: .settings
		settings:   settings
	}
}

// handle_subsequent_frame processes frames after SETTINGS has been received.
fn (r &ControlStreamReader) handle_subsequent_frame(known_type ?FrameType, payload []u8) !ControlFrameResult {
	ft := known_type or { return ControlFrameResult{
		frame_type: .unknown
	} }

	return match ft {
		.goaway {
			parse_goaway_payload(payload)!
		}
		.settings {
			error('H3_FRAME_UNEXPECTED: duplicate SETTINGS (RFC 9114 §7.2.4)')
		}
		else {
			ControlFrameResult{
				frame_type: .unknown
			}
		}
	}
}

// parse_peer_settings decodes a SETTINGS payload into a Settings struct.
fn parse_peer_settings(payload []u8) !Settings {
	ids, values := parse_settings_payload(payload)!
	mut s := Settings{}
	for i, id in ids {
		match id {
			0x01 { s.qpack_max_table_capacity = values[i] }
			0x06 { s.max_field_section_size = values[i] }
			0x07 { s.qpack_blocked_streams = values[i] }
			else {}
		}
	}
	return s
}

// parse_goaway_payload extracts a stream ID from a GOAWAY frame payload.
fn parse_goaway_payload(payload []u8) !ControlFrameResult {
	stream_id, _ := decode_varint(payload)!
	return ControlFrameResult{
		frame_type: .goaway
		goaway_id:  stream_id
	}
}

// apply_peer_settings updates the client with peer-advertised settings.
// Calls set_peer_max_table_capacity on the QPACK encoder when capacity changes.
pub fn apply_peer_settings(mut c Client, settings Settings) {
	c.settings.max_field_section_size = settings.max_field_section_size
	c.settings.qpack_blocked_streams = settings.qpack_blocked_streams
	if settings.qpack_max_table_capacity != c.settings.qpack_max_table_capacity {
		c.settings.qpack_max_table_capacity = settings.qpack_max_table_capacity
		c.qpack_encoder.set_peer_max_table_capacity(int(settings.qpack_max_table_capacity))
	}
}

// apply_goaway records the peer's GOAWAY stream ID on the client.
pub fn apply_goaway(mut c Client, goaway_id u64) {
	c.last_peer_goaway_stream_id = goaway_id
}

// read_peer_control_stream reads frames from the peer's control stream in a loop.
// Designed to run as a spawned goroutine. Exits on stream close or fatal error.
pub fn read_peer_control_stream(mut c Client) {
	peer_id := c.uni.peer_control_stream_id
	if peer_id < 0 {
		return
	}

	for {
		data := c.quic_conn.recv(u64(peer_id)) or { break }
		if data.len == 0 {
			break
		}

		result := c.control_reader.read_control_frame(data) or {
			$if debug {
				eprintln('control stream error: ${err}')
			}
			break
		}

		match result.frame_type {
			.settings {
				if s := result.settings {
					apply_peer_settings(mut c, s)
				}
			}
			.goaway {
				if gid := result.goaway_id {
					apply_goaway(mut c, gid)
				}
			}
			.unknown {}
		}
	}
}

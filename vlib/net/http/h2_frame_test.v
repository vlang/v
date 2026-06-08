module http

// Tests for the HTTP/2 framing layer (RFC 7540 Sections 4 and 6).

fn roundtrip(f H2Frame) !H2Frame {
	encoded := f.encode()
	decoded, consumed := h2_read_frame(encoded)!
	assert consumed == encoded.len, 'consumed ${consumed} != encoded ${encoded.len}'
	return decoded
}

// --- Frame header ---

fn test_frame_header_layout() {
	// DATA frame, length 5, flags END_STREAM, stream 1, payload "hello".
	f := H2Frame(H2DataFrame{
		stream_id:  1
		data:       'hello'.bytes()
		end_stream: true
	})
	enc := f.encode()
	// 9-byte header: length(3) type(1) flags(1) stream_id(4)
	assert enc[0] == 0 && enc[1] == 0 && enc[2] == 5 // length = 5
	assert enc[3] == h2_frame_data
	assert enc[4] == h2_flag_end_stream
	assert enc[5] == 0 && enc[6] == 0 && enc[7] == 0 && enc[8] == 1 // stream 1
	assert enc[9..] == 'hello'.bytes()

	h := h2_parse_frame_header(enc)!
	assert h.length == 5
	assert h.typ == h2_frame_data
	assert h.flags == h2_flag_end_stream
	assert h.stream_id == 1
}

fn test_frame_header_clears_reserved_bit() {
	// The reserved high bit of the stream id must be ignored on read.
	raw := [u8(0), 0, 0, h2_frame_window_update, 0, 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 1]
	h := h2_parse_frame_header(raw)!
	assert h.stream_id == 0x7fff_ffff
}

// --- Round-trips for each frame type ---

fn test_roundtrip_data() {
	got := roundtrip(H2DataFrame{
		stream_id:  3
		data:       'abc'.bytes()
		end_stream: true
	})!
	d := got as H2DataFrame
	assert d.stream_id == 3
	assert d.data == 'abc'.bytes()
	assert d.end_stream
}

fn test_roundtrip_headers_plain() {
	got := roundtrip(H2HeadersFrame{
		stream_id:   1
		fragment:    [u8(0x82), 0x86, 0x84]
		end_stream:  true
		end_headers: true
	})!
	h := got as H2HeadersFrame
	assert h.stream_id == 1
	assert h.fragment == [u8(0x82), 0x86, 0x84]
	assert h.end_stream
	assert h.end_headers
	assert !h.has_priority
}

fn test_roundtrip_headers_with_priority() {
	got := roundtrip(H2HeadersFrame{
		stream_id:    5
		fragment:     [u8(0x88)]
		end_headers:  true
		has_priority: true
		exclusive:    true
		stream_dep:   3
		weight:       201
	})!
	h := got as H2HeadersFrame
	assert h.has_priority
	assert h.exclusive
	assert h.stream_dep == 3
	assert h.weight == 201
	assert h.fragment == [u8(0x88)]
}

fn test_roundtrip_priority() {
	got := roundtrip(H2PriorityFrame{
		stream_id:  7
		exclusive:  false
		stream_dep: 1
		weight:     16
	})!
	p := got as H2PriorityFrame
	assert p.stream_id == 7
	assert p.stream_dep == 1
	assert p.weight == 16
	assert !p.exclusive
}

fn test_roundtrip_rst_stream() {
	got := roundtrip(H2RstStreamFrame{
		stream_id:  9
		error_code: u32(H2ErrorCode.cancel)
	})!
	r := got as H2RstStreamFrame
	assert r.stream_id == 9
	assert r.error_code == u32(H2ErrorCode.cancel)
}

fn test_roundtrip_settings() {
	got := roundtrip(H2SettingsFrame{
		settings: [
			H2Setting{h2_settings_header_table_size, 4096},
			H2Setting{h2_settings_enable_push, 0},
			H2Setting{h2_settings_initial_window_size, 65535},
		]
	})!
	s := got as H2SettingsFrame
	assert !s.ack
	assert s.settings.len == 3
	assert s.settings[0].id == h2_settings_header_table_size
	assert s.settings[0].value == 4096
	assert s.settings[1].id == h2_settings_enable_push
	assert s.settings[1].value == 0
	assert s.settings[2].value == 65535
}

fn test_roundtrip_settings_ack() {
	got := roundtrip(H2SettingsFrame{
		ack: true
	})!
	s := got as H2SettingsFrame
	assert s.ack
	assert s.settings.len == 0
}

fn test_roundtrip_push_promise() {
	got := roundtrip(H2PushPromiseFrame{
		stream_id:          1
		promised_stream_id: 2
		fragment:           [u8(0x82), 0x84]
		end_headers:        true
	})!
	p := got as H2PushPromiseFrame
	assert p.stream_id == 1
	assert p.promised_stream_id == 2
	assert p.fragment == [u8(0x82), 0x84]
	assert p.end_headers
}

fn test_roundtrip_ping() {
	got := roundtrip(H2PingFrame{
		ack:  true
		data: [u8(1), 2, 3, 4, 5, 6, 7, 8]
	})!
	p := got as H2PingFrame
	assert p.ack
	assert p.data == [u8(1), 2, 3, 4, 5, 6, 7, 8]
}

fn test_roundtrip_goaway() {
	got := roundtrip(H2GoawayFrame{
		last_stream_id: 7
		error_code:     u32(H2ErrorCode.protocol_error)
		debug_data:     'oops'.bytes()
	})!
	g := got as H2GoawayFrame
	assert g.last_stream_id == 7
	assert g.error_code == u32(H2ErrorCode.protocol_error)
	assert g.debug_data == 'oops'.bytes()
}

fn test_roundtrip_window_update() {
	got := roundtrip(H2WindowUpdateFrame{
		stream_id:             0
		window_size_increment: 65535
	})!
	w := got as H2WindowUpdateFrame
	assert w.stream_id == 0
	assert w.window_size_increment == 65535
}

fn test_roundtrip_continuation() {
	got := roundtrip(H2ContinuationFrame{
		stream_id:   3
		fragment:    [u8(0x40), 0x88]
		end_headers: true
	})!
	c := got as H2ContinuationFrame
	assert c.stream_id == 3
	assert c.fragment == [u8(0x40), 0x88]
	assert c.end_headers
}

// --- Padding ---

fn test_data_padding_decode() {
	// Hand-built padded DATA frame: pad length 3, data "hi", 3 pad bytes.
	mut payload := [u8(3)] // pad length
	payload << 'hi'.bytes()
	payload << [u8(0), 0, 0] // padding
	raw := h2_frame_bytes(h2_frame_data, h2_flag_padded, 1, payload)
	frame, _ := h2_read_frame(raw)!
	d := frame as H2DataFrame
	assert d.data == 'hi'.bytes()
}

fn test_padding_length_exceeds_frame_rejected() {
	// pad length 5 but only 2 bytes of payload after it -> error.
	payload := [u8(5), 0x61, 0x62]
	raw := h2_frame_bytes(h2_frame_data, h2_flag_padded, 1, payload)
	h2_read_frame(raw) or { return }
	assert false, 'expected pad-length error'
}

// --- Unknown frames ---

fn test_unknown_frame_preserved() {
	// Type 0x16 is not defined; it must be preserved, not rejected.
	raw := h2_frame_bytes(0x16, 0x0, 0, [u8(0xde), 0xad, 0xbe, 0xef])
	frame, consumed := h2_read_frame(raw)!
	assert consumed == raw.len
	u := frame as H2UnknownFrame
	assert u.header.typ == 0x16
	assert u.payload == [u8(0xde), 0xad, 0xbe, 0xef]
	// And it round-trips back to the same bytes.
	assert frame.encode() == raw
}

// --- Structural validation ---

fn test_read_frame_truncated_payload() {
	// Header claims 10 bytes of payload but none follow.
	raw := [u8(0), 0, 10, h2_frame_data, 0, 0, 0, 0, 1]
	h2_read_frame(raw) or { return }
	assert false, 'expected truncated payload error'
}

fn test_settings_bad_length_rejected() {
	// SETTINGS payload not a multiple of 6.
	raw := h2_frame_bytes(h2_frame_settings, 0, 0, [u8(0), 1, 0, 0, 0])
	h2_read_frame(raw) or { return }
	assert false, 'expected SETTINGS length error'
}

fn test_settings_ack_with_payload_rejected() {
	raw := h2_frame_bytes(h2_frame_settings, h2_flag_ack, 0, [u8(0), 1, 0, 0, 0x10, 0])
	h2_read_frame(raw) or { return }
	assert false, 'expected SETTINGS ACK payload error'
}

fn test_window_update_bad_length_rejected() {
	raw := h2_frame_bytes(h2_frame_window_update, 0, 0, [u8(0), 0, 1])
	h2_read_frame(raw) or { return }
	assert false, 'expected WINDOW_UPDATE length error'
}

fn test_ping_bad_length_rejected() {
	raw := h2_frame_bytes(h2_frame_ping, 0, 0, [u8(1), 2, 3, 4])
	h2_read_frame(raw) or { return }
	assert false, 'expected PING length error'
}

fn test_data_on_stream_zero_rejected() {
	raw := h2_frame_bytes(h2_frame_data, 0, 0, 'x'.bytes())
	h2_read_frame(raw) or { return }
	assert false, 'expected DATA-on-stream-0 error'
}

fn test_settings_on_nonzero_stream_rejected() {
	raw := h2_frame_bytes(h2_frame_settings, 0, 1, [])
	h2_read_frame(raw) or { return }
	assert false, 'expected SETTINGS-on-stream-1 error'
}

// --- Multiple frames back to back ---

fn test_read_consecutive_frames() {
	mut buf := []u8{}
	buf << H2Frame(H2SettingsFrame{}).encode()
	buf << H2Frame(H2DataFrame{
		stream_id: 1
		data:      'one'.bytes()
	}).encode()
	buf << H2Frame(H2PingFrame{
		data: [u8(0), 0, 0, 0, 0, 0, 0, 9]
	}).encode()

	mut pos := 0
	f1, c1 := h2_read_frame(buf[pos..])!
	pos += c1
	f2, c2 := h2_read_frame(buf[pos..])!
	pos += c2
	f3, c3 := h2_read_frame(buf[pos..])!
	pos += c3
	assert pos == buf.len
	assert f1 is H2SettingsFrame
	d := f2 as H2DataFrame
	assert d.data == 'one'.bytes()
	p := f3 as H2PingFrame
	assert p.data[7] == 9
}

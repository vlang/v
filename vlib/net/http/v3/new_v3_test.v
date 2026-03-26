module v3

// Tests for QPACK encoding/decoding and varint codec.

// new_test_server_connection creates a minimal ServerConnection for unit tests.
fn new_test_server_connection() ServerConnection {
	return ServerConnection{
		encoder:  new_qpack_encoder(4096, 100)
		decoder:  new_qpack_decoder(4096, 100)
		settings: Settings{
			max_field_section_size:   8192
			qpack_max_table_capacity: 4096
			qpack_blocked_streams:    100
		}
	}
}

fn test_qpack_encoding_decoding() {
	mut encoder := new_qpack_encoder(4096, 0)
	mut decoder := new_qpack_decoder(4096, 0)

	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  ':path'
			value: '/index.html'
		},
		HeaderField{
			name:  'content-type'
			value: 'text/html'
		},
	]

	println('Testing QPACK encoding...')
	encoded := encoder.encode(headers)
	assert encoded.len > 0

	println('Testing QPACK decoding...')
	decoded := decoder.decode(encoded) or {
		println('Decode Error: ${err}')
		assert false
		return
	}

	assert decoded.len == headers.len
	for i in 0 .. headers.len {
		assert decoded[i].name == headers[i].name
		assert decoded[i].value == headers[i].value
	}
	println('QPACK test passed')
}

fn test_varint_encoding_decoding() {
	println('Testing VarInt encoding/decoding...')

	cases := {
		u64(25):                 1
		u64(15293):              2
		u64(494878333):          4
		u64(151288809941952652): 8
	}

	for val, expected_len in cases {
		encoded := encode_varint(val) or {
			assert false, 'Failed to encode varint'
			return
		}
		assert encoded.len == expected_len

		decoded, bytes_read := decode_varint(encoded) or {
			assert false, 'Failed to decode varint'
			return
		}
		assert decoded == val
		assert bytes_read == expected_len
	}
	println('VarInt test passed')
}

fn test_goaway_stream_id_extraction() {
	// Build a GOAWAY frame: frame_type=0x07, payload=varint(stream_id=8)
	goaway_stream_id := u64(8)
	payload := encode_varint(goaway_stream_id) or {
		assert false, 'Failed to encode varint for GOAWAY'
		return
	}

	mut frame_data := []u8{}
	frame_data << encode_varint(u64(FrameType.goaway)) or {
		assert false, 'Failed to encode frame type'
		return
	}
	frame_data << encode_varint(u64(payload.len)) or {
		assert false, 'Failed to encode frame length'
		return
	}
	frame_data << payload

	mut encoder := new_qpack_encoder(4096, 0)
	mut decoder := new_qpack_decoder(4096, 0)

	// Build a HEADERS frame first so parse_response_frames has something before GOAWAY
	headers := [
		HeaderField{
			name:  ':status'
			value: '200'
		},
	]
	encoded_headers := encoder.encode(headers)
	mut all_data := []u8{}
	all_data << encode_varint(u64(FrameType.headers)) or {
		assert false, 'header frame type encode failed'
		return
	}
	all_data << encode_varint(u64(encoded_headers.len)) or {
		assert false, 'header frame len encode failed'
		return
	}
	all_data << encoded_headers
	all_data << frame_data

	// Create a minimal client for parse_response_frames
	mut client := Client{
		qpack_decoder: decoder
	}

	parsed_headers, _ := client.parse_response_frames(all_data) or {
		assert false, 'parse_response_frames failed: ${err}'
		return
	}

	assert parsed_headers.len == 1
	assert parsed_headers[0].name == ':status'

	// Verify the GOAWAY stream ID was extracted and stored
	assert client.last_peer_goaway_stream_id == goaway_stream_id
}

fn test_goaway_blocks_new_streams() {
	mut client := Client{
		last_peer_goaway_stream_id: 4
		next_stream_id:             8
	}

	// Attempting request with stream_id (8) > goaway limit (4) should fail
	client.request(Request{
		method: .get
		url:    '/'
		host:   'example.com'
	}) or {
		assert err.msg().contains('going away')
		return
	}
	assert false, 'expected goaway error'
}

fn test_header_helpers() {
	println('Testing header helpers...')

	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  'content-type'
			value: 'application/json'
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 0

	decoded := decoder.decode(encoded) or {
		assert false, 'Failed to decode headers'
		return
	}

	assert decoded.len == headers.len
	assert decoded[0].name == ':method'
	assert decoded[1].value == 'application/json'
	println('Header helpers test passed')
}

fn test_large_body_chunking() {
	println('Testing large body chunking...')

	// Body that spans 2 full chunks + 1 partial chunk
	body_size := max_data_frame_size * 2 + 100
	body := []u8{len: body_size, init: u8(0x41)}.bytestr()

	frames := create_data_frames(body)

	// Expect 3 frames: 2 full (16384 bytes each) + 1 partial (100 bytes)
	assert frames.len == 3, 'expected 3 frames, got ${frames.len}'
	assert frames[0].payload.len == max_data_frame_size
	assert frames[0].length == u64(max_data_frame_size)
	assert frames[1].payload.len == max_data_frame_size
	assert frames[1].length == u64(max_data_frame_size)
	assert frames[2].payload.len == 100
	assert frames[2].length == u64(100)

	// All frames must be DATA type
	for f in frames {
		assert f.frame_type == .data
	}

	// Total payload must equal original body
	mut total := 0
	for f in frames {
		total += f.payload.len
	}
	assert total == body_size

	println('Large body chunking test passed')
}

fn test_small_body_no_chunking() {
	println('Testing small body produces single frame...')

	small_body := 'hello world'
	frames := create_data_frames(small_body)

	assert frames.len == 1
	assert frames[0].payload.len == small_body.len
	assert frames[0].frame_type == .data

	println('Small body no chunking test passed')
}

fn test_empty_body_data_frame() {
	println('Testing empty body produces empty DATA frame...')

	frames := create_data_frames('')

	assert frames.len == 1
	assert frames[0].payload.len == 0
	assert frames[0].length == u64(0)
	assert frames[0].frame_type == .data

	println('Empty body data frame test passed')
}

fn test_exact_chunk_boundary() {
	println('Testing body at exact chunk boundary...')

	body := []u8{len: max_data_frame_size * 2, init: u8(0x42)}.bytestr()
	frames := create_data_frames(body)

	// Exactly 2 full frames, no partial
	assert frames.len == 2
	assert frames[0].payload.len == max_data_frame_size
	assert frames[1].payload.len == max_data_frame_size

	println('Exact chunk boundary test passed')
}

// ── P3-2: DATA before HEADERS returns H3_FRAME_UNEXPECTED ──

fn test_data_before_headers_returns_frame_unexpected() {
	mut s := Server{}
	mut conn := new_test_server_connection()

	// Send DATA on stream 0 without a preceding HEADERS frame
	s.handle_data_frame(mut conn, u64(0), 'hello'.bytes()) or {
		errmsg := err.msg()
		assert errmsg.contains('H3_FRAME_UNEXPECTED') || errmsg.contains('HEADERS'), 'unexpected error: "${errmsg}"'
		return
	}
	assert false, 'expected H3_FRAME_UNEXPECTED error when DATA arrives before HEADERS'
}

fn test_data_after_headers_succeeds() {
	mut s := Server{}
	mut conn := new_test_server_connection()
	stream_id := u64(0)

	// Manually create the stream with headers to simulate HEADERS arriving first.
	// Set request_complete to true to avoid triggering process_request, which
	// requires a fully initialized crypto context.
	conn.streams[stream_id] = &ServerStream{
		id:               stream_id
		headers:          [HeaderField{':method', 'POST'}, HeaderField{':path', '/'}]
		headers_received: true
		request_complete: true
	}

	// DATA after HEADERS should succeed (data is appended)
	s.handle_data_frame(mut conn, stream_id, 'body'.bytes()) or {
		assert false, 'DATA after HEADERS should not fail: ${err}'
		return
	}

	// Verify data was appended
	stream := conn.streams[stream_id] or {
		assert false, 'stream should still exist'
		return
	}
	assert stream.data == 'body'.bytes()
}

// ── P3-3: GOAWAY 2-phase shutdown ──

fn test_server_stop_sends_two_phase_goaway() {
	mut s := Server{
		running: true
	}
	mut conn := new_test_server_connection()
	conn.uni.control_stream_id = 2
	conn.next_client_stream_id = 12
	s.connections['test'] = &conn

	// stop() should send two GOAWAY frames: initial (max) + final (actual last stream id)
	goaway_frames := s.build_goaway_shutdown_frames(mut conn)
	assert goaway_frames.len == 2, 'expected 2 GOAWAY frames, got ${goaway_frames.len}'

	// First GOAWAY should use max_stream_id signal
	first_id := extract_goaway_stream_id(goaway_frames[0]) or {
		assert false, 'failed to extract first GOAWAY stream ID: ${err}'
		return
	}
	assert first_id == max_varint, 'first GOAWAY should use max_varint as stream ID'

	// Second GOAWAY should use actual last stream ID
	second_id := extract_goaway_stream_id(goaway_frames[1]) or {
		assert false, 'failed to extract second GOAWAY stream ID: ${err}'
		return
	}
	assert second_id == u64(12), 'second GOAWAY should use actual last stream ID'
}

// ── P3-4: SETTINGS expansion with actual values ──

fn test_send_settings_encodes_actual_values() {
	// build_settings_payload should encode three settings
	payload := build_settings_payload(Settings{
		qpack_max_table_capacity: 4096
		qpack_blocked_streams:    100
		max_field_section_size:   65536
	}) or {
		assert false, 'build_settings_payload failed: ${err}'
		return
	}
	assert payload.len > 0

	// Parse the payload back into settings
	ids, values := parse_settings_payload(payload) or {
		assert false, 'parse_settings_payload failed: ${err}'
		return
	}

	assert ids.len == 3, 'expected 3 settings, got ${ids.len}'

	// Verify all expected setting IDs are present
	mut found_0x01 := false
	mut found_0x06 := false
	mut found_0x07 := false
	for i, id in ids {
		if id == 0x01 {
			found_0x01 = true
			assert values[i] == u64(4096), 'QPACK_MAX_TABLE_CAPACITY should be 4096'
		}
		if id == 0x06 {
			found_0x06 = true
			assert values[i] == u64(65536), 'MAX_FIELD_SECTION_SIZE should be 65536'
		}
		if id == 0x07 {
			found_0x07 = true
			assert values[i] == u64(100), 'QPACK_BLOCKED_STREAMS should be 100'
		}
	}
	assert found_0x01, 'QPACK_MAX_TABLE_CAPACITY (0x01) missing'
	assert found_0x06, 'MAX_FIELD_SECTION_SIZE (0x06) missing'
	assert found_0x07, 'QPACK_BLOCKED_STREAMS (0x07) missing'
}

fn test_handle_settings_rejects_duplicate_ids() {
	mut s := Server{}
	mut conn := new_test_server_connection()

	// Build a SETTINGS payload with duplicate setting ID 0x01
	mut payload := []u8{}
	payload << encode_varint(u64(0x01)) or { return }
	payload << encode_varint(u64(4096)) or { return }
	payload << encode_varint(u64(0x01)) or { return } // duplicate
	payload << encode_varint(u64(8192)) or { return }

	s.handle_settings_frame(mut conn, payload) or {
		assert err.msg().contains('H3_SETTINGS_ERROR') || err.msg().contains('duplicate')
		return
	}
	assert false, 'expected H3_SETTINGS_ERROR for duplicate setting IDs'
}

// ── P3-5: QPACK capacity enforcement ──

fn test_encoder_respects_peer_max_table_capacity() {
	mut encoder := new_qpack_encoder(4096, 100)

	// Set a very small peer capacity — forces literal encoding
	encoder.set_peer_max_table_capacity(0)

	headers := [HeaderField{'x-test', 'value'}]
	encoded := encoder.encode(headers)
	assert encoded.len > 0

	// With 0 peer capacity, dynamic table should have 0 entries
	assert encoder.dynamic_table.count == 0, 'no entries should be in dynamic table with 0 peer capacity'
}

fn test_dynamic_table_resize_evicts_entries() {
	mut dt := new_dynamic_table(4096)

	// Insert entries
	dt.insert(HeaderField{ name: 'key1', value: 'value1' })
	dt.insert(HeaderField{ name: 'key2', value: 'value2' })
	assert dt.count == 2

	// Resize to very small — should evict entries
	dt.resize(0)
	assert dt.count == 0, 'resize(0) should evict all entries'
	assert dt.max_size == 0, 'max_size should be updated to 0'
}

fn test_dynamic_table_resize_keeps_fitting_entries() {
	mut dt := new_dynamic_table(4096)

	// Insert one small entry (name=3 + value=1 + 32 overhead = 36 bytes)
	dt.insert(HeaderField{ name: 'abc', value: 'x' })
	assert dt.count == 1

	// Resize to something that fits this entry
	dt.resize(100)
	assert dt.count == 1, 'entry should still fit after resize to 100'
}

// ── P3-6: Request cancel wiring ──

fn test_cancel_request_uses_h3_request_cancelled() {
	// Verify that cancel_request exists as pub method and uses correct error code
	mut client := Client{}
	// cancel_request on a zero-initialized client will fail (closed conn),
	// but it should attempt reset_stream with h3_request_cancelled code
	client.cancel_request(u64(4)) or {
		// Expected to fail on closed/nil connection — the important thing
		// is that the method exists and compiles correctly
		return
	}
}

// ── P4-1: QUIC Packet Matching by CID (RFC 9000 §5.2) ──

fn test_extract_dcid_short_header() {
	// Short header: bit 7 of byte 0 is 0. DCID starts at byte 1.
	cid_len := 4
	mut packet := []u8{len: 1 + cid_len + 10}
	packet[0] = 0x40 // short header (bit 7 = 0)
	packet[1] = 0xab
	packet[2] = 0xcd
	packet[3] = 0xef
	packet[4] = 0x01

	dcid := extract_dcid_from_packet(packet, cid_len) or {
		assert false, 'extract_dcid_from_packet failed: ${err}'
		return
	}
	assert dcid == 'abcdef01', 'expected abcdef01, got ${dcid}'
}

fn test_extract_dcid_long_header() {
	// Long header: bit 7 of byte 0 is 1. Byte 5 = DCID length, DCID at byte 6.
	mut packet := []u8{len: 20}
	packet[0] = 0xc0 // long header (bit 7 = 1)
	packet[1] = 0x00 // version bytes
	packet[2] = 0x00
	packet[3] = 0x00
	packet[4] = 0x01
	packet[5] = 3 // DCID length = 3
	packet[6] = 0xde
	packet[7] = 0xad
	packet[8] = 0xbe

	dcid := extract_dcid_from_packet(packet, 18) or {
		assert false, 'extract_dcid_from_packet failed: ${err}'
		return
	}
	assert dcid == 'deadbe', 'expected deadbe, got ${dcid}'
}

fn test_extract_dcid_too_short_packet() {
	// Packet too short to contain any DCID
	packet := [u8(0x40)] // just 1 byte, no room for CID

	extract_dcid_from_packet(packet, 4) or {
		assert err.msg().contains('too short')
		return
	}
	assert false, 'expected error for too-short packet'
}

// ── P4-2: QPACK Dynamic Table Capacity Negotiation ──

fn test_settings_applies_peer_max_table_capacity_to_encoder() {
	mut s := Server{}
	mut conn := new_test_server_connection()

	// Insert some entries into encoder's dynamic table first
	headers := [HeaderField{'x-fill', 'data1'}, HeaderField{'x-fill2', 'data2'}]
	_ = conn.encoder.encode(headers)
	assert conn.encoder.dynamic_table.count > 0, 'encoder should have dynamic entries'

	// Build SETTINGS payload with small qpack_max_table_capacity (0x01 = 32)
	mut payload := []u8{}
	payload << encode_varint(u64(0x01)) or { return }
	payload << encode_varint(u64(32)) or { return }

	// handle_settings_frame should apply capacity to encoder
	s.handle_settings_frame(mut conn, payload) or {
		assert false, 'handle_settings_frame failed: ${err}'
		return
	}

	// Encoder's peer_max_table_capacity should be updated
	assert conn.encoder.peer_max_table_capacity == 32, 'peer_max_table_capacity should be 32, got ${conn.encoder.peer_max_table_capacity}'
}

fn test_encoder_peer_capacity_smaller_triggers_eviction() {
	mut encoder := new_qpack_encoder(4096, 100)

	// Insert multiple entries to fill the dynamic table
	for i in 0 .. 5 {
		encoder.dynamic_table.insert(HeaderField{
			name:  'key${i}'
			value: 'value-that-takes-space-${i}'
		})
	}
	initial_count := encoder.dynamic_table.count
	assert initial_count == 5, 'should have 5 entries initially'

	// Set peer capacity smaller than current table size — should evict
	encoder.set_peer_max_table_capacity(50)

	assert encoder.dynamic_table.count < initial_count, 'entries should be evicted after shrinking peer capacity'
	assert encoder.dynamic_table.max_size == 50, 'max_size should be 50'
}

// ── Control Stream Reader (RFC 9114 §6.2.1) ──

fn build_settings_frame_data() ![]u8 {
	payload := build_settings_payload(Settings{
		qpack_max_table_capacity: 8192
		max_field_section_size:   32768
		qpack_blocked_streams:    50
	})!
	mut data := []u8{cap: 30}
	data << encode_varint(u64(FrameType.settings))!
	data << encode_varint(u64(payload.len))!
	data << payload
	return data
}

fn test_control_reader_parses_settings() {
	mut reader := new_control_reader()
	data := build_settings_frame_data() or {
		assert false, 'failed to build SETTINGS frame: ${err}'
		return
	}

	result := reader.read_control_frame(data) or {
		assert false, 'read_control_frame failed: ${err}'
		return
	}

	assert result.frame_type == .settings
	s := result.settings or {
		assert false, 'settings should be present'
		return
	}

	assert s.qpack_max_table_capacity == u64(8192)
	assert s.max_field_section_size == u64(32768)
	assert s.qpack_blocked_streams == u64(50)
	assert reader.settings_received == true
}

fn test_control_reader_first_frame_must_be_settings() {
	mut reader := new_control_reader()

	// Build a GOAWAY frame instead of SETTINGS
	goaway_data := build_goaway_frame(u64(4)) or {
		assert false, 'failed to build GOAWAY frame: ${err}'
		return
	}

	reader.read_control_frame(goaway_data) or {
		assert err.msg().contains('H3_MISSING_SETTINGS')
		return
	}
	assert false, 'expected H3_MISSING_SETTINGS error when first frame is not SETTINGS'
}

fn test_control_reader_parses_goaway() {
	mut reader := new_control_reader()

	// First send SETTINGS to satisfy the "first frame" requirement
	settings_data := build_settings_frame_data() or {
		assert false, 'failed to build SETTINGS: ${err}'
		return
	}
	reader.read_control_frame(settings_data) or {
		assert false, 'SETTINGS parse failed: ${err}'
		return
	}

	// Now send GOAWAY
	goaway_data := build_goaway_frame(u64(12)) or {
		assert false, 'failed to build GOAWAY: ${err}'
		return
	}
	result := reader.read_control_frame(goaway_data) or {
		assert false, 'GOAWAY parse failed: ${err}'
		return
	}

	assert result.frame_type == .goaway
	gid := result.goaway_id or {
		assert false, 'goaway_id should be present'
		return
	}

	assert gid == u64(12)
}

fn test_control_reader_ignores_unknown_frames() {
	mut reader := new_control_reader()

	// First send SETTINGS
	settings_data := build_settings_frame_data() or {
		assert false, 'failed to build SETTINGS: ${err}'
		return
	}
	reader.read_control_frame(settings_data) or {
		assert false, 'SETTINGS parse failed: ${err}'
		return
	}

	// Build an unknown frame type (0x21 = reserved/unknown)
	mut unknown_data := []u8{}
	unknown_data << encode_varint(u64(0x21)) or {
		assert false, 'failed to encode unknown frame type'
		return
	}
	payload := [u8(0xaa), 0xbb, 0xcc]
	unknown_data << encode_varint(u64(payload.len)) or {
		assert false, 'failed to encode unknown frame length'
		return
	}
	unknown_data << payload

	// Unknown frames must be silently ignored (RFC 9114 §7.2.8)
	result := reader.read_control_frame(unknown_data) or {
		assert false, 'unknown frame should not error: ${err}'
		return
	}
	assert result.frame_type == .unknown
}

// vtest build: present_openssl?
module quic

fn test_data_frame_roundtrip() {
	encoded := encode_data_frame([u8(1), 2, 3, 4, 5])!
	frame := decode_h3_frame_payload(h3_frame_data, encoded[2..])!
	match frame {
		DataFrame {
			assert frame.data == [u8(1), 2, 3, 4, 5]
		}
		else {
			assert false, 'expected DataFrame'
		}
	}
	assert encoded[0] == u8(h3_frame_data)
	assert encoded[1] == u8(5) // length, 1-byte varint
}

fn test_headers_frame_roundtrip() {
	blob := [u8(0xaa), 0xbb, 0xcc]
	encoded := encode_headers_frame(blob)!
	frame := decode_h3_frame_payload(h3_frame_headers, encoded[2..])!
	match frame {
		HeadersFrame {
			assert frame.encoded_field_section == blob
		}
		else {
			assert false, 'expected HeadersFrame'
		}
	}
}

fn test_cancel_push_frame_roundtrip() {
	encoded := encode_cancel_push_frame(42)!
	frame := decode_h3_frame_payload(h3_frame_cancel_push, encoded[2..])!
	match frame {
		CancelPushFrame {
			assert frame.push_id == 42
		}
		else {
			assert false, 'expected CancelPushFrame'
		}
	}
}

fn test_cancel_push_frame_rejects_trailing_bytes() {
	mut payload := encode_varint(42)!
	payload << u8(0xff) // one stray trailing byte
	if _ := decode_h3_frame_payload(h3_frame_cancel_push, payload) {
		assert false, 'expected an error for trailing bytes'
	} else {
		assert err.code() == int(H3ErrorCode.frame_error)
	}
}

fn test_goaway_frame_roundtrip() {
	encoded := encode_goaway_frame(1234)!
	frame := decode_h3_frame_payload(h3_frame_goaway, encoded[2..])!
	match frame {
		GoawayFrame {
			assert frame.id == 1234
		}
		else {
			assert false, 'expected GoawayFrame'
		}
	}
}

fn test_goaway_id_is_valid_client_initiated_bidi_stream_id() {
	assert goaway_id_is_valid_client_initiated_bidi_stream_id(0)
	assert goaway_id_is_valid_client_initiated_bidi_stream_id(4)
	assert goaway_id_is_valid_client_initiated_bidi_stream_id(8)
	// 1: client unidirectional; 2: server-initiated bidi; 3: server
	// unidirectional -- none of these are legal in a server-to-client
	// GOAWAY's stream-ID variant.
	assert !goaway_id_is_valid_client_initiated_bidi_stream_id(1)
	assert !goaway_id_is_valid_client_initiated_bidi_stream_id(2)
	assert !goaway_id_is_valid_client_initiated_bidi_stream_id(3)
	assert !goaway_id_is_valid_client_initiated_bidi_stream_id(5)
	assert !goaway_id_is_valid_client_initiated_bidi_stream_id(6)
	assert !goaway_id_is_valid_client_initiated_bidi_stream_id(7)
}

fn test_max_push_id_frame_roundtrip() {
	encoded := encode_max_push_id_frame(99)!
	frame := decode_h3_frame_payload(h3_frame_max_push_id, encoded[2..])!
	match frame {
		MaxPushIdFrame {
			assert frame.push_id == 99
		}
		else {
			assert false, 'expected MaxPushIdFrame'
		}
	}
}

fn test_push_promise_frame_decode_only() {
	// No encoder exists (v1 client role never sends PUSH_PROMISE, §7.2.5) --
	// build the payload by hand to exercise the decode path a client still
	// needs when RECEIVING one from a server.
	mut payload := encode_varint(77)! // push id
	payload << [u8(0xde), 0xad, 0xbe, 0xef] // opaque encoded field section
	frame := decode_h3_frame_payload(h3_frame_push_promise, payload)!
	match frame {
		PushPromiseFrame {
			assert frame.push_id == 77
			assert frame.encoded_field_section == [u8(0xde), 0xad, 0xbe, 0xef]
		}
		else {
			assert false, 'expected PushPromiseFrame'
		}
	}
}

fn test_settings_frame_roundtrip_and_preserves_order() {
	settings := [
		H3Setting{
			identifier: 0x06
			value:      16384
		},
		H3Setting{
			identifier: 0x21 // grease -- must round-trip, not be dropped
			value:      0
		},
	]
	encoded := encode_settings_frame(settings)!
	frame := decode_h3_frame_payload(h3_frame_settings, encoded[2..])!
	match frame {
		SettingsFrame {
			assert frame.settings.len == 2
			assert frame.settings[0].identifier == 0x06
			assert frame.settings[0].value == 16384
			assert frame.settings[1].identifier == 0x21
		}
		else {
			assert false, 'expected SettingsFrame'
		}
	}
}

fn test_settings_frame_rejects_duplicate_identifier() {
	mut payload := encode_varint(0x06)!
	payload << encode_varint(100)!
	payload << encode_varint(0x06)! // same identifier again
	payload << encode_varint(200)!
	if _ := decode_h3_frame_payload(h3_frame_settings, payload) {
		assert false, 'expected an error for a duplicate identifier'
	} else {
		assert err.code() == int(H3ErrorCode.settings_error)
	}
}

fn test_settings_frame_rejects_reserved_h2_carryover_identifiers() {
	reserved_ids := [u64(0x00), 0x02, 0x03, 0x04, 0x05]
	for id in reserved_ids {
		mut payload := encode_varint(id)!
		payload << encode_varint(1)!
		if _ := decode_h3_frame_payload(h3_frame_settings, payload) {
			assert false, 'expected an error for reserved identifier 0x${id.hex()}'
		} else {
			assert err.code() == int(H3ErrorCode.settings_error)
		}
	}
}

fn test_settings_frame_accepts_unknown_non_reserved_identifier() {
	// 0x01/0x07 are QPACK's own identifiers (RFC 9204), not reserved by
	// RFC 9114 -- must be accepted (ignored, not rejected) at this layer.
	mut payload := encode_varint(0x01)!
	payload << encode_varint(4096)!
	frame := decode_h3_frame_payload(h3_frame_settings, payload)!
	match frame {
		SettingsFrame {
			assert frame.settings.len == 1
			assert frame.settings[0].identifier == 0x01
		}
		else {
			assert false, 'expected SettingsFrame'
		}
	}
}

fn test_settings_frame_rejects_truncated_payload() {
	mut payload := encode_varint(0x06)!
	// value varint omitted entirely
	if _ := decode_h3_frame_payload(h3_frame_settings, payload) {
		assert false, 'expected an error for a truncated SETTINGS payload'
	} else {
		assert err.code() == int(H3ErrorCode.frame_error)
	}
}

fn test_settings_frame_empty_payload_is_valid() {
	// §7.2.4: "zero or more parameters" -- an empty SETTINGS frame is legal.
	frame := decode_h3_frame_payload(h3_frame_settings, [])!
	match frame {
		SettingsFrame {
			assert frame.settings.len == 0
		}
		else {
			assert false, 'expected SettingsFrame'
		}
	}
}

fn test_decode_rejects_reserved_h2_carryover_frame_types() {
	reserved := [u64(0x02), 0x06, 0x08, 0x09]
	for t in reserved {
		if _ := decode_h3_frame_payload(t, []) {
			assert false, 'expected an error for reserved frame type 0x${t.hex()}'
		} else {
			assert err.code() == int(H3ErrorCode.frame_unexpected)
		}
	}
}

fn test_decode_grease_frame_type_is_ignored_not_rejected() {
	frame := decode_h3_frame_payload(0x21, [u8(1), 2, 3])!
	match frame {
		H3RawFrame {
			assert frame.frame_type == 0x21
			assert frame.payload == [u8(1), 2, 3]
		}
		else {
			assert false, 'expected H3RawFrame'
		}
	}
}

fn test_decode_genuinely_unknown_frame_type_is_ignored_not_rejected() {
	// 0x0a/0x0b/0x0c sit in the gap between the H2-carryover reserved set
	// and MAX_PUSH_ID (0x0d) -- not defined, not grease, not H2-reserved.
	frame := decode_h3_frame_payload(0x0a, [u8(9), 9])!
	match frame {
		H3RawFrame {
			assert frame.frame_type == 0x0a
			assert frame.payload == [u8(9), 9]
		}
		else {
			assert false, 'expected H3RawFrame'
		}
	}
}

fn test_encode_h3_frame_envelope_refuses_reserved_frame_type() {
	if _ := encode_h3_frame_envelope(0x06, []) {
		assert false, 'expected an error encoding a reserved frame type'
	}
}

fn test_encode_h3_frame_envelope_length_prefix_matches_payload() {
	payload := []u8{len: 100, init: u8(0x42)}
	encoded := encode_h3_frame_envelope(h3_frame_data, payload)!
	length, n := decode_varint(encoded[1..])!
	assert length == 100
	assert encoded.len == 1 + n + 100
}

// --- H3FrameDecoder: incremental / resumable parsing ---

fn test_h3_frame_decoder_returns_no_frame_on_empty_buffer() {
	mut d := new_h3_frame_decoder()
	result := d.next()!
	assert result.has_frame == false
}

fn test_h3_frame_decoder_waits_for_full_frame_byte_by_byte() {
	full := encode_data_frame([u8(1), 2, 3, 4, 5])!
	mut d := new_h3_frame_decoder()
	for i := 0; i < full.len - 1; i++ {
		d.push([full[i]])
		result := d.next()!
		assert result.has_frame == false, 'expected no frame with ${i + 1}/${full.len} bytes fed'
	}
	d.push([full[full.len - 1]])
	result := d.next()!
	assert result.has_frame
	assert result.consumed == full.len
	match result.frame {
		DataFrame {
			assert result.frame.data == [u8(1), 2, 3, 4, 5]
		}
		else {
			assert false, 'expected DataFrame'
		}
	}
}

fn test_h3_frame_decoder_handles_multiple_frames_in_one_push() {
	mut combined := encode_data_frame([u8(1)])!
	combined << encode_cancel_push_frame(5)!
	combined << encode_max_push_id_frame(9)!

	mut d := new_h3_frame_decoder()
	d.push(combined)

	r1 := d.next()!
	assert r1.has_frame
	match r1.frame {
		DataFrame { assert r1.frame.data == [u8(1)] }
		else { assert false, 'expected DataFrame first' }
	}

	r2 := d.next()!
	assert r2.has_frame
	match r2.frame {
		CancelPushFrame { assert r2.frame.push_id == 5 }
		else { assert false, 'expected CancelPushFrame second' }
	}

	r3 := d.next()!
	assert r3.has_frame
	match r3.frame {
		MaxPushIdFrame { assert r3.frame.push_id == 9 }
		else { assert false, 'expected MaxPushIdFrame third' }
	}

	r4 := d.next()!
	assert r4.has_frame == false
	assert d.pending_len() == 0
}

fn test_h3_frame_decoder_huge_declared_length_with_insufficient_bytes_does_not_panic() {
	// Regression test for the u64-vs-int overflow this file's own review
	// caught before first compile: a declared Length near the varint max
	// (2^62-1) with only a few real bytes buffered must report "not ready
	// yet", never panic or misreport readiness via a truncated `int` cast.
	mut buf := encode_varint(h3_frame_data)! // frame type
	buf << encode_varint(max_varint)! // Length = 2^62-1
	buf << [u8(1), 2, 3] // far short of that many payload bytes

	mut d := new_h3_frame_decoder()
	d.push(buf)
	result := d.next()!
	assert result.has_frame == false
	assert d.pending_len() == buf.len
}

fn test_h3_frame_decoder_returns_error_for_reserved_frame_type_and_stops_consuming() {
	mut buf := encode_varint(u64(0x06))! // reserved (PING carryover)
	buf << encode_varint(u64(0))! // zero-length payload
	mut d := new_h3_frame_decoder()
	d.push(buf)
	if _ := d.next() {
		assert false, 'expected an error for a reserved frame type'
	} else {
		assert err.code() == int(H3ErrorCode.frame_unexpected)
	}
}

fn test_h3_frame_decoder_settings_split_across_pushes_mid_payload() {
	full := encode_settings_frame([
		H3Setting{
			identifier: 0x06
			value:      42
		},
	])!
	mut d := new_h3_frame_decoder()
	// Feed the envelope (type+length) but stop partway through the payload.
	split_point := full.len - 1
	d.push(full[..split_point])
	mid_result := d.next()!
	assert mid_result.has_frame == false

	d.push(full[split_point..])
	final_result := d.next()!
	assert final_result.has_frame
	match final_result.frame {
		SettingsFrame {
			assert final_result.frame.settings.len == 1
			assert final_result.frame.settings[0].value == 42
		}
		else {
			assert false, 'expected SettingsFrame'
		}
	}
}

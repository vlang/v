module quic

fn test_classify_h3_unidirectional_stream_type_known_kinds() {
	assert classify_h3_unidirectional_stream_type(0x00) == .control
	assert classify_h3_unidirectional_stream_type(0x01) == .push
	assert classify_h3_unidirectional_stream_type(0x21) == .reserved
	assert classify_h3_unidirectional_stream_type(0x40) == .reserved
	// 0x02/0x03 are QPACK's encoder/decoder stream types (RFC 9204) --
	// unrecognized here since QPACK is Phase 11, but MUST NOT be grease.
	assert classify_h3_unidirectional_stream_type(0x02) == .unknown
	assert classify_h3_unidirectional_stream_type(0x03) == .unknown
	assert classify_h3_unidirectional_stream_type(0xff) == .unknown
}

fn test_parse_h3_unidirectional_stream_header_control() {
	buf := [u8(0x00)]
	header := parse_h3_unidirectional_stream_header(buf) or { panic('expected a decoded header') }
	assert header.kind == .control
	assert header.raw_type == 0x00
	assert header.push_id == none
	assert header.consumed == 1
}

fn test_parse_h3_unidirectional_stream_header_push() {
	encoded := encode_h3_push_stream_header(12345)!
	header := parse_h3_unidirectional_stream_header(encoded) or {
		panic('expected a decoded header')
	}
	assert header.kind == .push
	assert header.raw_type == 0x01
	assert header.push_id? == u64(12345)
	assert header.consumed == encoded.len
}

fn test_parse_h3_unidirectional_stream_header_push_id_at_varint_max() {
	encoded := encode_h3_push_stream_header(max_varint)!
	header := parse_h3_unidirectional_stream_header(encoded) or {
		panic('expected a decoded header')
	}
	assert header.push_id? == max_varint
	assert header.consumed == encoded.len
}

fn test_parse_h3_unidirectional_stream_header_reserved() {
	buf := [u8(0x21)]
	header := parse_h3_unidirectional_stream_header(buf) or { panic('expected a decoded header') }
	assert header.kind == .reserved
}

// h3_header_is_none is a small test helper: true iff parsing buf returns
// `none` (not a decoded header) -- V's Option type has no direct `== none`
// comparison on a plain variable, so this wraps the idiomatic if-unwrap.
fn h3_header_is_none(buf []u8) bool {
	if _ := parse_h3_unidirectional_stream_header(buf) {
		return false
	}
	return true
}

fn test_parse_h3_unidirectional_stream_header_returns_none_on_empty_buffer() {
	buf := []u8{}
	assert h3_header_is_none(buf)
}

fn test_parse_h3_unidirectional_stream_header_returns_none_on_partial_type_varint() {
	// 0xC0-prefixed first byte declares an 8-byte varint -- one byte alone
	// is not enough, and this must be "wait for more", not an error.
	buf := [u8(0xC0)]
	assert h3_header_is_none(buf)
}

fn test_parse_h3_unidirectional_stream_header_returns_none_on_partial_push_id() {
	// Stream Type byte (push, 1 byte) present, but the Push ID varint's
	// declared length exceeds what's buffered so far.
	mut buf := [u8(h3_push_stream_type)]
	buf << u8(0x40) // 2-byte varint prefix, second byte missing
	assert h3_header_is_none(buf)
}

fn test_parse_h3_unidirectional_stream_header_incremental_feed_byte_by_byte() {
	full := encode_h3_push_stream_header(999)!
	for n in 1 .. full.len {
		partial := full[..n]
		assert h3_header_is_none(partial), 'expected none with only ${n}/${full.len} bytes buffered'
	}
	header := parse_h3_unidirectional_stream_header(full) or {
		panic('expected success with the full buffer')
	}
	assert header.push_id? == u64(999)
	assert header.consumed == full.len
}

fn test_encode_h3_control_stream_header_roundtrip() {
	encoded := encode_h3_control_stream_header()!
	header := parse_h3_unidirectional_stream_header(encoded) or {
		panic('expected a decoded header')
	}
	assert header.kind == .control
	assert header.consumed == encoded.len
}

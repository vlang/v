module quic

fn test_long_header_initial_round_trip() {
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    [u8(1), 2, 3, 4, 5, 6, 7, 8]
		scid:    [u8(9), 10, 11, 12]
		token:   [u8(0xAA), 0xBB, 0xCC]
		length:  1200
	}
	encoded := encode_long_header(h, 0, 3)!
	parsed, consumed := parse_long_header(encoded)!
	assert consumed == encoded.len
	assert parsed.typ == .initial
	assert parsed.version == quic_v1
	assert parsed.dcid == h.dcid
	assert parsed.scid == h.scid
	assert parsed.token == h.token
	assert parsed.length == h.length
}

fn test_long_header_handshake_has_no_token() {
	h := QuicLongHeader{
		typ:     .handshake
		version: quic_v1
		dcid:    [u8(1), 2, 3, 4]
		scid:    [u8(5), 6, 7, 8]
		length:  512
	}
	encoded := encode_long_header(h, 0, 1)!
	parsed, consumed := parse_long_header(encoded)!
	assert consumed == encoded.len
	assert parsed.typ == .handshake
	assert parsed.token.len == 0
	assert parsed.length == 512
}

fn test_long_header_zero_length_connection_ids() {
	// RFC 9000 §17.2 explicitly permits 0-length DCID/SCID.
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    []u8{}
		scid:    []u8{}
		length:  100
	}
	encoded := encode_long_header(h, 0, 0)!
	parsed, _ := parse_long_header(encoded)!
	assert parsed.dcid.len == 0
	assert parsed.scid.len == 0
}

fn test_long_header_type_bits_round_trip_all_types() {
	types := [LongPacketType.initial, LongPacketType.zero_rtt, LongPacketType.handshake]
	for t in types {
		h := QuicLongHeader{
			typ:     t
			version: quic_v1
			dcid:    [u8(1)]
			scid:    [u8(2)]
			length:  10
		}
		encoded := encode_long_header(h, 0, 0)!
		parsed, _ := parse_long_header(encoded)!
		assert parsed.typ == t
	}
}

fn test_long_header_rejects_oversized_token_len_without_truncation() {
	// token_len is a QUIC varint (up to 2^62-1); casting it to V's 32-bit
	// `int` before bounds-checking would silently wrap a huge declared
	// length down to something tiny, letting the parser "succeed" with a
	// corrupted result instead of rejecting the packet. Regression for a
	// truncation bug caught in /vreview before this landed.
	mut buf := []u8{}
	buf << u8(0x80 | 0x40) // long header, fixed bit, type=initial (00), rest 0
	buf << [u8(0), 0, 0, 1] // version = quic_v1
	buf << u8(0) // dcid_len = 0
	buf << u8(0) // scid_len = 0
	// token_len varint encoding 0x100000001 (2^32 + 1) as an 8-byte varint:
	// top 2 bits `11` (8-byte class) on the first byte, then the 62-bit value.
	huge_token_len := u64(0x1_0000_0001)
	buf << encode_varint(huge_token_len)!
	// Deliberately do NOT supply anywhere near that many token bytes.
	buf << [u8(1), 2, 3]

	parse_long_header(buf) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false, 'expected a truncation error, got a (mis)parsed header instead'
}

fn test_long_header_rejects_truncated_buffer() {
	parse_long_header([u8(0x80), 0, 0, 0]) or {
		assert err.msg().contains('too short')
		return
	}
	assert false, 'expected an error for a truncated long header'
}

fn test_short_header_round_trip_and_zero_length_dcid() {
	// Zero-length DCID short header: 1 byte total before the (still
	// protected) packet number.
	buf := [u8(0x40)]
	parsed, consumed := parse_short_header(buf, 0)!
	assert consumed == 1
	assert parsed.dcid.len == 0

	dcid := [u8(0xDE), 0xAD, 0xBE, 0xEF]
	mut buf2 := [u8(0x40)]
	buf2 << dcid
	parsed2, consumed2 := parse_short_header(buf2, dcid.len)!
	assert consumed2 == 1 + dcid.len
	assert parsed2.dcid == dcid
}

fn test_short_header_rejects_long_header_bytes() {
	parse_short_header([u8(0x80), 1, 2, 3], 0) or {
		assert err.msg().contains('not a short header')
		return
	}
	assert false, 'expected an error when the top bit is set'
}

fn test_peek_header_form() {
	assert peek_header_form([u8(0x80)])! == HeaderForm.long
	assert peek_header_form([u8(0x40)])! == HeaderForm.short
}

fn test_version_negotiation_parse() {
	mut buf := []u8{}
	buf << u8(0x80) // form bit set, rest of byte is unspecified for VN
	buf << u8(0) // version = 0 (4 bytes)
	buf << u8(0)
	buf << u8(0)
	buf << u8(0)
	dcid := [u8(1), 2, 3, 4]
	scid := [u8(5), 6]
	buf << u8(dcid.len)
	buf << dcid
	buf << u8(scid.len)
	buf << scid
	// supported versions list: quic_v1 and a made-up future version
	buf << [u8(0), 0, 0, 1]
	buf << [u8(0xFF), 0, 0, 1]

	vn := parse_version_negotiation(buf)!
	assert vn.dcid == dcid
	assert vn.scid == scid
	assert vn.versions.len == 2
	assert vn.versions[0] == quic_v1
	assert vn.versions[1] == u32(0xFF000001)
}

fn test_version_negotiation_rejects_nonzero_version() {
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    [u8(1)]
		scid:    [u8(2)]
		length:  10
	}
	encoded := encode_long_header(h, 0, 0)!
	parse_version_negotiation(encoded) or {
		assert err.msg().contains('not a version negotiation packet')
		return
	}
	assert false, 'expected an error for a non-zero version field'
}

fn test_long_header_lists_v1_only_and_matches_constant() {
	assert quic_v1 == u32(0x00000001)
}

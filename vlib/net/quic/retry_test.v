module quic

fn build_test_retry_packet(dcid []u8, scid []u8, token []u8, original_dcid []u8) ![]u8 {
	h := QuicLongHeader{
		typ:     .retry
		version: quic_v1
		dcid:    dcid
		scid:    scid
		token:   token // ignored by encode_long_header for .retry -- appended manually below
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << token
	tag := compute_retry_integrity_tag(original_dcid, buf)!
	buf << tag
	return buf
}

fn test_compute_retry_integrity_tag_is_16_bytes() {
	tag := compute_retry_integrity_tag([u8(1), 2, 3, 4], [u8(5), 6, 7])!
	assert tag.len == retry_integrity_tag_len
}

fn test_compute_retry_integrity_tag_is_deterministic() {
	tag1 := compute_retry_integrity_tag([u8(1), 2, 3, 4], [u8(5), 6, 7])!
	tag2 := compute_retry_integrity_tag([u8(1), 2, 3, 4], [u8(5), 6, 7])!
	assert tag1 == tag2
}

fn test_retry_packet_round_trip_verifies() {
	original_dcid := [u8(0xaa), 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x11, 0x22]
	new_dcid := [u8(1), 2, 3, 4, 5, 6, 7, 8]
	new_scid := [u8(9), 10, 11, 12]
	token := 'retry-token-bytes'.bytes()

	packet := build_test_retry_packet(new_dcid, new_scid, token, original_dcid)!

	parsed := parse_retry_packet(packet)!
	assert parsed.dcid == new_dcid
	assert parsed.scid == new_scid
	assert parsed.retry_token == token
	assert parsed.integrity_tag.len == retry_integrity_tag_len

	ok := verify_retry_integrity_tag(original_dcid, packet)!
	assert ok
}

fn test_retry_packet_rejects_tampered_tag() {
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	mut packet := build_test_retry_packet([u8(1), 2], [u8(3), 4], 'tok'.bytes(), original_dcid)!
	packet[packet.len - 1] ^= 0x01

	ok := verify_retry_integrity_tag(original_dcid, packet)!
	assert ok == false
}

fn test_retry_packet_rejects_tampered_token() {
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	mut packet := build_test_retry_packet([u8(1), 2], [u8(3), 4], 'tok'.bytes(), original_dcid)!
	// Flip a byte inside the token, well before the trailing tag.
	token_byte_index := packet.len - retry_integrity_tag_len - 1
	packet[token_byte_index] ^= 0x01

	ok := verify_retry_integrity_tag(original_dcid, packet)!
	assert ok == false
}

fn test_retry_packet_rejects_wrong_original_dcid() {
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	packet := build_test_retry_packet([u8(1), 2], [u8(3), 4], 'tok'.bytes(), original_dcid)!

	wrong_original_dcid := [u8(0xaa), 0xbb, 0xce] // one bit different
	ok := verify_retry_integrity_tag(wrong_original_dcid, packet)!
	assert ok == false
}

fn test_parse_retry_packet_rejects_non_retry_type() {
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    [u8(1), 2]
		scid:    [u8(3), 4]
		token:   []u8{}
		length:  20
	}
	buf := encode_long_header(h, 0, 0)!
	parse_retry_packet(buf) or {
		assert err.msg().contains('not a Retry packet')
		return
	}
	assert false, 'expected a non-Retry packet to be rejected'
}

fn test_parse_retry_packet_rejects_truncated_packet() {
	h := QuicLongHeader{
		typ:     .retry
		version: quic_v1
		dcid:    [u8(1), 2]
		scid:    [u8(3), 4]
		token:   []u8{}
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << [u8(1), 2, 3] // far fewer than the required 16-byte tag
	parse_retry_packet(buf) or {
		assert err.msg().contains('missing')
		return
	}
	assert false, 'expected a truncated Retry packet to be rejected'
}

fn test_verify_retry_integrity_tag_rejects_too_short_packet() {
	verify_retry_integrity_tag([u8(1)], [u8(1), 2, 3]) or {
		assert err.msg().contains('shorter than the retry integrity tag')
		return
	}
	assert false, 'expected a too-short packet to be rejected'
}

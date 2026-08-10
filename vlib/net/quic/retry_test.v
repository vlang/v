// vtest build: present_openssl?
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
	original_scid := [u8(0x55), 0x66, 0x77, 0x88]
	new_scid := [u8(9), 10, 11, 12]
	token := 'retry-token-bytes'.bytes()

	// The Retry's own DCID must echo original_scid (RFC 9000 §17.2.5.1's
	// echo requirement); its own SCID is the server's genuinely new value.
	packet := build_test_retry_packet(original_scid, new_scid, token, original_dcid)!

	// verify_retry_integrity_tag first, matching parse_retry_packet's own
	// documented contract ("Callers MUST call verify_retry_integrity_tag
	// first and only call this function when it returns true") -- this test
	// models the intended production call order, not just independent
	// checks on the same fixture.
	ok := verify_retry_integrity_tag(original_dcid, packet, false)!
	assert ok

	parsed := parse_retry_packet(packet, original_dcid, original_scid)!
	assert parsed.dcid == original_scid
	assert parsed.scid == new_scid
	assert parsed.retry_token == token
	assert parsed.integrity_tag.len == retry_integrity_tag_len
}

fn test_retry_packet_rejects_tampered_tag() {
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	original_scid := [u8(1), 2]
	mut packet := build_test_retry_packet(original_scid, [u8(3), 4], 'tok'.bytes(), original_dcid)!
	packet[packet.len - 1] ^= 0x01

	ok := verify_retry_integrity_tag(original_dcid, packet, false)!
	assert ok == false
}

fn test_retry_packet_rejects_tampered_token() {
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	original_scid := [u8(1), 2]
	mut packet := build_test_retry_packet(original_scid, [u8(3), 4], 'tok'.bytes(), original_dcid)!
	// Flip a byte inside the token, well before the trailing tag.
	token_byte_index := packet.len - retry_integrity_tag_len - 1
	packet[token_byte_index] ^= 0x01

	ok := verify_retry_integrity_tag(original_dcid, packet, false)!
	assert ok == false
}

fn test_retry_packet_rejects_wrong_original_dcid() {
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	original_scid := [u8(1), 2]
	packet := build_test_retry_packet(original_scid, [u8(3), 4], 'tok'.bytes(), original_dcid)!

	wrong_original_dcid := [u8(0xaa), 0xbb, 0xce] // one bit different
	ok := verify_retry_integrity_tag(wrong_original_dcid, packet, false)!
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
	parse_retry_packet(buf, [u8(99)], [u8(1), 2]) or {
		assert err.msg().contains('not a Retry packet')
		return
	}
	assert false, 'expected a non-Retry packet to be rejected'
}

fn test_parse_retry_packet_rejects_truncated_packet() {
	original_scid := [u8(1), 2]
	h := QuicLongHeader{
		typ:     .retry
		version: quic_v1
		dcid:    original_scid
		scid:    [u8(3), 4]
		token:   []u8{}
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << [u8(1), 2, 3] // far fewer than the required 16-byte tag
	parse_retry_packet(buf, [u8(99)], original_scid) or {
		assert err.msg().contains('missing')
		return
	}
	assert false, 'expected a truncated Retry packet to be rejected'
}

fn test_parse_retry_packet_rejects_empty_token() {
	// RFC 9000 §17.2.5.2: "A client MUST discard a Retry packet with a
	// zero-length Retry Token field."
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	original_scid := [u8(1), 2]
	packet := build_test_retry_packet(original_scid, [u8(3), 4], []u8{}, original_dcid)!
	parse_retry_packet(packet, original_dcid, original_scid) or {
		assert err.msg().contains('zero-length')
		return
	}
	assert false, 'expected a Retry packet with an empty token to be rejected'
}

fn test_parse_retry_packet_rejects_scid_equal_to_original_dcid() {
	// RFC 9000 §17.2.5.1: "A client MUST discard a Retry packet that
	// contains a Source Connection ID field that is identical to the
	// Destination Connection ID field of its Initial packet."
	original_dcid := [u8(0xaa), 0xbb, 0xcc, 0xdd]
	original_scid := [u8(1), 2]
	// SCID (the server's supposed new CID) is identical to the client's own
	// original DCID -- not a real server-chosen replacement.
	packet := build_test_retry_packet(original_scid, original_dcid, 'tok'.bytes(), original_dcid)!
	parse_retry_packet(packet, original_dcid, original_scid) or {
		assert err.msg().contains('Source Connection ID')
		return
	}
	assert false, 'expected a Retry packet whose SCID equals the original DCID to be rejected'
}

fn test_parse_retry_packet_rejects_dcid_not_echoing_original_scid() {
	// RFC 9000 §17.2.5.1: "The server populates the Destination Connection
	// ID with the connection ID that the client included in the Source
	// Connection ID of the Initial packet." A Retry whose DCID does NOT
	// echo this client's own original SCID cannot be a genuine response to
	// this client's own Initial -- an attacker who knows (or guesses) the
	// client's original DCID could otherwise forge a Retry that passes the
	// (publicly-known-key) integrity tag but is addressed to the wrong
	// connection attempt.
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	original_scid := [u8(1), 2]
	wrong_dcid := [u8(9), 9]
	packet := build_test_retry_packet(wrong_dcid, [u8(3), 4], 'tok'.bytes(), original_dcid)!
	parse_retry_packet(packet, original_dcid, original_scid) or {
		assert err.msg().contains('Destination Connection ID')
		return
	}
	assert false, 'expected a Retry packet whose DCID does not echo the original SCID to be rejected'
}

fn test_verify_retry_integrity_tag_discards_too_short_packet() {
	// A packet shorter than the tag itself cannot be verified at all --
	// RFC 9000 §17.2.5.2 requires silent discard here, the same as a tag
	// mismatch, not an error a caller might propagate as connection-fatal.
	ok := verify_retry_integrity_tag([u8(1)], [u8(1), 2, 3], false)!
	assert ok == false
}

fn test_verify_retry_integrity_tag_discards_when_already_processed_other_packet() {
	// RFC 9000 §17.2.5.2: "After the client has received and processed an
	// Initial or Retry packet from the server, it MUST discard any
	// subsequent Retry packets that it receives." This must hold even for a
	// packet whose tag is otherwise genuinely valid -- already_processed
	// gates BEFORE tag verification, not as a tiebreaker after it.
	original_dcid := [u8(0xaa), 0xbb, 0xcc]
	original_scid := [u8(1), 2]
	packet := build_test_retry_packet(original_scid, [u8(3), 4], 'tok'.bytes(), original_dcid)!

	// Sanity: this exact packet verifies true when nothing has been
	// processed yet, so the discard below is attributable to the flag.
	assert verify_retry_integrity_tag(original_dcid, packet, false)!

	ok := verify_retry_integrity_tag(original_dcid, packet, true)!
	assert ok == false
}

// vtest build: present_openssl?
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

// test_long_header_rejects_clear_fixed_bit and
// test_short_header_rejects_clear_fixed_bit are regression tests for a
// Codex finding (vlang/v#27680 pullrequestreview-4782360314): RFC 9000
// §17.2/§17.3.1's Fixed Bit (0x40) was never checked on either header
// parser -- "Packets containing a zero value for this bit are not valid
// packets in this version and MUST be discarded." A packet with Header
// Form set but Fixed Bit clear was silently parsed as a normal long/short
// header instead of being rejected outright.
fn test_long_header_rejects_clear_fixed_bit() {
	// Header Form set (0x80), Fixed Bit CLEAR, type bits = 00 (initial),
	// otherwise a well-formed v1 header.
	mut buf := []u8{}
	buf << u8(0x80)
	buf << u8(0)
	buf << u8(0)
	buf << u8(0)
	buf << u8(1) // version = quic_v1
	buf << u8(0) // dcid_len
	buf << u8(0) // scid_len
	buf << u8(0) // token_len varint
	buf << u8(0) // length varint
	parse_long_header(buf) or {
		assert err.msg().contains('Fixed Bit')
		return
	}
	assert false, 'expected an error for a long header with the Fixed Bit clear'
}

fn test_short_header_rejects_clear_fixed_bit() {
	// Header Form clear (0), Fixed Bit ALSO clear -- 0x00.
	parse_short_header([u8(0x00)], 0) or {
		assert err.msg().contains('Fixed Bit')
		return
	}
	assert false, 'expected an error for a short header with the Fixed Bit clear'
}

// The three tests below are regression tests for Codex findings on
// vlang/v#27680 (pullrequestreview-4781706846): QUIC v1's own 20-byte
// connection-ID limit (RFC 9000 §17.2) wasn't enforced on either the
// encode or parse path (only the wire format's own 255-byte length-field
// limit was), and long-header packet-type decoding applied v1's bit
// mapping unconditionally regardless of the version field.

fn test_encode_long_header_rejects_v1_cid_over_20_bytes() {
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    []u8{len: 21, init: 0xAB}
		scid:    []u8{len: 4}
		length:  1200
	}
	encode_long_header(h, 0, 3) or {
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a QUIC v1 DCID longer than 20 bytes'
}

fn test_parse_long_header_rejects_v1_cid_over_20_bytes() {
	// Hand-build a long header with a 21-byte dcid_len byte -- the wire
	// format itself has no problem with this (up to 255), only QUIC v1's
	// own protocol limit does.
	mut buf := []u8{}
	buf << u8(0x80 | 0x40) // long header, Initial type bits = 00
	buf << u8(quic_v1 >> 24)
	buf << u8(quic_v1 >> 16)
	buf << u8(quic_v1 >> 8)
	buf << u8(quic_v1)
	buf << u8(21) // dcid_len
	buf << []u8{len: 21, init: 0xCD}
	buf << u8(0) // scid_len
	parse_long_header(buf) or {
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a v1 dcid_len of 21 bytes'
}

fn test_parse_long_header_rejects_unsupported_version() {
	mut buf := []u8{}
	buf << u8(0x80 | 0x40)
	// A hypothetical QUIC v2 (0x6b3343cf is the actual registered v2
	// value, RFC 9369 -- any nonzero, non-v1 value proves the point).
	buf << u8(0x6b)
	buf << u8(0x33)
	buf << u8(0x43)
	buf << u8(0xcf)
	buf << u8(0) // dcid_len
	buf << u8(0) // scid_len
	parse_long_header(buf) or {
		assert err.msg().contains('unsupported')
		return
	}
	assert false, 'expected an error for a non-v1 version'
}

// test_encode_long_header_rejects_unsupported_version is a regression test
// for a Codex finding (vlang/v#27680 pullrequestreview-4783410111):
// encode_long_header's v1-CID-length check was correctly gated on
// `h.version == quic_v1`, but nothing stopped the function from going on to
// encode `h.typ` using v1's OWN packet-type bit mapping regardless of
// `h.version` -- a non-v1 header (e.g. QUIC v2, RFC 9369 §3.2) would get
// silently mis-encoded (v2 assigns different meanings to the same two
// bits), even though parse_long_header already rejects any non-v1 version
// outright (test_parse_long_header_rejects_unsupported_version above).
fn test_encode_long_header_rejects_unsupported_version() {
	h := QuicLongHeader{
		typ:     .initial
		version: u32(0x6b3343cf) // QUIC v2's registered version value
		dcid:    [u8(1), 2, 3, 4]
		scid:    [u8(5), 6, 7, 8]
		length:  10
	}
	encode_long_header(h, 0, 0) or {
		assert err.msg().contains('unsupported')
		return
	}
	assert false, 'expected an error for encoding a non-v1 long header'
}

// test_encode_long_header_rejects_length_shorter_than_packet_number is a
// regression test for a Codex finding (vlang/v#27680
// pullrequestreview-4822597219): Length (RFC 9000 §17.2) covers the packet
// number field plus payload, so it can never be shorter than the packet
// number field's own encoded width -- but h.length is caller-supplied and
// nothing checked it against pn_length_bits. A 4-byte packet number
// (pn_length_bits=3) with length=1 previously encoded successfully,
// directing a peer using Length to skip this packet to a byte offset
// landing inside the packet number field instead of past it.
fn test_encode_long_header_rejects_length_shorter_than_packet_number() {
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    [u8(1), 2, 3, 4]
		scid:    [u8(5), 6, 7, 8]
		length:  1
	}
	encode_long_header(h, 0, 3) or {
		assert err.msg().contains('must be at least')
		return
	}
	assert false, 'expected an error for a Length shorter than the packet number field'
}

// test_encode_long_header_accepts_length_equal_to_packet_number is the
// boundary check: length exactly equal to the packet number field's width
// (a zero-byte payload) must still succeed, not just anything strictly
// larger.
fn test_encode_long_header_accepts_length_equal_to_packet_number() {
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    [u8(1), 2, 3, 4]
		scid:    [u8(5), 6, 7, 8]
		length:  4
	}
	encoded := encode_long_header(h, 0, 3)!
	assert encoded.len > 0
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

fn test_encode_short_header_round_trips_through_parse() {
	dcid := [u8(0xDE), 0xAD, 0xBE, 0xEF]
	encoded := encode_short_header(dcid, true, 0, true, 0x3)!
	assert encoded.len == 1 + dcid.len
	// Protected low bits (reserved+key_phase+pn_length) are checked directly
	// here rather than round-tripped through parse_short_header, since that
	// function assumes header protection has already been removed and would
	// reject these reserved bits as nonzero.
	assert encoded[0] & 0x40 != 0 // Fixed Bit always set
	assert encoded[0] & 0x20 != 0 // spin_bit
	assert encoded[0] & 0x04 != 0 // key_phase
	assert encoded[0] & 0x03 == 0x3 // pn_length_bits
	assert encoded[1..] == dcid

	encoded_off := encode_short_header(dcid, false, 0, false, 0)!
	assert encoded_off[0] & 0x20 == 0
	assert encoded_off[0] & 0x04 == 0
	assert encoded_off[0] & 0x03 == 0
}

fn test_encode_short_header_rejects_out_of_range_fields() {
	encode_short_header([]u8{}, false, 0x4, false, 0) or {
		assert err.msg().contains('reserved_bits')
		return
	}
	assert false, 'expected out-of-range reserved_bits to be rejected'
}

// test_short_header_rejects_negative_dcid_len is a regression test for a
// Codex finding (vlang/v#27680 pullrequestreview-4806500473): a negative
// `dcid_len` (e.g. from unvalidated connection state) made
// `buf.len < 1 + dcid_len` pass (since `1 + dcid_len` went negative too),
// so the function proceeded to `buf[1..1 + dcid_len]` -- a slice with
// start > end, which panics rather than returning this `!`-declared
// function's own error. Confirmed via a standalone repro before this fix:
// `parse_short_header([]u8{len: 10, init: 0x40}, -1)` crashed the process
// with "V panic: array.slice: invalid slice index (start>end):1, 0".
fn test_short_header_rejects_negative_dcid_len() {
	buf := []u8{len: 10, init: 0x40}
	parse_short_header(buf, -1) or {
		assert err.msg().contains('dcid_len')
		assert err.msg().contains('-1')
		return
	}
	assert false, 'expected an error for a negative dcid_len, not a panic or success'
}

// test_short_header_decodes_spin_bit_and_key_phase is a regression test
// for a Codex finding (vlang/v#27680 pullrequestreview-4781706846):
// parse_short_header used to always return spin_bit/key_phase as false,
// regardless of the actual wire bits -- a caller relying on key_phase to
// select updated 1-RTT keys after a peer-initiated key update (RFC 9001
// §6) could never detect the rotation.
fn test_short_header_decodes_spin_bit_and_key_phase() {
	// 0x40 = Fixed Bit only (spin=0, key_phase=0) -- the baseline.
	parsed_off, _ := parse_short_header([u8(0x40)], 0)!
	assert parsed_off.spin_bit == false
	assert parsed_off.key_phase == false

	// 0x40 | 0x20 (spin bit) | 0x04 (key phase) -- both set.
	parsed_on, _ := parse_short_header([u8(0x40 | 0x20 | 0x04)], 0)!
	assert parsed_on.spin_bit == true
	assert parsed_on.key_phase == true

	// Spin bit alone.
	parsed_spin_only, _ := parse_short_header([u8(0x40 | 0x20)], 0)!
	assert parsed_spin_only.spin_bit == true
	assert parsed_spin_only.key_phase == false

	// Key phase alone.
	parsed_kp_only, _ := parse_short_header([u8(0x40 | 0x04)], 0)!
	assert parsed_kp_only.spin_bit == false
	assert parsed_kp_only.key_phase == true
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

// test_version_negotiation_rejects_empty_versions_list is a regression test
// for a Codex finding (vlang/v#27680 pullrequestreview-4783410111): a VN
// packet with nothing after the SCID has `remaining == 0`, which trivially
// passes the `remaining % 4 != 0` check, so parse_version_negotiation
// returned a QuicVersionNegotiation with an empty `versions` list instead of
// rejecting it. A VN packet exists specifically to advertise the versions
// the server supports; one listing zero is not a meaningful negotiation
// offer.
fn test_version_negotiation_rejects_empty_versions_list() {
	mut buf := []u8{}
	buf << u8(0x80)
	buf << [u8(0), 0, 0, 0]
	buf << u8(0) // dcid_len = 0
	buf << u8(0) // scid_len = 0
	// no versions follow
	parse_version_negotiation(buf) or {
		assert err.msg().contains('no supported versions')
		return
	}
	assert false, 'expected an error for a version negotiation packet with no versions listed'
}

fn test_long_header_lists_v1_only_and_matches_constant() {
	assert quic_v1 == u32(0x00000001)
}

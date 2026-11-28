// Copyright (c) 2026 blackshirt.
//
// ASN.1 DER reader tests: read_asn1_element's tag/length framing (short/long
// forms, constructed vs primitive) and its rejection of malformed encodings.
// parse_oid's basic decoding lives in pki_test.v; this file adds its edge inputs.
module pki

import tls.wire

fn test_asn1_read_element_short_form() ! {
	// SEQUENCE (0x30, constructed bit set) with a short-form 2-byte length.
	mut r := wire.new_reader([u8(0x30), 0x02, 0xaa, 0xbb])
	el := read_asn1_element(mut r)!
	assert el.tag == 0x30
	assert el.is_construct
	assert el.data == [u8(0xaa), 0xbb]
}

fn test_asn1_read_element_primitive_flag() ! {
	// INTEGER (0x02) is primitive: the constructed bit (0x20) is clear.
	mut r := wire.new_reader([u8(0x02), 0x01, 0x05])
	el := read_asn1_element(mut r)!
	assert !el.is_construct
	assert el.data == [u8(0x05)]
}

fn test_asn1_read_element_long_form() ! {
	// OCTET STRING (0x04) with a minimal long-form length: 0x81 marks "one length
	// octet follows", whose value is 129. DER only permits long form when the
	// length is >= 0x80, so a 129-byte payload is the smallest legal 0x81 case.
	mut payload := []u8{len: 129, init: 0x40}
	mut data := [u8(0x04), 0x81, u8(129)]
	data << payload
	mut r := wire.new_reader(data)
	el := read_asn1_element(mut r)!
	assert el.tag == 0x04
	assert !el.is_construct
	assert el.data == payload
}

fn test_asn1_read_element_rejects_nonminimal_length() ! {
	// 0x81 0x03 spells length 3 in long form, but 3 < 0x80 must use the short
	// form (X.690 §10.1); tolerating it lets one byte stream parse two ways.
	mut r := wire.new_reader([u8(0x04), 0x81, 0x03, 0x01, 0x02, 0x03])
	read_asn1_element(mut r) or {
		assert err.msg().contains('non-minimal DER length')
		return
	}
	assert false
	// 0x82 0x00 0x33 uses two length octets with a leading zero — also non-minimal.
	mut r2 := wire.new_reader([u8(0x04), 0x82, 0x00, 0x33])
	read_asn1_element(mut r2) or {
		assert err.msg().contains('non-minimal DER length')
		return
	}
	assert false
}

fn test_asn1_read_element_truncated_header() ! {
	// Fewer than 2 bytes cannot form a tag+length header.
	mut r := wire.new_reader([u8(0x30)])
	read_asn1_element(mut r) or {
		assert err.msg().contains('truncated element header')
		return
	}
	assert false
}

fn test_asn1_read_element_indefinite_length() ! {
	// 0x80 declares zero following length octets — the indefinite-length marker
	// that DER forbids — and must be rejected.
	mut r := wire.new_reader([u8(0x30), 0x80, 0x01])
	read_asn1_element(mut r) or {
		assert err.msg().contains('invalid or unsupported length octets')
		return
	}
	assert false
}

fn test_asn1_read_element_length_exceeds_remaining() ! {
	// Short-form length 0x10 (16) but only 1 payload byte is buffered.
	mut r := wire.new_reader([u8(0x02), 0x10, 0x01])
	read_asn1_element(mut r) or {
		assert err.msg().contains('exceeds remaining bytes')
		return
	}
	assert false
}

fn test_asn1_read_element_rejects_high_tag_form() ! {
	// (tag & 0x1f) == 0x1f marks the multi-octet high-tag-number form, which DER
	// forbids (X.690 §10.5); it must be rejected before the length is parsed.
	mut r := wire.new_reader([u8(0x1f), 0x81, 0x02, 0x01, 0x02])
	read_asn1_element(mut r) or {
		assert err.msg().contains('high-tag-number form')
		return
	}
	assert false
}

fn test_asn1_parse_sequence_children() ! {
	// Two concatenated INTEGER elements: both are returned, in order, with
	// their exact value bytes.
	data := [u8(0x02), 0x01, 0x0a, 0x02, 0x01, 0x14]
	els := parse_sequence(data)!
	assert els.len == 2
	assert els[0].data == [u8(0x0a)]
	assert els[1].data == [u8(0x14)]
	// An empty buffer yields an empty list, not an error.
	assert parse_sequence([]u8{})!.len == 0
}

fn test_parse_oid_edge_inputs() {
	// Empty content has no first subidentifier: '' rather than a bogus arc.
	assert parse_oid([]u8{}) == ''
	// First-arc packing boundaries (RFC 5280 §A.1): first<40 → arc 0, and
	// first==80 lands exactly on the arc-2 branch as 2.0 (not 1.40).
	assert parse_oid([u8(0x04)]) == '0.4'
	assert parse_oid([u8(0x50)]) == '2.0'
	// sha256WithRSAEncryption pinned end to end: the multi-byte 840 and 113549
	// subidentifiers plus the terminating 01 01 0B arcs.
	assert parse_oid([u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]) == '1.2.840.113549.1.1.11'
}

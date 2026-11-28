// Copyright (c) 2026 blackshirt.
//
// asn1.v is a minimal DER reader for the ASN.1 subset x509.v needs: tag/length/value,
// object identifiers and integer payload extraction. It performs no certificate
// semantics validation.
module pki

import tls.wire

// Asn1Element represents a parsed ASN.1 DER tag, length, and content bytes.
struct Asn1Element {
	tag          u8   // identifier octet, including class/constructed bits
	is_construct bool // bit 0x20 of the tag: data holds child TLVs rather than raw bytes
	data         []u8 // value octets; children for constructed, raw bytes for primitive
}

// read_asn1_element parses a single ASN.1 TLV; only definite-length encodings
// (DER) are supported. bit 0x20 marks constructed values; length uses short or long form.
fn read_asn1_element(mut r wire.Reader) !Asn1Element {
	if r.remaining() < 2 {
		return error('asn1: truncated element header')
	}
	tag := r.read_u8()!
	// DER permits only the low-tag-number form (X.690 §10.5): bits 1–5 all set
	// (0x1f) signal a multi-octet identifier, which valid DER never emits. X.509
	// tags in use here are all < 0x1f, so rejecting it fails closed on malformed
	// or adversarial input rather than misreading the following byte as a tag.
	if (tag & 0x1f) == 0x1f {
		return error('asn1: high-tag-number form is not permitted in DER')
	}
	is_construct := (tag & 0x20) != 0

	len_byte := r.read_u8()!
	mut length := u32(0)

	if (len_byte & 0x80) == 0 {
		// Short form: the byte is the length itself.
		length = u32(len_byte)
	} else {
		// Long form: low 7 bits count the big-endian length octets that follow.
		num_len_bytes := int(len_byte & 0x7f)
		if num_len_bytes == 0 || num_len_bytes > 4 {
			// 0 is reserved (indefinite length, illegal in DER); >4 would overflow u32 and is not needed.
			return error('asn1: invalid or unsupported length octets (${num_len_bytes})')
		}
		if r.remaining() < num_len_bytes {
			return error('asn1: truncated length octets')
		}
		mut first_len_byte := u8(0) // X.690 §8.1.3: leading octet must not be 0
		for i in 0 .. num_len_bytes {
			b := r.read_u8()!
			if i == 0 {
				first_len_byte = b
			}
			length = (length << 8) | u32(b)
		}
		// DER mandates minimal length encoding (X.690 §10.1): long form is only
		// legal when the value is ≥ 0x80 and carries no leading zero octet.
		if first_len_byte == 0 {
			return error('asn1: non-minimal DER length (leading zero octet)')
		}
		if length < 0x80 {
			return error('asn1: non-minimal DER length (long form for ${length})')
		}
	}

	if r.remaining() < int(length) {
		return error('asn1: element length ${length} exceeds remaining bytes (${r.remaining()})')
	}

	data := r.read_bytes(int(length))!
	return Asn1Element{
		tag:          tag
		is_construct: is_construct
		data:         data
	}
}

// parse_sequence decodes an ASN.1 SEQUENCE into its child elements.
@[direct_array_access]
fn parse_sequence(data []u8) ![]Asn1Element {
	mut r := wire.new_reader(data)
	mut elements := []Asn1Element{}
	for r.remaining() > 0 {
		el := read_asn1_element(mut r)!
		elements << el
	}
	return elements
}

// parse_oid decodes an ASN.1 object identifier into canonical dotted-decimal
// form (RFC 5280 §A.1). The first subidentifier packs arcs 1 and 2 as (arc1*40+arc2).
@[direct_array_access]
fn parse_oid(data []u8) string {
	if data.len == 0 {
		return ''
	}
	// The final byte of an ASN.1 OID must have bit 7 clear.
	if (data[data.len - 1] & 0x80) != 0 {
		return ''
	}

	mut idx := 0
	mut first := u64(0)
	for {
		if idx >= data.len {
			return '' // truncated: subidentifier never terminated
		}
		b := data[idx]
		first = (first << 7) | u64(b & 0x7f)
		idx++
		if (b & 0x80) == 0 {
			break
		}
	}

	arc1 := if first < 40 {
		u64(0)
	} else if first < 80 {
		u64(1)
	} else {
		u64(2)
	}
	mut oid := '${arc1}.${first - arc1 * 40}'

	mut val := u64(0)
	for idx < data.len {
		b := data[idx]
		val = (val << 7) | u64(b & 0x7f)
		idx++
		if (b & 0x80) == 0 {
			oid += '.${val}'
			val = 0
		}
	}
	return oid
}

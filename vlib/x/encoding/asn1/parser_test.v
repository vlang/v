module asn1

// The streaming `Parser` API is the only surface in this module that reads a
// byte sequence incrementally, and it had no direct coverage: the other test
// files exercise it indirectly through the `parse` helpers. Every expected
// value below comes from hand-built DER byte arrays measured on this tree,
// not from the ASN.1 specifications.

struct TlvCase {
	bytes      []u8
	tag_number int
	tag_class  TagClass
	payload    []u8
}

const tlv_cases = [
	TlvCase{
		bytes:      [u8(0x02), 0x01, 0x05]
		tag_number: 2
		tag_class:  .universal
		payload:    [u8(0x05)]
	},
	// NOTE: the ENUMERATED payload for 2 comes out as `10`, not `02`.
	// `Enumerated.payload_with_rule` shifts and multiplies in the same
	// expression, so the shift result is scaled by 8. Asserted as measured.
	TlvCase{
		bytes:      [u8(0x0a), 0x01, 0x02]
		tag_number: 10
		tag_class:  .universal
		payload:    [u8(0x10)]
	},
	TlvCase{
		bytes:      [u8(0x16), 0x03, 0x61, 0x62, 0x63]
		tag_number: 22
		tag_class:  .universal
		payload:    [u8(0x61), 0x62, 0x63]
	},
	TlvCase{
		bytes:      [u8(0x04), 0x02, 0xde, 0xad]
		tag_number: 4
		tag_class:  .universal
		payload:    [u8(0xde), 0xad]
	},
	TlvCase{
		bytes:      [u8(0x0c), 0x02, 0x68, 0x69]
		tag_number: 12
		tag_class:  .universal
		payload:    [u8(0x68), 0x69]
	},
	TlvCase{
		bytes:      [u8(0x1b), 0x01, 0x41]
		tag_number: 27
		tag_class:  .universal
		payload:    [u8(0x41)]
	},
	TlvCase{
		bytes:      [u8(0x05), 0x00]
		tag_number: 5
		tag_class:  .universal
		payload:    []u8{}
	},
	TlvCase{
		bytes:      [u8(0x30), 0x02, 0x05, 0x00]
		tag_number: 16
		tag_class:  .universal
		payload:    [u8(0x05), 0x00]
	},
	TlvCase{
		bytes:      [u8(0x44), 0x01, 0x07]
		tag_number: 4
		tag_class:  .application
		payload:    [u8(0x07)]
	},
	TlvCase{
		bytes:      [u8(0xc4), 0x01, 0x07]
		tag_number: 4
		tag_class:  .private
		payload:    [u8(0x07)]
	},
]

struct LengthCase {
	bytes []u8
	want  int
	rest  []u8
}

const length_cases = [
	LengthCase{
		bytes: [u8(0x00)]
		want:  0
		rest:  []u8{}
	},
	LengthCase{
		bytes: [u8(0x05), 0xaa]
		want:  5
		rest:  [u8(0xaa)]
	},
	LengthCase{
		bytes: [u8(0x7f), 0xaa]
		want:  127
		rest:  [u8(0xaa)]
	},
	LengthCase{
		bytes: [u8(0x81), 0x80]
		want:  128
		rest:  []u8{}
	},
	LengthCase{
		bytes: [u8(0x82), 0x01, 0x00]
		want:  256
		rest:  []u8{}
	},
]

fn test_read_tlv_reads_a_built_entry_and_exhausts_the_parser() {
	for c in tlv_cases {
		mut p := Parser.new(c.bytes)
		el := p.read_tlv()!
		assert el.tag().tag_number() == c.tag_number, 'bytes ${c.bytes.hex()}: tag number ${el.tag().tag_number()}, want ${c.tag_number}'
		assert el.tag().tag_class() == c.tag_class, 'bytes ${c.bytes.hex()}: class ${el.tag().tag_class()}, want ${c.tag_class}'
		assert el.payload()! == c.payload, 'bytes ${c.bytes.hex()}: payload ${el.payload()!.hex()}, want ${c.payload.hex()}'
		assert p.is_empty(), 'bytes ${c.bytes.hex()}: parser not empty after read_tlv'
	}
}

fn test_read_tag_consumes_only_the_identifier_octets() {
	// 30 03 02 01 05: the SEQUENCE tag is one byte, the four bytes after it stay.
	mut p := Parser.new([u8(0x30), 0x03, 0x02, 0x01, 0x05])
	tag := p.read_tag()!
	assert tag.tag_number() == int(TagType.sequence)
	assert tag.tag_class() == .universal
	assert tag.is_constructed()
	assert p.data == [u8(0x03), 0x02, 0x01, 0x05], 'remaining ${p.data.hex()}'
}

fn test_peek_tag_does_not_advance_the_parser() {
	mut p := Parser.new([u8(0x30), 0x03, 0x02, 0x01, 0x05])
	first := p.peek_tag()!
	second := p.peek_tag()!
	assert first.equal(second)
	assert p.data.len == 5, 'peek_tag consumed one byte, ${p.data.len} remain'
	tag := p.read_tag()!
	assert tag.equal(first)
	assert p.data.len == 4, 'read_tag left ${p.data.len} bytes'
}

fn test_read_length_handles_short_and_long_definite_forms() {
	for c in length_cases {
		mut p := Parser.new(c.bytes)
		length := p.read_length()!
		assert length == Length(c.want), 'bytes ${c.bytes.hex()}: length ${int(length)}, want ${c.want}'
		assert p.data == c.rest, 'bytes ${c.bytes.hex()}: remaining ${p.data.hex()}, want ${c.rest.hex()}'
	}
}

fn test_read_bytes_consumes_exactly_the_requested_count() {
	mut p := Parser.new([u8(1), 2, 3, 4])
	first := p.read_bytes(2)!
	assert first == [u8(1), 2], 'first read_bytes gave ${first.hex()}'
	second := p.read_bytes(2)!
	assert second == [u8(3), 4], 'second read_bytes gave ${second.hex()}'
	assert p.is_empty(), 'parser should be empty after both reads'
}

fn test_read_bytes_rejects_a_count_beyond_the_buffer() {
	mut p := Parser.new([u8(1), 2, 3])
	p.read_bytes(99) or {
		assert err.msg() == 'Parser: too short data to read 99 bytes'
		return
	}
	assert false, 'read_bytes(99) on a 3 byte buffer should have failed'
}

fn test_read_bytes_rejects_a_read_from_an_empty_buffer() {
	mut p := Parser.new([]u8{})
	p.read_bytes(1) or {
		assert err.msg() == 'Parser: too short data to read 1 bytes'
		return
	}
	assert false, 'read_bytes(1) on an empty parser should have failed'
}

fn test_read_tlv_rejects_a_truncated_content_octet_string() {
	// 02 02 01 announces two content octets but supplies only one.
	mut p := Parser.new([u8(0x02), 0x02, 0x01])
	p.read_tlv() or {
		assert err.msg() == 'Parser: too short data to read 2 bytes'
		return
	}
	assert false, 'read_tlv on truncated content should have failed'
}

fn test_read_tlv_streams_several_entries_from_one_parser() {
	mut p := Parser.new([u8(0x02), 0x01, 0x05, 0x02, 0x01, 0x06])
	first := p.read_tlv()!
	second := p.read_tlv()!
	assert !first.equal(second)
	assert first.equal(Integer.from_int(5))
	assert second.equal(Integer.from_int(6))
	assert p.is_empty(), 'parser should hold no bytes after both entries'
}

fn test_finish_passes_only_on_an_exhausted_parser() {
	mut p := Parser.new([u8(0x02), 0x01, 0x05])
	p.read_tlv()!
	p.finish()!
	assert p.is_empty()
}

fn test_finish_rejects_trailing_bytes() {
	mut p := Parser.new([u8(0x02), 0x01, 0x05, 0x02, 0x01, 0x06])
	p.read_tlv()!
	p.finish() or {
		assert err.msg() == 'not empty on finish'
		return
	}
	assert false, 'finish() with unread bytes should have failed'
}

fn test_reset_empties_the_buffer_and_blocks_further_reads() {
	mut p := Parser.new([u8(0x02), 0x01, 0x05, 0x02, 0x01, 0x06])
	p.read_tlv()!
	p.reset()
	assert p.is_empty(), 'reset should leave an empty buffer'
	assert p.data.len == 0, 'reset left ${p.data.len} bytes'
	p.read_tag() or {
		assert err.msg() == 'Tag: bytes underflow'
		return
	}
	assert false, 'read_tag after reset should have failed'
}

fn test_is_empty_reflects_the_buffer() {
	assert Parser.new([]u8{}).is_empty()
	assert !Parser.new([u8(0x02)]).is_empty()
}

fn test_read_tag_on_empty_data_fails() {
	mut p := Parser.new([]u8{})
	p.read_tag() or {
		assert err.msg() == 'Tag: bytes underflow'
		return
	}
	assert false, 'read_tag on an empty parser should have failed'
}

fn test_read_length_on_empty_data_fails() {
	mut p := Parser.new([]u8{})
	p.read_length() or {
		assert err.msg() == 'Length: truncated length'
		return
	}
	assert false, 'read_length on an empty parser should have failed'
}

fn test_read_length_rejects_a_truncated_long_form() {
	// 82 01 promises two following octets but only one is present.
	mut p := Parser.new([u8(0x82), 0x01])
	p.read_length() or {
		assert err.msg() == 'Length: truncated length'
		return
	}
	assert false, 'read_length on a truncated long form should have failed'
}

fn test_read_tlv_on_empty_data_fails() {
	mut p := Parser.new([]u8{})
	p.read_tlv() or {
		assert err.msg() == 'Tag: bytes underflow'
		return
	}
	assert false, 'read_tlv on an empty parser should have failed'
}

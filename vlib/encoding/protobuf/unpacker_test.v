module protobuf

fn test_unpacker_reads_tag() {
	mut u := new_unpacker([u8(0x08), u8(0x96), u8(0x01)], DecodeOpts{})
	number, wire_type := u.read_tag()!
	assert number == 1
	assert wire_type == .varint
	assert u.read_uint32()! == 150
	assert u.eof()
	assert u.remaining() == 0
}

fn test_unpacker_offset_tracks_reads() {
	mut u := new_unpacker([u8(0x08), u8(0x01), u8(0x10), u8(0x02)], DecodeOpts{})
	assert u.offset() == 0
	u.read_tag()!
	assert u.offset() == 1
	u.read_uint32()!
	assert u.offset() == 2
	assert u.remaining() == 2
}

fn test_unpacker_bool_round_trip() {
	// proto3 says any non-zero value is true, so a producer that wrote 2
	// instead of 1 still has to decode as true.
	for value in [true, false] {
		mut p := new_packer(EncodeOpts{})
		p.write_bool(1, value)
		mut u := new_unpacker(p.bytes(), DecodeOpts{})
		u.read_tag()!
		assert u.read_bool()! == value
	}
	mut u := new_unpacker([u8(0x08), u8(0x02)], DecodeOpts{})
	u.read_tag()!
	assert u.read_bool()! == true
}

fn test_unpacker_uint_round_trip() {
	mut p := new_packer(EncodeOpts{})
	p.write_uint32(1, 4294967295)
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	u.read_tag()!
	assert u.read_uint32()! == 4294967295
}

fn test_unpacker_int32_negative_round_trip() {
	mut p := new_packer(EncodeOpts{})
	p.write_int32(1, -2147483648)
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	u.read_tag()!
	assert u.read_int32()! == -2147483648
}

fn test_unpacker_int64_negative_round_trip() {
	mut p := new_packer(EncodeOpts{})
	p.write_int64(1, -9223372036854775807 - 1)
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	u.read_tag()!
	assert u.read_int64()! == -9223372036854775807 - 1
}

fn test_unpacker_sint_round_trip() {
	mut p := new_packer(EncodeOpts{})
	p.write_sint32(1, -12345)
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	u.read_tag()!
	assert u.read_sint32()! == -12345

	mut q := new_packer(EncodeOpts{})
	q.write_sint64(1, -1234567890123)
	mut v := new_unpacker(q.bytes(), DecodeOpts{})
	v.read_tag()!
	assert v.read_sint64()! == -1234567890123
}

fn test_unpacker_fixed_round_trip() {
	mut p := new_packer(EncodeOpts{})
	p.write_fixed32(1, 0xdeadbeef)
	p.write_fixed64(2, 0x0123456789abcdef)
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	u.read_tag()!
	assert u.read_fixed32()! == 0xdeadbeef
	u.read_tag()!
	assert u.read_fixed64()! == 0x0123456789abcdef
	assert u.eof()
}

fn test_unpacker_sfixed_round_trip() {
	mut p := new_packer(EncodeOpts{})
	p.write_sfixed32(1, -12345)
	p.write_sfixed64(2, -1234567890123)
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	u.read_tag()!
	assert u.read_sfixed32()! == -12345
	u.read_tag()!
	assert u.read_sfixed64()! == -1234567890123
}

fn test_unpacker_float_round_trip() {
	mut p := new_packer(EncodeOpts{})
	p.write_float(1, f32(3.5))
	p.write_double(2, f64(-2.25))
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	u.read_tag()!
	assert u.read_float()! == f32(3.5)
	u.read_tag()!
	assert u.read_double()! == f64(-2.25)
}

fn test_unpacker_string_and_bytes() {
	mut p := new_packer(EncodeOpts{})
	p.write_string(1, 'hello')!
	p.write_bytes(2, [u8(0xde), u8(0xad), u8(0xbe), u8(0xef)])
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	u.read_tag()!
	assert u.read_string()! == 'hello'
	u.read_tag()!
	assert u.read_bytes()! == [u8(0xde), u8(0xad), u8(0xbe), u8(0xef)]
	assert u.eof()
}

fn test_unpacker_read_string_rejects_bad_utf8_when_asked() {
	mut p := new_packer(EncodeOpts{})
	p.write_bytes(1, [u8(0xff), u8(0xfe)])
	mut strict := new_unpacker(p.bytes(), DecodeOpts{ validate_utf8: true })
	if number, _ := strict.read_tag() {
		if _ := strict.read_string() {
			assert false, 'expected invalid UTF-8 to be rejected at field ${number}'
		} else {
			assert err is InvalidUtf8Error
		}
	} else {
		assert false, 'expected the tag to read cleanly'
	}

	// the same payload is fine when validation is off
	mut lax := new_unpacker(p.bytes(), DecodeOpts{})
	lax.read_tag()!
	assert lax.read_string()! == [u8(0xff), u8(0xfe)].bytestr()
}

fn test_unpacker_len_delimited_bounds() {
	// read_len_delimited starts at the length, not at a tag, so these buffers
	// begin with the count.
	// a length that claims more than the buffer holds
	mut overlong := new_unpacker([u8(0x05), u8(0x01)], DecodeOpts{})
	if got := overlong.read_len_delimited() {
		assert false, 'expected an overlong length to fail, got ${got}'
	} else {
		assert err is UnexpectedEofError
	}
}

fn test_unpacker_len_delimited_exact_and_zero() {
	// a length of exactly what is left is fine
	mut ok := new_unpacker([u8(0x02), u8(0x01), u8(0x02)], DecodeOpts{})
	payload := ok.read_len_delimited()!
	assert payload == [u8(0x01), u8(0x02)]
	// and so is a zero length
	mut zero := new_unpacker([u8(0x00)], DecodeOpts{})
	empty := zero.read_len_delimited()!
	assert empty.len == 0
}

fn test_unpacker_length_ceiling_is_checked_before_the_int_cast() {
	// A length is a 64-bit value while `int` is 32 bits on some targets, so
	// casting first would wrap. This declares u64 max with no payload behind
	// it: nine continuation bytes then the terminator, which is the longest
	// varint a 64-bit value can have.
	mut data := []u8{len: 10, init: 0xff}
	data[9] = 0x01
	mut u := new_unpacker(data, DecodeOpts{})
	if got := u.read_len_delimited() {
		assert false, 'expected the ceiling to reject this, got ${got}'
	} else {
		assert err is MaxLengthError
	}
}

fn test_unpacker_custom_length_ceiling() {
	// a length just past a tightened ceiling
	mut p := new_packer(EncodeOpts{})
	p.write_bytes(1, []u8{len: 16, init: 0xaa})
	// start at the length the packer wrote, which is a full 16
	mut tight := new_unpacker(p.bytes()[1..], DecodeOpts{ max_length: 8 })
	if got := tight.read_len_delimited() {
		assert false, 'expected max_length 8 to reject 16 bytes, got ${got}'
	} else {
		assert err is MaxLengthError
	}
	// and the default ceiling is far above a small message
	mut roomy := new_unpacker(p.bytes()[1..], DecodeOpts{})
	payload := roomy.read_len_delimited()!
	assert payload.len == 16
}

fn test_unpacker_skip_field_steps_over_unknown_fields() {
	// The forward-compatibility requirement: a newer producer's extra field
	// must not break an older consumer.
	mut p := new_packer(EncodeOpts{})
	p.write_uint32(1, 42)
	p.write_string(99, 'a field from the future')!
	p.write_uint32(2, 7)

	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	mut seen := map[int]u64{}
	for !u.eof() {
		number, wire_type := u.read_tag()!
		if number == 1 || number == 2 {
			seen[number] = u.read_uint32()!
		} else {
			u.skip_field(number, wire_type)!
		}
	}
	assert seen[1] == 42
	assert seen[2] == 7
	assert seen.len == 2, 'the unknown field must not be recorded'
	assert u.eof()
}

fn test_unpacker_skip_field_rejects_when_unknown_fields_are_not_allowed() {
	mut p := new_packer(EncodeOpts{})
	p.write_string(99, 'a field from the future')!
	mut u := new_unpacker(p.bytes(), DecodeOpts{ allow_unknown_fields: false })
	number, wire_type := u.read_tag()!
	if _ := u.skip_field(number, wire_type) {
		assert false, 'expected the unknown field to be rejected'
	} else {
		assert err is UnknownFieldError
	}
}

fn test_unpacker_skip_covers_every_fixed_width() {
	mut p := new_packer(EncodeOpts{})
	p.write_uint32(10, 1)
	p.write_sint64(11, -5)
	p.write_fixed32(12, 9)
	p.write_fixed64(13, 9)
	p.write_string(14, 'x')!
	p.write_message(15, [u8(1), u8(2)])

	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	for _ in 0 .. 6 {
		number, wire_type := u.read_tag()!
		u.skip_field(number, wire_type)!
	}
	assert u.eof(), 'skipping every field must land exactly on the end'
}

fn test_unpacker_rejects_group_wire_types() {
	// start_group is tag ...| 3. A group's extent cannot be found without
	// reading fields the reader does not understand, so it is refused.
	for n in [3, 4] {
		data := [u8(0x08 | n), u8(0x01)]
		mut u := new_unpacker(data, DecodeOpts{})
		number, wire_type := u.read_tag()!
		if _ := u.skip_field(number, wire_type) {
			assert false, 'expected group wire type ${n} to be refused'
		} else {
			assert err is GroupUnsupportedError
		}
	}
}

fn test_unpacker_rejects_unassigned_wire_types() {
	// Six and seven were never assigned, so a tag naming one is malformed
	// rather than merely unknown.
	for n in [6, 7] {
		data := [u8(0x08 | n), u8(0x01)]
		mut u := new_unpacker(data, DecodeOpts{})
		if got, _ := u.read_tag() {
			assert false, 'expected wire type ${n} to be rejected, got ${got}'
		} else {
			assert err is UnknownWireTypeError
		}
	}
}

fn test_unpacker_rejects_out_of_range_field_number() {
	// Field number zero is reserved: a tag of zero names no field.
	mut u := new_unpacker([u8(0x00), u8(0x01)], DecodeOpts{})
	if got, _ := u.read_tag() {
		assert false, 'expected field number 0 to be rejected, got ${got}'
	} else {
		assert err is MalformedError
	}
}

fn test_unpacker_rejects_a_field_number_that_would_wrap_to_a_legal_one() {
	// 2^32 + 1 is 1 once truncated to 32 bits, so a target with a 32-bit `int`
	// would read it as field 1 unless the range is checked before the cast.
	mut data := []u8{}
	put_varint(mut data, ((u64(1) << 32) | 1) << 3)
	data << u8(0x01)
	mut u := new_unpacker(data, DecodeOpts{})
	if got, _ := u.read_tag() {
		assert false, 'expected field number 2^32+1 to be rejected, got ${got}'
	} else {
		assert err is MalformedError
	}
	// The largest legal number still reads back as itself.
	mut ok := []u8{}
	put_varint(mut ok, field_key(max_field_number, .varint))
	mut v := new_unpacker(ok, DecodeOpts{})
	number, _ := v.read_tag()!
	assert number == max_field_number
}

fn test_unpacker_depth_limit() {
	// Each nesting level costs two bytes, so a 40-byte input can nest 20 deep.
	// Without a bound that is a denial of service rather than a big message.
	mut payload := [u8(0x00)]
	for _ in 0 .. 20 {
		// wrap: field 1, length-delimited, one byte of inner message
		mut wrapped := [u8(0x0a), u8(payload.len)]
		wrapped << payload
		payload = wrapped.clone()
	}
	mut u := new_unpacker(payload, DecodeOpts{ max_depth: 5 })
	for _ in 0 .. 5 {
		u.enter()!
	}
	if _ := u.enter() {
		assert false, 'expected the depth limit to stop the descent'
	} else {
		assert err is MaxDepthError
	}
}

fn test_unpacker_sub_reads_a_nested_message() {
	mut inner := new_packer(EncodeOpts{})
	inner.write_uint32(1, 99)
	mut outer := new_packer(EncodeOpts{})
	outer.write_message(1, inner.bytes())
	outer.write_uint32(2, 5)

	mut u := new_unpacker(outer.bytes(), DecodeOpts{})
	number, wire_type := u.read_tag()!
	assert number == 1
	assert wire_type == .length_delimited
	mut nested := u.sub()!
	inner_number, _ := nested.read_tag()!
	assert inner_number == 1
	assert nested.read_uint32()! == 99
	assert nested.eof()
	// the parent is positioned just past the nested payload
	u.read_tag()!
	assert u.read_uint32()! == 5
	assert u.eof()
}

fn test_unpacker_enter_leave_track_depth() {
	mut u := new_unpacker([]u8{}, DecodeOpts{})
	assert u.depth == 0
	u.enter()!
	u.enter()!
	assert u.depth == 2
	u.leave()
	assert u.depth == 1
	u.leave()
	assert u.depth == 0
	// leaving past zero must not wrap around
	u.leave()
	assert u.depth == 0
}

fn test_unpacker_nonminimal_varint_accepted_by_default() {
	// 300 padded to three bytes must still read as 300, because the spec asks
	// parsers to accept it.
	mut u := new_unpacker([u8(0x08), u8(0xac), u8(0x82), u8(0x00)], DecodeOpts{})
	u.read_tag()!
	assert u.read_uint32()! == 300
}

fn test_unpacker_nonminimal_varint_rejected_when_asked() {
	mut u := new_unpacker([u8(0x08), u8(0xac), u8(0x82), u8(0x00)],
		DecodeOpts{ reject_nonminimal_varint: true })
	u.read_tag()!
	if got := u.read_uint32() {
		assert false, 'expected the padded varint to be rejected, got ${got}'
	} else {
		assert err is MalformedError
	}
}

fn test_unpacker_minimal_varint_passes_strict_mode() {
	mut u := new_unpacker([u8(0x08), u8(0xac), u8(0x02)],
		DecodeOpts{ reject_nonminimal_varint: true })
	u.read_tag()!
	assert u.read_uint32()! == 300
}

fn test_unpacker_truncated_value_fails() {
	// tag says varint, but the varint runs off the end
	mut u := new_unpacker([u8(0x08), u8(0x80)], DecodeOpts{})
	u.read_tag()!
	if got := u.read_uint32() {
		assert false, 'expected a truncated value to fail, got ${got}'
	} else {
		assert err is UnexpectedEofError
	}
}

fn test_unpacker_packed_reads_back() {
	// A packed field is one length-delimited run of values; unpack it by hand
	// the way generated code does.
	mut p := new_packer(EncodeOpts{})
	p.write_packed_varints(1, [u64(1), 200, 300])
	mut u := new_unpacker(p.bytes(), DecodeOpts{})
	number, wire_type := u.read_tag()!
	assert number == 1
	assert wire_type == .length_delimited
	mut values := []u64{}
	mut sub := u.sub()!
	for !sub.eof() {
		values << sub.read_varint()!
	}
	assert values == [u64(1), 200, 300]
}

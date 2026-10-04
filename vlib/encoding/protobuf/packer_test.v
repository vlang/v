module protobuf

fn test_new_packer_starts_empty() {
	p := new_packer(EncodeOpts{})
	assert p.len() == 0
	assert p.bytes().len == 0
}

fn test_packer_writes_tag_and_value() {
	mut p := new_packer(EncodeOpts{})
	p.write_uint32(1, 150)
	// field 1, wire type 0 -> tag 0x08; 150 -> 0x96 0x01
	assert p.bytes() == [u8(0x08), u8(0x96), u8(0x01)]
	assert p.len() == 3
}

fn test_packer_len_delimited() {
	mut p := new_packer(EncodeOpts{})
	p.write_string(2, 'hi')!
	// field 2, wire type 2 -> tag 0x12; length 2; 'h' 'i'
	assert p.bytes() == [u8(0x12), u8(0x02), u8(0x68), u8(0x69)]
}

fn test_packer_writes_default_values_too() {
	// A Packer is a plain writer. It emits what it is told to, including
	// proto3 defaults; the presence rules live in the generated code.
	mut p := new_packer(EncodeOpts{})
	p.write_bool(1, false)
	assert p.bytes() == [u8(0x08), u8(0x00)]

	mut q := new_packer(EncodeOpts{})
	q.write_uint32(1, 0)
	assert q.bytes() == [u8(0x08), u8(0x00)]
}

fn test_packer_negative_int32_takes_ten_bytes() {
	mut p := new_packer(EncodeOpts{})
	p.write_int32(1, -1)
	// tag plus the sign-extended 64-bit varint
	assert p.len() == 11
	assert p.bytes()[0] == 0x08
	for i in 1 .. 10 {
		assert p.bytes()[i] == 0xff, 'byte ${i} of the sign extension'
	}
	assert p.bytes()[10] == 0x01
}

fn test_packer_sint32_negative_is_one_byte() {
	mut p := new_packer(EncodeOpts{})
	p.write_sint32(1, -1)
	assert p.bytes() == [u8(0x08), u8(0x01)]
}

fn test_packer_fixed_widths() {
	mut p := new_packer(EncodeOpts{})
	p.write_fixed32(1, 0x01020304)
	// tag 0x0d for field 1 wire type 5, then little-endian
	assert p.bytes() == [u8(0x0d), u8(0x04), u8(0x03), u8(0x02), u8(0x01)]

	mut q := new_packer(EncodeOpts{})
	q.write_fixed64(1, 0x0102030405060708)
	assert q.bytes() == [u8(0x09), u8(0x08), u8(0x07), u8(0x06), u8(0x05), u8(0x04), u8(0x03),
		u8(0x02), u8(0x01)]
}

fn test_packer_floats() {
	mut p := new_packer(EncodeOpts{})
	p.write_float(1, f32(1.0))
	assert p.bytes() == [u8(0x0d), u8(0x00), u8(0x00), u8(0x80), u8(0x3f)]

	mut q := new_packer(EncodeOpts{})
	q.write_double(1, f64(1.0))
	assert q.bytes() == [u8(0x09), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00),
		u8(0xf0), u8(0x3f)]
}

fn test_packer_reset_keeps_capacity() {
	mut p := new_packer(EncodeOpts{ initial_cap: 64 })
	p.write_string(1, 'hello')!
	assert p.len() == 7
	cap_before := p.bytes().cap
	p.reset()
	assert p.len() == 0
	assert p.bytes().cap == cap_before, 'reset kept the allocation'
	// and the packer is usable again
	p.write_uint32(1, 1)
	assert p.bytes() == [u8(0x08), u8(0x01)]
}

fn test_packer_reserve_grows_once() {
	mut p := new_packer(EncodeOpts{ initial_cap: 4 })
	p.reserve(1000)
	assert p.bytes().cap >= 1000
	assert p.len() == 0, 'reserve must not change the length'
}

fn test_packer_from_existing_bytes() {
	mut p := new_packer_from([u8(0xaa), u8(0xbb)], EncodeOpts{})
	p.write_uint32(1, 1)
	assert p.bytes() == [u8(0xaa), u8(0xbb), u8(0x08), u8(0x01)]
}

fn test_packer_write_string_rejects_bad_utf8_when_asked() {
	// `bytestr` keeps every byte, so this is a string holding bytes that are
	// not valid UTF-8 -- something a V source literal cannot express.
	bad := [u8(0xff), u8(0xfe)].bytestr()

	mut strict := new_packer(EncodeOpts{ validate_utf8: true })
	if _ := strict.write_string(1, bad) {
		assert false, 'expected invalid UTF-8 to be rejected'
	} else {
		assert err is InvalidUtf8Error
	}
	assert strict.len() == 0, 'a rejected string must not leave a partial field'

	// without the option the same bytes go through untouched
	mut lax := new_packer(EncodeOpts{})
	lax.write_string(1, bad)!
	assert lax.bytes() == [u8(0x0a), u8(0x02), u8(0xff), u8(0xfe)]

	// valid multi-byte UTF-8 passes the check
	mut ok := new_packer(EncodeOpts{ validate_utf8: true })
	ok.write_string(1, 'héllo')!
	assert ok.len() == 1 + 1 + 6
}

fn test_packer_write_bytes_is_exempt_from_utf8() {
	// `bytes` carries arbitrary octets by design, so validation must not
	// apply to it even when the option is on.
	mut p := new_packer(EncodeOpts{ validate_utf8: true })
	p.write_bytes(1, [u8(0xff), u8(0xfe)])
	assert p.bytes() == [u8(0x0a), u8(0x02), u8(0xff), u8(0xfe)]
}

fn test_packer_packed_varints() {
	mut p := new_packer(EncodeOpts{})
	p.write_packed_varints(1, [u64(1), 2, 3])
	// one tag, one length, then the values
	assert p.bytes() == [u8(0x0a), u8(0x03), u8(0x01), u8(0x02), u8(0x03)]
}

fn test_packer_packed_empty_still_writes_a_header() {
	// A present-but-empty list and an absent field are different on the wire,
	// so the empty case still emits the tag and a zero length.
	mut p := new_packer(EncodeOpts{})
	p.write_packed_varints(1, []u64{})
	assert p.bytes() == [u8(0x0a), u8(0x00)]
}

fn test_packer_packed_sint32s() {
	mut p := new_packer(EncodeOpts{})
	p.write_packed_sint32s(1, [-1, 1, -2])
	// zigzag: -1 -> 1, 1 -> 2, -2 -> 3
	assert p.bytes() == [u8(0x0a), u8(0x03), u8(0x01), u8(0x02), u8(0x03)]
}

fn test_packer_packed_fixed_and_float() {
	mut p := new_packer(EncodeOpts{})
	p.write_packed_fixed32s(1, [u32(1), 2])
	// two 32-bit values, little-endian
	assert p.bytes() == [u8(0x0a), u8(0x08), u8(0x01), u8(0x00), u8(0x00), u8(0x00), u8(0x02),
		u8(0x00), u8(0x00), u8(0x00)]

	mut q := new_packer(EncodeOpts{})
	q.write_packed_floats(1, [f32(1.0)])
	assert q.bytes() == [u8(0x0a), u8(0x04), u8(0x00), u8(0x00), u8(0x80), u8(0x3f)]

	mut r := new_packer(EncodeOpts{})
	r.write_packed_doubles(1, [f64(1.0)])
	assert r.bytes() == [u8(0x0a), u8(0x08), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00),
		u8(0x00), u8(0xf0), u8(0x3f)]
}

fn test_packer_many_fields_concatenate() {
	mut p := new_packer(EncodeOpts{})
	p.write_string(1, 'ab')!
	p.write_uint32(2, 3)
	p.write_bool(3, true)
	out := p.bytes()
	assert out == [
		u8(0x0a),
		u8(0x02),
		u8(0x61),
		u8(0x62),
		u8(0x10),
		u8(0x03),
		u8(0x18),
		u8(0x01),
	]
}

module protobuf

// KeyCase is a field number paired with the wire type it should survive as.
struct KeyCase {
	number int
	wire   WireType
}

fn test_field_key_layout() {
	// The three low bits hold the wire type; everything above is the field
	// number shifted up by three.
	assert field_key(1, .varint) == 0x08
	assert field_key(1, .fixed64) == 0x09
	assert field_key(1, .length_delimited) == 0x0a
	assert field_key(2, .varint) == 0x10
	assert field_key(2, .fixed64) == 0x11
	assert field_key(3, .length_delimited) == 0x1a
	assert field_key(15, .fixed32) == 0x7d
	assert field_key(16, .varint) == 0x80
	assert field_key(16, .fixed64) == 0x81
	assert field_key(2047, .length_delimited) == 0x3ffa
	assert field_key(2048, .varint) == 0x4000
}

fn test_split_field_key_round_trip() {
	cases := [
		KeyCase{
			number: 1
			wire:   .varint
		},
		KeyCase{
			number: 1
			wire:   .length_delimited
		},
		KeyCase{
			number: 2
			wire:   .fixed64
		},
		KeyCase{
			number: 15
			wire:   .fixed32
		},
		KeyCase{
			number: 16
			wire:   .varint
		},
		KeyCase{
			number: 1234
			wire:   .length_delimited
		},
		KeyCase{
			number: max_field_number
			wire:   .fixed32
		},
	]
	for c in cases {
		field, wire := split_field_key(field_key(c.number, c.wire))
		assert field == c.number, 'field number survived as ${field}'
		assert wire == c.wire, 'wire type survived as ${wire}'
	}
}

fn test_split_field_key_masks_wire_type() {
	// Six and seven are not assigned by the spec, but the mask still has to
	// keep them out of the field number rather than letting them bleed upward.
	high, _ := split_field_key(u64(0x0f))
	assert high == 1
	low, _ := split_field_key(u64(0x07))
	assert low == 0
}

fn test_max_field_number_fits_a_varint() {
	// The largest legal tag is 29 bits of field number plus 3 of wire type,
	// which is still only 32 bits, so five bytes at most.
	key := field_key(max_field_number, .length_delimited)
	assert varint_size(key) == 5
	field, wire := split_field_key(key)
	assert field == max_field_number
	assert wire == .length_delimited
}

fn test_field_key_valid_range() {
	assert field_key_valid(1)
	assert field_key_valid(max_field_number)
	assert !field_key_valid(0)
	assert !field_key_valid(-1)
	assert !field_key_valid(max_field_number + 1)
}

fn test_field_key_is_one_varint() {
	// Every field number a schema may use has to fit the tag encoding, or the
	// tag and the value would run together on the wire.
	for number in [1, 15, 16, 1000, 100000, max_field_number] {
		for wire in [WireType.varint, WireType.fixed64, WireType.length_delimited, WireType.fixed32] {
			mut buf := []u8{}
			put_varint(mut buf, field_key(number, wire))
			back, pos := read_varint(buf, 0)!
			assert pos == buf.len
			got_field, got_wire := split_field_key(back)
			assert got_field == number, 'field ${number} came back as ${got_field}'
			assert got_wire == wire
		}
	}
}

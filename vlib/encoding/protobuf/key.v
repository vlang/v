module protobuf

// max_field_number is the largest field number a protobuf message can carry.
// A tag is one varint holding the field number shifted left three bits plus
// three bits of wire type, and the spec caps the number at 29 bits so the
// shift can never overflow.
pub const max_field_number = (1 << 29) - 1

// min_field_number is the smallest legal field number. Zero is reserved: a tag
// of zero would carry no field and no wire type, so it cannot appear.
pub const min_field_number = 1

// field_key builds the tag written ahead of a field's value: the field number
// shifted left three bits, or-ed with the wire type in the low three.
pub fn field_key(field_number int, wire_type WireType) u64 {
	return u64(field_number) << 3 | u64(wire_type)
}

// split_field_key splits a tag read off the wire into its field number and wire
// type. The wire type is not validated here, because a tag carrying an
// unassigned value is a malformed tag rather than an unknown field, and the
// two need different handling.
//
// The field number is converted to `int` without a range check, so a caller
// reading untrusted input has to check `key >> 3` against `max_field_number`
// first: where `int` is 32 bits the conversion wraps. `Unpacker.read_tag` does.
pub fn split_field_key(key u64) (int, WireType) {
	// The mask guarantees 0..7, so the conversion cannot produce a value
	// outside the enum.
	return int(key >> 3), unsafe { WireType(key & 7) }
}

// field_key_valid reports whether `field_number` is inside the range the spec
// allows.
pub fn field_key_valid(field_number int) bool {
	return field_number >= min_field_number && field_number <= max_field_number
}

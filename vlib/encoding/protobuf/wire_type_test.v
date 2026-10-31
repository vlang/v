module protobuf

fn test_wire_type_numbers_are_fixed_by_the_spec() {
	// These numbers are the encoding, not an implementation detail. Changing
	// one would silently break interoperability, so they are asserted directly.
	assert int(WireType.varint) == 0
	assert int(WireType.fixed64) == 1
	assert int(WireType.length_delimited) == 2
	assert int(WireType.start_group) == 3
	assert int(WireType.end_group) == 4
	assert int(WireType.fixed32) == 5
}

fn test_wire_type_valid() {
	assert wire_type_valid(0)
	assert wire_type_valid(1)
	assert wire_type_valid(2)
	assert wire_type_valid(5)
	// Six and seven are unassigned, and a tag carrying one is malformed.
	assert !wire_type_valid(6)
	assert !wire_type_valid(7)
	assert !wire_type_valid(-1)
}

fn test_is_packable() {
	// The spec packs every repeated numeric type and leaves the rest alone.
	assert is_packable(.varint)
	assert is_packable(.fixed32)
	assert is_packable(.fixed64)
	assert !is_packable(.length_delimited)
}

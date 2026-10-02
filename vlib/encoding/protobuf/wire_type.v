module protobuf

// WireType says how a field's value is laid out on the wire. The low three
// bits of every field tag carry it, so the numbering is fixed by the protobuf
// encoding spec and must not be reordered.
pub enum WireType {
	// varint carries bool, the integer types, and enums as base 128.
	varint           = 0
	// fixed64 carries fixed64, sfixed64, and double as eight little-endian
	// bytes.
	fixed64          = 1
	// length_delimited carries string, bytes, embedded messages, and packed
	// repeated fields, each behind a varint byte count.
	length_delimited = 2
	// start_group opens a group, a proto2 construct the spec deprecated and
	// proto3 forbids. Listed for tag decoding only; this module cannot encode
	// or skip one.
	start_group      = 3
	// end_group closes a group. See WireType.start_group.
	end_group        = 4
	// fixed32 carries fixed32, sfixed32, and float as four little-endian
	// bytes.
	fixed32          = 5
}

// wire_type_valid reports whether `n` names a wire type this encoding spec
// defines. Six and seven are unassigned, and a tag carrying one is malformed
// rather than merely unknown, so callers reject it instead of skipping it.
pub fn wire_type_valid(n int) bool {
	return n >= int(WireType.varint) && n <= int(WireType.fixed32)
}

// is_packable reports whether a repeated field of `wt`'s element wire type may
// use the packed encoding, which is a single length-delimited run of values
// instead of one tag per value. The spec packs every repeated numeric type and
// leaves string, bytes, and message unpacked.
pub fn is_packable(wt WireType) bool {
	return wt != .length_delimited
}

// check_wire_type returns a WireTypeMismatchError unless `got` is the wire type
// a field declared to use `want` would arrive as.
//
// A generated codec calls this once per field, so a producer that sends the
// wrong wire type is reported against the field number instead of having its
// bytes reinterpreted into a plausible-looking wrong value. A repeated numeric
// field is the one case where a mismatch is not an error, since it may arrive
// packed; the generated decoder checks that case itself.
pub fn check_wire_type(field_number int, got WireType, want WireType) ! {
	if got != want {
		return wire_type_mismatch(field_number, want, got)
	}
}

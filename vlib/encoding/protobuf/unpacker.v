module protobuf

import encoding.utf8.validate

// DecodeOpts configures an Unpacker. The defaults are the ones that keep a
// decoder safe on untrusted input while still interoperating.
@[params]
pub struct DecodeOpts {
pub:
	// allow_unknown_fields skips a field this build has no mapping for. It
	// defaults to true because forward compatibility is a requirement of the
	// format: a newer producer must not break an older consumer. Set it false
	// to reject such a field instead.
	allow_unknown_fields bool = true
	// reject_nonminimal_varint fails on a varint padded with redundant zero
	// groups. The spec asks parsers to accept those, so this is off by default
	// and is there for callers that would rather be strict about a producer.
	reject_nonminimal_varint bool
	// max_depth bounds how deeply messages may nest. Nesting without a bound
	// is a denial of service rather than a long message, since each level costs
	// only a few bytes on the wire.
	max_depth int = 100
	// max_length bounds a single length-delimited payload, and therefore the
	// whole message. A length on the wire is a 64-bit value, so this is the
	// check that stops a handful of bytes from claiming a huge slice.
	max_length int = 64 * 1024 * 1024
	// validate_utf8 rejects a `string` field whose bytes are not valid UTF-8.
	// A `bytes` field is exempt either way, since it carries arbitrary octets.
	validate_utf8 bool
}

// Unpacker reads protobuf-encoded fields out of a byte slice.
//
// It tracks a position rather than reslicing, so a field's offset is always
// known and can be reported in an error — the only way to locate a problem
// inside a nested payload, since a protobuf message has no line or column.
pub struct Unpacker {
mut:
	data  []u8
	pos   int
	depth int
	opts  DecodeOpts
}

// new_unpacker returns an Unpacker positioned at the start of `data`.
pub fn new_unpacker(data []u8, opts DecodeOpts) &Unpacker {
	return &Unpacker{
		data: data
		opts: opts
	}
}

// eof reports whether the whole input has been consumed.
pub fn (u &Unpacker) eof() bool {
	return u.pos >= u.data.len
}

// remaining returns how many bytes are left unread.
pub fn (u &Unpacker) remaining() int {
	return u.data.len - u.pos
}

// offset returns the current read offset.
pub fn (u &Unpacker) offset() int {
	return u.pos
}

// seek moves the read position to `pos`, an offset previously returned by
// `offset`.
//
// It is for a hand-written reader that peeks at a tag to find out which field
// comes next: when the tag turns out to belong to someone else, seeking back to
// the offset taken before the peek hands it back to be read again.
pub fn (mut u Unpacker) seek(pos int) {
	u.pos = pos
}

// read_tag reads a field tag, returning the field number and wire type. A tag
// naming an unassigned wire type is malformed rather than unknown, so it is
// rejected here instead of being left for the caller to skip.
pub fn (mut u Unpacker) read_tag() !(int, WireType) {
	at := u.pos
	key := u.read_varint()!
	// The field number is range-checked while it is still 64 bits wide. Where
	// `int` is 32 bits, converting first would wrap a number such as 2^32 + 1
	// to 1, which is in range and would be read as a different field.
	if key >> 3 > u64(max_field_number) {
		return malformed_at(at, 'field number ${key >> 3} is outside the legal range')
	}
	number, wire_type := split_field_key(key)
	if !wire_type_valid(int(wire_type)) {
		return unknown_wire_type(at, int(wire_type))
	}
	if !field_key_valid(number) {
		return malformed_at(at, 'field number ${number} is outside the legal range')
	}
	return number, wire_type
}

// read_varint reads one base-128 varint.
pub fn (mut u Unpacker) read_varint() !u64 {
	if u.opts.reject_nonminimal_varint {
		start := u.pos
		mut first := u.pos
		mut extra := false
		for _ in 0 .. max_varint_bytes {
			if first >= u.data.len {
				return unexpected_eof_at(start, u.data.len - start + 1, u.data.len -
					start)
			}
			b := u.data[first]
			if first > start && b == 0 {
				extra = true
			}
			first++
			if b & 0x80 == 0 {
				break
			}
		}
		if extra {
			return malformed_at(start, 'varint uses more bytes than necessary')
		}
	}
	value, next := read_varint(u.data, u.pos)!
	u.pos = next
	return value
}

// read_bool reads a `bool`. Any non-zero value is true, which is what proto3
// specifies, so a producer that wrote 2 rather than 1 is still understood.
pub fn (mut u Unpacker) read_bool() !bool {
	value := u.read_varint()!
	return value != 0
}

// read_uint32 reads a `uint32` varint.
pub fn (mut u Unpacker) read_uint32() !u32 {
	value := u.read_varint()!
	return u32(value)
}

// read_uint64 reads a `uint64` varint.
pub fn (mut u Unpacker) read_uint64() !u64 {
	value := u.read_varint()!
	return value
}

// read_int32 reads an `int32` varint, truncating to 32 bits as the spec
// requires for a value a producer wrote wider than the field allows.
pub fn (mut u Unpacker) read_int32() !i32 {
	value := u.read_varint()!
	return i32(i64(value))
}

// read_int64 reads an `int64` varint.
pub fn (mut u Unpacker) read_int64() !i64 {
	value := u.read_varint()!
	return i64(value)
}

// read_sint32 reads a `sint32` varint and unzigzags it.
pub fn (mut u Unpacker) read_sint32() !i32 {
	value := u.read_varint()!
	return zigzag_decode_i32(u32(value))
}

// read_sint64 reads a `sint64` varint and unzigzags it.
pub fn (mut u Unpacker) read_sint64() !i64 {
	value := u.read_varint()!
	return zigzag_decode_i64(value)
}

// read_enum reads an `enum` varint.
pub fn (mut u Unpacker) read_enum() !int {
	value := u.read_varint()!
	return int(i32(i64(value)))
}

// read_fixed32 reads four little-endian bytes.
pub fn (mut u Unpacker) read_fixed32() !u32 {
	value, next := get_fixed32(u.data, u.pos)!
	u.pos = next
	return value
}

// read_sfixed32 reads four little-endian bytes as a signed value.
pub fn (mut u Unpacker) read_sfixed32() !i32 {
	return i32(u.read_fixed32()!)
}

// read_float reads the four little-endian bytes of an IEEE-754 binary32.
pub fn (mut u Unpacker) read_float() !f32 {
	value, next := get_float(u.data, u.pos)!
	u.pos = next
	return value
}

// read_fixed64 reads eight little-endian bytes.
pub fn (mut u Unpacker) read_fixed64() !u64 {
	value, next := get_fixed64(u.data, u.pos)!
	u.pos = next
	return value
}

// read_sfixed64 reads eight little-endian bytes as a signed value.
pub fn (mut u Unpacker) read_sfixed64() !i64 {
	return i64(u.read_fixed64()!)
}

// read_double reads the eight little-endian bytes of an IEEE-754 binary64.
pub fn (mut u Unpacker) read_double() !f64 {
	value, next := get_double(u.data, u.pos)!
	u.pos = next
	return value
}

// read_len_delimited reads a byte count and returns the payload it describes.
// The returned slice aliases the input rather than copying it.
//
// The count arrives as a 64-bit value while the host's `int` may be 32 bits, so
// it is compared against the configured ceiling *before* being cast. Casting
// first would let a small input name a length that wraps negative, and the
// resulting slice would be nonsense rather than a clean error.
pub fn (mut u Unpacker) read_len_delimited() ![]u8 {
	at := u.pos
	count := u.read_varint()!
	if count > u64(u.opts.max_length) {
		return max_length_exceeded(at, i64(count), u.opts.max_length)
	}
	length := int(count)
	// Subtracting rather than adding keeps the comparison safe even when
	// `length` is close to the limit and `u.pos` is large.
	avail := u.data.len - u.pos
	if length > avail {
		return unexpected_eof_at(at, length, avail)
	}
	// The payload aliases the input rather than copying it: a message can be
	// large, and the caller is told to copy if it needs the bytes to outlive
	// the buffer.
	payload := unsafe { u.data[u.pos..u.pos + length] }
	u.pos += length
	return payload
}

// read_bytes reads a `bytes` field. The result aliases the input, so copy it if
// the input is going away.
pub fn (mut u Unpacker) read_bytes() ![]u8 {
	return u.read_len_delimited()
}

// read_string reads a `string` field. With `validate_utf8` set, bytes that are
// not valid UTF-8 are rejected; a `string` that fails that check could not be
// turned into a V `string` meaningfully.
pub fn (mut u Unpacker) read_string() !string {
	at := u.pos
	payload := u.read_len_delimited()!
	if u.opts.validate_utf8 && !validate.utf8_data(payload.data, payload.len) {
		return invalid_utf8_at(at)
	}
	return payload.bytestr()
}

// skip_field advances past a value of `wire_type`, which is how a field this
// build does not understand gets stepped over. Without it a newer producer
// would break an older consumer the moment it added a field.
//
// With `allow_unknown_fields` off, the field is reported as an
// UnknownFieldError instead of being skipped.
pub fn (mut u Unpacker) skip_field(field_number int, wire_type WireType) ! {
	if !u.opts.allow_unknown_fields {
		return unknown_field_at(u.pos, field_number)
	}
	match wire_type {
		.varint {
			u.read_varint()!
		}
		.fixed64 {
			u.read_fixed64()!
		}
		.length_delimited {
			u.read_len_delimited()!
		}
		.fixed32 {
			u.read_fixed32()!
		}
		.start_group, .end_group {
			return group_unsupported_at(u.pos, field_number)
		}
	}
}

// enter descends into a nested message, failing once the nesting is deeper than
// the configured bound. Each level of nesting costs only a couple of bytes on
// the wire, so an unbounded one is a denial of service rather than a big
// message.
pub fn (mut u Unpacker) enter() ! {
	if u.depth >= u.opts.max_depth {
		return max_depth_exceeded(u.pos, u.opts.max_depth)
	}
	u.depth++
}

// leave returns from a nested message.
pub fn (mut u Unpacker) leave() {
	if u.depth > 0 {
		u.depth--
	}
}

// sub returns an Unpacker over a nested message's payload, with the depth
// carried over and the parent's position left just past the payload. A failure
// to read the payload is reported here rather than in the sub-reader, so the
// offset in the error points at the enclosing message.
pub fn (mut u Unpacker) sub() !&Unpacker {
	payload := u.read_len_delimited()!
	return &Unpacker{
		data:  payload
		opts:  u.opts
		depth: u.depth
	}
}

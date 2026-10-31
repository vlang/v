module protobuf

import encoding.utf8.validate

// EncodeOpts configures a Packer. The zero value is a reasonable default, so
// `Packer{}` with no options is valid.
@[params]
pub struct EncodeOpts {
pub:
	// initial_cap is the capacity the buffer starts with, so a message that
	// fits never reallocates. Zero means the packer's own default.
	initial_cap int = 256
	// emit_defaults writes a field that holds its default value instead of
	// leaving it off the wire. proto3 omits defaults; proto2 keeps explicit
	// presence, which is what this flag is for.
	emit_defaults bool
	// validate_utf8 rejects a `string` field whose bytes are not valid UTF-8.
	// It is off by default because it costs a pass over the bytes, and a
	// `bytes` field is exempt either way.
	validate_utf8 bool
}

// Packer appends protobuf-encoded bytes to a growable buffer.
//
// A Packer is a plain writer: every `write_*` method emits the field you ask
// for, including a value equal to its default. Applying proto3's rule that
// defaults are not written is the caller's job, which the code `v pbgen`
// generates does for you based on the schema. Keeping the two apart is
// deliberate — a writer that silently dropped values would make the manual API
// impossible to use for proto2, where presence has to survive.
pub struct Packer {
mut:
	buf  []u8
	opts EncodeOpts
}

// new_packer returns a Packer with an empty buffer of `opts.initial_cap`
// capacity.
pub fn new_packer(opts EncodeOpts) &Packer {
	return &Packer{
		buf:  []u8{cap: opts.initial_cap}
		opts: opts
	}
}

// new_packer_from returns a Packer whose buffer starts with a copy of `data`,
// so a message can be appended to an existing one. `data` itself is not
// modified.
pub fn new_packer_from(data []u8, opts EncodeOpts) &Packer {
	mut p := new_packer(opts)
	p.buf << data
	return p
}

// reserve grows the buffer's capacity so at least `n` more bytes fit without
// reallocating. Call it before a known-size payload write to skip the
// per-byte growth check that `<<` carries.
pub fn (mut p Packer) reserve(n int) {
	if n <= 0 {
		return
	}
	needed := p.buf.len + n
	if needed <= p.buf.cap {
		return
	}
	mut new_cap := if p.buf.cap == 0 { 64 } else { p.buf.cap * 2 }
	for new_cap < needed {
		new_cap *= 2
	}
	mut grown := []u8{cap: new_cap}
	grown << p.buf
	p.buf = grown
}

// reset empties the buffer while keeping its capacity, so a Packer can be
// reused across messages without reallocating.
pub fn (mut p Packer) reset() {
	unsafe {
		p.buf.len = 0
	}
}

// bytes returns the encoded output. The result aliases the packer's buffer, so
// copy it before reusing the packer.
pub fn (p &Packer) bytes() []u8 {
	return p.buf
}

// len returns how many bytes have been written so far.
pub fn (p &Packer) len() int {
	return p.buf.len
}

// write_tag writes the field tag that introduces a value, without the value
// itself.
pub fn (mut p Packer) write_tag(field_number int, wire_type WireType) {
	put_varint(mut p.buf, field_key(field_number, wire_type))
}

// write_uint32 writes a `uint32` field.
pub fn (mut p Packer) write_uint32(field_number int, value u32) {
	p.write_tag(field_number, .varint)
	put_varint(mut p.buf, u64(value))
}

// write_uint64 writes a `uint64` field.
pub fn (mut p Packer) write_uint64(field_number int, value u64) {
	p.write_tag(field_number, .varint)
	put_varint(mut p.buf, value)
}

// write_int32 writes an `int32` field. A negative value is sign-extended to 64
// bits, so it costs ten bytes, which is what the spec requires.
pub fn (mut p Packer) write_int32(field_number int, value i32) {
	p.write_tag(field_number, .varint)
	put_varint(mut p.buf, int32_varint(value))
}

// write_int64 writes an `int64` field, sign-extending a negative value.
pub fn (mut p Packer) write_int64(field_number int, value i64) {
	p.write_tag(field_number, .varint)
	put_varint(mut p.buf, int64_varint(value))
}

// write_sint32 writes a `sint32` field, zigzagged so a small negative stays
// small.
pub fn (mut p Packer) write_sint32(field_number int, value i32) {
	p.write_tag(field_number, .varint)
	put_varint(mut p.buf, u64(zigzag_encode_i32(value)))
}

// write_sint64 writes a `sint64` field, zigzagged.
pub fn (mut p Packer) write_sint64(field_number int, value i64) {
	p.write_tag(field_number, .varint)
	put_varint(mut p.buf, zigzag_encode_i64(value))
}

// write_bool writes a `bool` field.
pub fn (mut p Packer) write_bool(field_number int, value bool) {
	p.write_tag(field_number, .varint)
	put_varint(mut p.buf, if value { u64(1) } else { u64(0) })
}

// write_enum writes an `enum` field. Enums travel as their integer value.
pub fn (mut p Packer) write_enum(field_number int, value int) {
	p.write_tag(field_number, .varint)
	put_varint(mut p.buf, int32_varint(i32(value)))
}

// write_fixed32 writes a `fixed32` field as four little-endian bytes.
pub fn (mut p Packer) write_fixed32(field_number int, value u32) {
	p.write_tag(field_number, .fixed32)
	put_fixed32(mut p.buf, value)
}

// write_sfixed32 writes an `sfixed32` field as four little-endian bytes.
pub fn (mut p Packer) write_sfixed32(field_number int, value i32) {
	p.write_tag(field_number, .fixed32)
	put_fixed32(mut p.buf, u32(value))
}

// write_float writes a `float` field as the four little-endian bytes of its
// IEEE-754 binary32 bit pattern.
pub fn (mut p Packer) write_float(field_number int, value f32) {
	p.write_tag(field_number, .fixed32)
	put_float(mut p.buf, value)
}

// write_fixed64 writes a `fixed64` field as eight little-endian bytes.
pub fn (mut p Packer) write_fixed64(field_number int, value u64) {
	p.write_tag(field_number, .fixed64)
	put_fixed64(mut p.buf, value)
}

// write_sfixed64 writes an `sfixed64` field as eight little-endian bytes.
pub fn (mut p Packer) write_sfixed64(field_number int, value i64) {
	p.write_tag(field_number, .fixed64)
	put_fixed64(mut p.buf, u64(value))
}

// write_double writes a `double` field as the eight little-endian bytes of its
// IEEE-754 binary64 bit pattern.
pub fn (mut p Packer) write_double(field_number int, value f64) {
	p.write_tag(field_number, .fixed64)
	put_double(mut p.buf, value)
}

// write_bytes writes a `bytes` field: the length, then the bytes unchanged.
pub fn (mut p Packer) write_bytes(field_number int, value []u8) {
	p.write_len_delimited(field_number, value)
}

// write_string writes a `string` field. With `validate_utf8` set, bytes that
// are not valid UTF-8 are rejected rather than written, since a decoder on the
// other end would reject them anyway.
pub fn (mut p Packer) write_string(field_number int, value string) ! {
	if p.opts.validate_utf8 && !validate.utf8_string(value) {
		return invalid_utf8_at(p.buf.len)
	}
	p.write_len_delimited(field_number, value.bytes())
}

// write_message writes an already-encoded message as a length-delimited
// field. Generated code encodes a nested message into a buffer of its own and
// splices it in here.
pub fn (mut p Packer) write_message(field_number int, encoded []u8) {
	p.write_len_delimited(field_number, encoded)
}

// write_len_delimited writes a length-delimited field: the tag, the byte count,
// then the payload verbatim.
pub fn (mut p Packer) write_len_delimited(field_number int, payload []u8) {
	p.write_tag(field_number, .length_delimited)
	put_varint(mut p.buf, u64(payload.len))
	p.buf << payload
}

// write_packed_payload writes a repeated numeric field in the packed form: one
// tag, one byte count, then every element already encoded back to back with no
// per-element tag.
//
// Packing is the default the spec sets for repeated numeric types, and an empty
// list still writes the tag and a zero length, which is what keeps a
// present-but-empty list distinguishable from an absent one.
//
// The elements are supplied pre-encoded because a V type does not determine the
// wire encoding on its own: `i32` is `int32`, `sint32`, or `sfixed32` depending
// on the schema, and only the schema knows which. `write_packed_varints`,
// `write_packed_fixed32s` and `write_packed_fixed64s` cover the common cases;
// generated code builds the payload itself because it knows the schema type.
pub fn (mut p Packer) write_packed_payload(field_number int, wire_type WireType, payload []u8) {
	p.write_tag(field_number, .length_delimited)
	put_varint(mut p.buf, u64(payload.len))
	p.buf << payload
}

// write_packed_varints writes a packed field from `int32`, `int64`, `uint32`,
// `uint64`, or enum elements, whose encodings are all plain varints once
// negative values are sign-extended.
pub fn (mut p Packer) write_packed_varints(field_number int, values []u64) {
	mut payload := []u8{cap: values.len}
	for value in values {
		put_varint(mut payload, value)
	}
	p.write_packed_payload(field_number, .varint, payload)
}

// write_packed_sint32s writes a packed field from `sint32` elements, zigzagged.
pub fn (mut p Packer) write_packed_sint32s(field_number int, values []i32) {
	mut payload := []u8{cap: values.len}
	for value in values {
		put_varint(mut payload, u64(zigzag_encode_i32(value)))
	}
	p.write_packed_payload(field_number, .varint, payload)
}

// write_packed_sint64s writes a packed field from `sint64` elements, zigzagged.
pub fn (mut p Packer) write_packed_sint64s(field_number int, values []i64) {
	mut payload := []u8{cap: values.len}
	for value in values {
		put_varint(mut payload, zigzag_encode_i64(value))
	}
	p.write_packed_payload(field_number, .varint, payload)
}

// write_packed_bools writes a packed field from `bool` elements.
pub fn (mut p Packer) write_packed_bools(field_number int, values []bool) {
	mut payload := []u8{cap: values.len}
	for value in values {
		put_varint(mut payload, if value { u64(1) } else { u64(0) })
	}
	p.write_packed_payload(field_number, .varint, payload)
}

// write_packed_fixed32s writes a packed field from `fixed32` or `sfixed32`
// elements.
pub fn (mut p Packer) write_packed_fixed32s(field_number int, values []u32) {
	mut payload := []u8{cap: values.len * 4}
	for value in values {
		put_fixed32(mut payload, value)
	}
	p.write_packed_payload(field_number, .fixed32, payload)
}

// write_packed_floats writes a packed field from `float` elements.
pub fn (mut p Packer) write_packed_floats(field_number int, values []f32) {
	mut payload := []u8{cap: values.len * 4}
	for value in values {
		put_float(mut payload, value)
	}
	p.write_packed_payload(field_number, .fixed32, payload)
}

// write_packed_fixed64s writes a packed field from `fixed64` or `sfixed64`
// elements.
pub fn (mut p Packer) write_packed_fixed64s(field_number int, values []u64) {
	mut payload := []u8{cap: values.len * 8}
	for value in values {
		put_fixed64(mut payload, value)
	}
	p.write_packed_payload(field_number, .fixed64, payload)
}

// write_packed_doubles writes a packed field from `double` elements.
pub fn (mut p Packer) write_packed_doubles(field_number int, values []f64) {
	mut payload := []u8{cap: values.len * 8}
	for value in values {
		put_double(mut payload, value)
	}
	p.write_packed_payload(field_number, .fixed64, payload)
}

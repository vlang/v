// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// QPACK encoder and decoder stream instructions (RFC 9204 §4.3, §4.4).
// Encoder stream (unidirectional type 0x02) carries instructions to update
// the decoder's dynamic table. Decoder stream (unidirectional type 0x03)
// carries acknowledgments and flow control signals back to the encoder.

// --- Encoder Stream Instructions (RFC 9204 §4.3.1) ---

// InsertWithNameRef inserts a new entry into the dynamic table using a name
// reference from the static or dynamic table. Wire format:
//   1T NNNNNN — T=1 static, T=0 dynamic; N = name index (6-bit prefix)
//   followed by value string.
pub struct InsertWithNameRef {
pub:
	is_static  bool
	name_index int
	value      string
}

// encode serialises the instruction onto the encoder stream.
pub fn (i InsertWithNameRef) encode() []u8 {
	mut result := []u8{cap: 10 + i.value.len}
	// First byte: 1T NNNNNN
	flag := if i.is_static { u8(0xc0) } else { u8(0x80) }
	if i.name_index < 64 {
		result << flag | u8(i.name_index)
	} else {
		mut suffix := encode_integer(i.name_index, 6)
		suffix[0] |= flag
		result << suffix
	}
	result << encode_qpack_string(i.value)
	return result
}

// decode_insert_with_name_ref decodes an InsertWithNameRef instruction from data.
pub fn decode_insert_with_name_ref(data []u8) !(InsertWithNameRef, int) {
	if data.len == 0 {
		return error('empty data for InsertWithNameRef')
	}
	is_static := (data[0] & 0x40) != 0
	index, idx := decode_prefixed_integer(data, 6)!
	value, vlen := decode_qpack_string(data[idx..])!
	return InsertWithNameRef{
		is_static:  is_static
		name_index: index
		value:      value
	}, idx + vlen
}

// InsertWithoutNameRef inserts a new entry with a literal name and value.
// Wire format: 01 NNNNNN — N = name length (5-bit prefix with H flag),
// followed by name string, then value string.
pub struct InsertWithoutNameRef {
pub:
	name  string
	value string
}

// encode serialises the instruction onto the encoder stream.
pub fn (i InsertWithoutNameRef) encode() []u8 {
	mut result := []u8{cap: 10 + i.name.len + i.value.len}
	// First byte: 01 XXXXXX — use 0x40 as base; name follows with 5-bit prefix
	name_bytes := i.name.bytes()
	mut name_len := encode_integer(name_bytes.len, 5)
	name_len[0] |= 0x40
	result << name_len
	result << name_bytes
	result << encode_qpack_string(i.value)
	return result
}

// decode_insert_without_name_ref decodes an InsertWithoutNameRef instruction.
pub fn decode_insert_without_name_ref(data []u8) !(InsertWithoutNameRef, int) {
	if data.len == 0 {
		return error('empty data for InsertWithoutNameRef')
	}
	name_len, idx := decode_prefixed_integer(data, 5)!
	end := idx + name_len
	if end > data.len {
		return error('InsertWithoutNameRef name exceeds data')
	}
	name := data[idx..end].bytestr()
	value, vlen := decode_qpack_string(data[end..])!
	return InsertWithoutNameRef{
		name:  name
		value: value
	}, end + vlen
}

// Duplicate duplicates an existing dynamic table entry.
// Wire format: 000 XXXXX — 5-bit prefix for relative index.
pub struct Duplicate {
pub:
	index int
}

// encode serialises the Duplicate instruction.
pub fn (d Duplicate) encode() []u8 {
	mut result := encode_integer(d.index, 5)
	// High 3 bits must be 000 — encode_integer for 5-bit prefix
	// already leaves bits 7-5 as 0 for values < 31.
	return result
}

// decode_duplicate decodes a Duplicate instruction from data.
pub fn decode_duplicate(data []u8) !(Duplicate, int) {
	if data.len == 0 {
		return error('empty data for Duplicate')
	}
	index, bytes_read := decode_prefixed_integer(data, 5)!
	return Duplicate{
		index: index
	}, bytes_read
}

// SetDynamicTableCapacity sets the dynamic table capacity.
// Wire format: 001 XXXXX — 5-bit prefix for capacity.
pub struct SetDynamicTableCapacity {
pub:
	capacity int
}

// encode serialises the SetDynamicTableCapacity instruction.
pub fn (s SetDynamicTableCapacity) encode() []u8 {
	mut result := encode_integer(s.capacity, 5)
	result[0] |= 0x20
	return result
}

// decode_set_dynamic_table_capacity decodes a SetDynamicTableCapacity instruction.
pub fn decode_set_dynamic_table_capacity(data []u8) !(SetDynamicTableCapacity, int) {
	if data.len == 0 {
		return error('empty data for SetDynamicTableCapacity')
	}
	capacity, bytes_read := decode_prefixed_integer(data, 5)!
	return SetDynamicTableCapacity{
		capacity: capacity
	}, bytes_read
}

// --- Decoder Stream Instructions (RFC 9204 §4.4.1) ---

// SectionAcknowledgment acknowledges processing of a header block
// on the given stream. Wire format: 1 XXXXXXX — 7-bit stream ID prefix.
pub struct SectionAcknowledgment {
pub:
	stream_id int
}

// encode serialises the SectionAcknowledgment instruction.
pub fn (s SectionAcknowledgment) encode() []u8 {
	mut result := encode_integer(s.stream_id, 7)
	result[0] |= 0x80
	return result
}

// decode_section_acknowledgment decodes a SectionAcknowledgment instruction.
pub fn decode_section_acknowledgment(data []u8) !(SectionAcknowledgment, int) {
	if data.len == 0 {
		return error('empty data for SectionAcknowledgment')
	}
	stream_id, bytes_read := decode_prefixed_integer(data, 7)!
	return SectionAcknowledgment{
		stream_id: stream_id
	}, bytes_read
}

// StreamCancellation cancels all references to a stream.
// Wire format: 01 XXXXXX — 6-bit stream ID prefix.
pub struct StreamCancellation {
pub:
	stream_id int
}

// encode serialises the StreamCancellation instruction.
pub fn (s StreamCancellation) encode() []u8 {
	mut result := encode_integer(s.stream_id, 6)
	result[0] |= 0x40
	return result
}

// decode_stream_cancellation decodes a StreamCancellation instruction.
pub fn decode_stream_cancellation(data []u8) !(StreamCancellation, int) {
	if data.len == 0 {
		return error('empty data for StreamCancellation')
	}
	stream_id, bytes_read := decode_prefixed_integer(data, 6)!
	return StreamCancellation{
		stream_id: stream_id
	}, bytes_read
}

// InsertCountIncrement signals that the decoder has processed additional
// dynamic table inserts. Wire format: 00 XXXXXX — 6-bit increment prefix.
pub struct InsertCountIncrement {
pub:
	increment int
}

// encode serialises the InsertCountIncrement instruction.
pub fn (i InsertCountIncrement) encode() []u8 {
	return encode_integer(i.increment, 6)
}

// decode_insert_count_increment decodes an InsertCountIncrement instruction.
pub fn decode_insert_count_increment(data []u8) !(InsertCountIncrement, int) {
	if data.len == 0 {
		return error('empty data for InsertCountIncrement')
	}
	increment, bytes_read := decode_prefixed_integer(data, 6)!
	return InsertCountIncrement{
		increment: increment
	}, bytes_read
}

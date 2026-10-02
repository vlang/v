module kv

// roundtrip_varint encodes v and decodes it straight back, returning 0 if the
// round trip fails. decode_varint returns two values, which cannot be compared
// inline, so the tests go through this helper.
fn roundtrip_varint(v u64) u64 {
	encoded := encode_varint(mut []u8{}, v)
	got, _ := decode_varint(encoded, 0) or { return 0 }
	return got
}

fn test_varint_roundtrip_single_byte() {
	assert roundtrip_varint(0) == 0
	assert roundtrip_varint(1) == 1
	assert roundtrip_varint(127) == 127
}

fn test_varint_roundtrip_multi_byte() {
	assert roundtrip_varint(128) == 128
	assert roundtrip_varint(300) == 300
	assert roundtrip_varint(0xffffffff) == 0xffffffff
	assert roundtrip_varint(0xdeadbeefcafe) == 0xdeadbeefcafe
}

fn test_varint_uses_base128_not_little_endian() {
	// 300 is 0b100101100. A little-endian encoding would be 2c 01, but varint
	// is low-7-bits-first, so it must be ac 02.
	assert encode_varint(mut []u8{}, 300) == [u8(0xac), u8(0x02)]
}

fn test_varint_truncated_is_an_error() {
	if _, _ := decode_varint([u8(0xac)], 0) {
		assert false, 'a varint missing its final byte must error'
	}
}

fn test_varint_overlong_is_an_error() {
	// Eleven bytes all with the continuation bit set: longer than the ten a
	// u64 can need, so no terminating byte is ever reached.
	mut overlong := []u8{len: max_varint_bytes + 1}
	for i in 0 .. overlong.len {
		overlong[i] = 0x80
	}
	if _, _ := decode_varint(overlong, 0) {
		assert false, 'a varint longer than ${max_varint_bytes} bytes must error'
	}
}

fn test_varint_reports_the_offset_it_stopped_at() {
	mut data := []u8{cap: 4}
	data = encode_varint(mut data, 300) // two bytes
	data << [u8(0xff), u8(0xff)]
	value, next := decode_varint(data, 0)!
	assert value == 300
	assert next == 2
}

fn test_tag_encoding() {
	// field 1, wire type 2 -> (1 << 3) | 2 == 0x0a
	assert append_tag(mut []u8{}, 1, wire_len) == [u8(0x0a)]
	// field 1, wire type 0 -> 0x08
	assert append_tag(mut []u8{}, 1, wire_varint) == [u8(0x08)]
	// field 2, wire type 0 -> (2 << 3) | 0 == 0x10
	assert append_tag(mut []u8{}, 2, wire_varint) == [u8(0x10)]
}

fn test_get_request_roundtrip() {
	msg := GetRequest{
		key: 'answer'
	}
	got := decode_get_request(msg.encode())!
	assert got.key == 'answer'
}

fn test_get_request_omits_the_default_empty_key() {
	// proto3 never writes a default-valued scalar, so an empty key encodes to
	// zero bytes and decodes back to the same default.
	assert GetRequest{}.encode() == []u8{}
	assert decode_get_request([]u8{})!.key == ''
}

fn test_get_response_roundtrip() {
	msg := GetResponse{
		value: 'world'.bytes()
		found: true
	}
	got := decode_get_response(msg.encode())!
	assert got.value == 'world'.bytes()
	assert got.found
}

fn test_get_response_found_false_is_omitted() {
	// found=false is the proto3 default, so an empty value and a false found
	// together mean the whole message encodes to nothing at all.
	assert GetResponse{}.encode() == []u8{}
	got := decode_get_response([]u8{})!
	assert !got.found
	assert got.value.len == 0
}

fn test_get_response_found_only() {
	// found=true with no value: the bool is written, the empty bytes are not
	got := decode_get_response(GetResponse{
		found: true
	}.encode())!
	assert got.found
	assert got.value.len == 0
}

fn test_put_request_roundtrip() {
	msg := PutRequest{
		key:   'greeting'
		value: 'hello'.bytes()
	}
	got := decode_put_request(msg.encode())!
	assert got.key == 'greeting'
	assert got.value == 'hello'.bytes()
}

fn test_put_request_key_only() {
	got := decode_put_request(PutRequest{
		key: 'lonely'
	}.encode())!
	assert got.key == 'lonely'
	assert got.value.len == 0
}

fn test_put_response_roundtrip() {
	assert decode_put_response(PutResponse{
		replaced: true
	}.encode())!.replaced
	assert PutResponse{}.encode() == []u8{}
	assert !decode_put_response([]u8{})!.replaced
}

fn test_put_many_response_roundtrip() {
	got := decode_put_many_response(PutManyResponse{
		written: 3
	}.encode())!
	assert got.written == 3
	assert PutManyResponse{}.encode() == []u8{}
	assert decode_put_many_response([]u8{})!.written == 0
}

fn test_put_many_response_large_count() {
	// int32, so negative values round-trip as their two's-complement varint
	got := decode_put_many_response(PutManyResponse{
		written: 100000
	}.encode())!
	assert got.written == 100000
}

// a producer that knows a field the consumer does not must not break it
fn test_unknown_fields_are_skipped() {
	mut data := GetRequest{
		key: 'kept'
	}.encode()
	// an unknown varint field 9 and an unknown bytes field 10
	data = append_tag(mut data, 9, wire_varint)
	data = encode_varint(mut data, 1234)
	data = append_len(mut data, 10, 'ignored'.bytes())
	got := decode_get_request(data)!
	assert got.key == 'kept'
}

fn test_unknown_fixed_width_fields_are_skipped() {
	mut data := PutResponse{
		replaced: true
	}.encode()
	data = append_tag(mut data, 7, wire_fixed64)
	data << '\x00'.repeat(8).bytes()
	data = append_tag(mut data, 8, wire_fixed32)
	data << '\x00'.repeat(4).bytes()
	assert decode_put_response(data)!.replaced
}

fn test_unknown_field_appearing_before_a_known_one() {
	// the decoder must keep scanning rather than give up at the unknown tag
	mut data := append_len(mut []u8{}, 9, 'noise'.bytes())
	data = append_len(mut data, 1, 'kept'.bytes())
	assert decode_get_request(data)!.key == 'kept'
}

// a length-delimited field claiming more bytes than remain must be rejected,
// not read past the end of the buffer
fn test_length_running_past_the_message_is_an_error() {
	// field 1, wire type 2, declared length 200, only 3 bytes actually follow
	mut data := [u8(0x0a), u8(0xc8), u8(0x01)]
	data << 'abc'.bytes()
	if _ := decode_get_request(data) {
		assert false, 'an over-long length prefix must be rejected'
	}
}

// A known field arriving with the wrong wire type is treated as an unknown
// field and skipped, which is what the protobuf spec requires of a parser: only
// an unsupported *wire type* is a hard error. So `key` keeps its default.
fn test_wrong_wire_type_on_a_known_field_is_skipped() {
	// field 1 declared as a varint, but GetRequest.key is length-delimited
	mut data := [u8(0x08)]
	data = encode_varint(mut data, 5)
	got := decode_get_request(data)!
	assert got.key == ''
}

fn test_truncated_varint_inside_a_field_is_an_error() {
	mut data := [u8(0x08)]
	data << u8(0xac) // continuation bit set, nothing after it
	if _ := decode_put_response(data) {
		assert false, 'a truncated varint must be rejected'
	}
}

fn test_unsupported_wire_type_is_an_error() {
	// wire types 3 and 4 (start/end group) are not valid in proto3
	mut data := [u8(0x0b)]
	data << u8(0x00)
	if _ := decode_get_request(data) {
		assert false, 'a reserved wire type must be rejected'
	}
}

fn test_truncated_fixed_width_field_is_an_error() {
	// field 7, wire type 1 claims 8 bytes but only 2 follow
	mut data := [u8(0x39), u8(0x00), u8(0x00)]
	if _ := decode_put_response(data) {
		assert false, 'a truncated fixed64 must be rejected'
	}
}

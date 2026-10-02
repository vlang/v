// A worked example of the schema in kv.proto, with a hand-written proto3
// encoder/decoder.
//
// net.grpc is transport-only: it moves []u8 payloads and never looks inside
// them, so the codec is entirely the caller's choice. A real project would
// generate this file with protobuf.v's `vpbgen`; writing it out by hand keeps
// the example free of external dependencies and shows what a codec actually has
// to do — only the proto3 wire format below, nothing from net.grpc.
//
// Wire format recap:
//   field  -> tag, where tag = (field_number << 3) | wire_type
//   wire 0 -> varint          (bool, int32, ...)
//   wire 1 -> 64-bit          (fixed64, double)
//   wire 2 -> length-delimited (string, bytes, embedded messages)
//   wire 5 -> 32-bit          (fixed32, float)
//
// The proto3 rule that matters most here: a scalar holding its default value is
// never written. An absent field and a field explicitly set to zero/false/empty
// are the same thing on the wire, so every encoder below skips empty scalars.
module kv

const wire_varint = u8(0)
const wire_fixed64 = u8(1)
const wire_len = u8(2)
const wire_fixed32 = u8(5)

// max_varint_bytes bounds a base-128 varint: 64 bits needs at most ten 7-bit
// groups, so anything longer is malformed rather than merely large.
const max_varint_bytes = 10

// GetRequest is the request for Get and Scan: `string key = 1`.
pub struct GetRequest {
pub mut:
	key string
}

// GetResponse is one stored value: `bytes value = 1; bool found = 2`.
pub struct GetResponse {
pub mut:
	value []u8
	found bool
}

// PutRequest is the request for Put and PutMany: `string key = 1; bytes value = 2`.
pub struct PutRequest {
pub mut:
	key   string
	value []u8
}

// PutResponse reports whether an existing value was overwritten: `bool replaced = 1`.
pub struct PutResponse {
pub mut:
	replaced bool
}

// PutManyResponse counts the stored keys: `int32 written = 1`.
pub struct PutManyResponse {
pub mut:
	written int
}

// encode_varint appends v in base 128: low 7 bits per byte, high bit set on
// every byte but the last.
fn encode_varint(mut buf []u8, v u64) []u8 {
	// shifting needs a local: a `mut v` parameter would force every caller to
	// write `mut 300` at the call site
	mut rest := v
	for rest >= 0x80 {
		buf << u8(rest & 0x7f) | 0x80
		rest >>= 7
	}
	buf << u8(rest)
	return buf
}

// decode_varint reads one base-128 varint at off, returning the value and the
// offset just past it. A varint that runs past the end of the buffer, or that
// exceeds ten bytes, is an error rather than a silent misread.
fn decode_varint(data []u8, off int) !(u64, int) {
	mut value := u64(0)
	mut shift := u32(0)
	mut i := off
	for _ in 0 .. max_varint_bytes {
		if i >= data.len {
			return error('kv: truncated varint at offset ${off}')
		}
		b := data[i]
		value |= u64(b & 0x7f) << shift
		i++
		if b & 0x80 == 0 {
			return value, i
		}
		shift += 7
	}
	return error('kv: varint at offset ${off} is longer than ${max_varint_bytes} bytes')
}

// append_tag writes the tag for a field number and wire type.
fn append_tag(mut buf []u8, field int, wire u8) []u8 {
	return encode_varint(mut buf, u64(field) << 3 | u64(wire))
}

// append_len writes a length-delimited field. Callers guard on emptiness
// themselves, because proto3 omits empty payloads entirely.
fn append_len(mut buf []u8, field int, payload []u8) []u8 {
	buf = append_tag(mut buf, field, wire_len)
	buf = encode_varint(mut buf, u64(payload.len))
	buf << payload
	return buf
}

// append_bool writes a bool field, omitting the false default as proto3 does.
fn append_bool(mut buf []u8, field int, value bool) []u8 {
	if !value {
		return buf
	}
	buf = append_tag(mut buf, field, wire_varint)
	return encode_varint(mut buf, 1)
}

// append_varint writes a varint field, omitting the zero default as proto3 does.
fn append_varint(mut buf []u8, field int, value int) []u8 {
	if value == 0 {
		return buf
	}
	buf = append_tag(mut buf, field, wire_varint)
	return encode_varint(mut buf, u64(value))
}

// skip_field advances off past a field the decoder does not know about, so a
// newer producer adding fields cannot break an older consumer.
fn skip_field(data []u8, off int, wire u8) !int {
	match wire {
		wire_varint {
			_, next := decode_varint(data, off)!
			return next
		}
		wire_fixed64 {
			if off + 8 > data.len {
				return error('kv: truncated fixed64 field at offset ${off}')
			}
			return off + 8
		}
		wire_len {
			_, next := read_len(data, off)!
			return next
		}
		wire_fixed32 {
			if off + 4 > data.len {
				return error('kv: truncated fixed32 field at offset ${off}')
			}
			return off + 4
		}
		else {
			return error('kv: unsupported wire type ${wire} at offset ${off}')
		}
	}
}

// read_len reads the payload of a length-delimited field at off, returning it
// and the offset just past the field. A length that runs past the message is
// rejected rather than trusted.
fn read_len(data []u8, off int) !([]u8, int) {
	n, next := decode_varint(data, off)!
	end := next + int(n)
	if end < next || end > data.len {
		return error('kv: length-delimited field at offset ${off} runs past the message')
	}
	return data[next..end], end
}

// decode_bool_field reads a bool stored as a varint, treating any non-zero value
// as true, which is what proto3 specifies.
fn decode_bool_field(data []u8, off int) !(bool, int) {
	v, next := decode_varint(data, off)!
	return v != 0, next
}

// encode_get_request serializes msg.
pub fn (msg GetRequest) encode() []u8 {
	mut buf := []u8{}
	if msg.key.len > 0 {
		buf = append_len(mut buf, 1, msg.key.bytes())
	}
	return buf
}

// decode_get_request parses data.
pub fn decode_get_request(data []u8) !GetRequest {
	mut msg := GetRequest{}
	mut off := 0
	for off < data.len {
		tag, next := decode_varint(data, off)!
		field := int(tag >> 3)
		wire := u8(tag & 7)
		if field == 1 && wire == wire_len {
			payload, end := read_len(data, next)!
			msg.key = payload.bytestr()
			off = end
		} else {
			off = skip_field(data, next, wire)!
		}
	}
	return msg
}

// encode_get_response serializes msg.
pub fn (msg GetResponse) encode() []u8 {
	mut buf := []u8{}
	if msg.value.len > 0 {
		buf = append_len(mut buf, 1, msg.value)
	}
	buf = append_bool(mut buf, 2, msg.found)
	return buf
}

// decode_get_response parses data.
pub fn decode_get_response(data []u8) !GetResponse {
	mut msg := GetResponse{}
	mut off := 0
	for off < data.len {
		tag, next := decode_varint(data, off)!
		field := int(tag >> 3)
		wire := u8(tag & 7)
		if field == 1 && wire == wire_len {
			payload, end := read_len(data, next)!
			msg.value = payload.clone()
			off = end
		} else if field == 2 && wire == wire_varint {
			msg.found, off = decode_bool_field(data, next)!
		} else {
			off = skip_field(data, next, wire)!
		}
	}
	return msg
}

// encode_put_request serializes msg.
pub fn (msg PutRequest) encode() []u8 {
	mut buf := []u8{}
	if msg.key.len > 0 {
		buf = append_len(mut buf, 1, msg.key.bytes())
	}
	if msg.value.len > 0 {
		buf = append_len(mut buf, 2, msg.value)
	}
	return buf
}

// decode_put_request parses data.
pub fn decode_put_request(data []u8) !PutRequest {
	mut msg := PutRequest{}
	mut off := 0
	for off < data.len {
		tag, next := decode_varint(data, off)!
		field := int(tag >> 3)
		wire := u8(tag & 7)
		if field == 1 && wire == wire_len {
			payload, end := read_len(data, next)!
			msg.key = payload.bytestr()
			off = end
		} else if field == 2 && wire == wire_len {
			payload, end := read_len(data, next)!
			msg.value = payload.clone()
			off = end
		} else {
			off = skip_field(data, next, wire)!
		}
	}
	return msg
}

// encode_put_response serializes msg.
pub fn (msg PutResponse) encode() []u8 {
	return append_bool(mut []u8{}, 1, msg.replaced)
}

// decode_put_response parses data.
pub fn decode_put_response(data []u8) !PutResponse {
	mut msg := PutResponse{}
	mut off := 0
	for off < data.len {
		tag, next := decode_varint(data, off)!
		field := int(tag >> 3)
		wire := u8(tag & 7)
		if field == 1 && wire == wire_varint {
			msg.replaced, off = decode_bool_field(data, next)!
		} else {
			off = skip_field(data, next, wire)!
		}
	}
	return msg
}

// encode_put_many_response serializes msg.
pub fn (msg PutManyResponse) encode() []u8 {
	return append_varint(mut []u8{}, 1, msg.written)
}

// decode_put_many_response parses data.
pub fn decode_put_many_response(data []u8) !PutManyResponse {
	mut msg := PutManyResponse{}
	mut off := 0
	for off < data.len {
		tag, next := decode_varint(data, off)!
		field := int(tag >> 3)
		wire := u8(tag & 7)
		if field == 1 && wire == wire_varint {
			v, end := decode_varint(data, next)!
			msg.written = int(v)
			off = end
		} else {
			off = skip_field(data, next, wire)!
		}
	}
	return msg
}

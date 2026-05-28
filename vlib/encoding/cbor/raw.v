module cbor

// RawMessage holds the byte-exact encoding of one CBOR data item as it
// appeared on the wire. Useful for caching/forwarding code that wants to
// defer decoding of a nested field.
//
//   struct Envelope {
//       id      int
//       payload cbor.RawMessage   // bytes preserved as-is
//   }
//
//   raw  := cbor.decode[cbor.RawMessage](bytes)!
//   back := cbor.encode(raw, cbor.EncodeOpts{})!  // identical bytes
pub struct RawMessage {
pub mut:
	data []u8
}

// pack_raw appends a RawMessage's bytes to the Packer without re-encoding.
// An empty RawMessage is rejected: emitting zero bytes would silently
// drop the slot when the value is a struct field or array element,
// shifting every subsequent item by one — almost always a caller bug.
@[inline]
pub fn (mut p Packer) pack_raw(raw RawMessage) ! {
	if raw.data.len == 0 {
		return error('cbor: pack_raw called with empty RawMessage')
	}
	p.reserve(raw.data.len)
	unsafe { p.buf.push_many(raw.data.data, raw.data.len) }
}

// unpack_raw captures the bytes of the next value without building a
// Value tree. Returns an owned clone, safe to outlive the unpacker.
@[direct_array_access]
pub fn (mut u Unpacker) unpack_raw() !RawMessage {
	start := u.pos
	u.skip_value()!
	return RawMessage{
		data: u.data[start..u.pos].clone()
	}
}

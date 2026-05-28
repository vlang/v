module cbor

// Marshaler lets a user type control its own CBOR encoding. Returned
// bytes must be exactly one well-formed CBOR data item — the generic
// encoder copies them verbatim, so malformed output corrupts the
// surrounding stream.
//
// Example:
//
//   struct Ipv4 { mut: octets [4]u8 }
//
//   pub fn (ip Ipv4) to_cbor() []u8 {
//       mut p := cbor.new_packer(cbor.EncodeOpts{})
//       p.pack_bytes(ip.octets[..])
//       return p.bytes().clone()
//   }
pub interface Marshaler {
	to_cbor() []u8
}

// Unmarshaler is the reverse: given the bytes of one CBOR data item,
// populate the receiver. The slice is already trimmed to exactly one
// item by the generic decoder.
//
// Implementers use a mut receiver:
//
//   pub fn (mut ip Ipv4) from_cbor(data []u8) ! { ... }
pub interface Unmarshaler {
mut:
	from_cbor(data []u8) !
}

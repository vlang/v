// Package protobuf implements the Protocol Buffers binary wire format in pure
// V, with no C dependency and no `protoc` step at build time.
//
// The module is the runtime a codec is written against, plus the schema
// vocabulary that describes one. A message is encoded by appending fields to a
// Packer and read back by pulling them off an Unpacker:
//
// ```v
// import encoding.protobuf
//
// struct GetRequest {
//     key string @[protobuf: 1]
// }
//
// fn encode_get_request(msg GetRequest) ![]u8 {
//     mut p := protobuf.new_packer(protobuf.EncodeOpts{})
//     // proto3 omits a field holding its default, so an empty key writes
//     // nothing at all
//     if msg.key.len > 0 {
//         p.write_string(1, msg.key)!
//!     }
//     return p.bytes()
//! }
//
// fn decode_get_request(data []u8) !GetRequest {
//     mut u := protobuf.new_unpacker(data, protobuf.DecodeOpts{})
//     mut out := GetRequest{}
//     for !u.eof() {
//         number, wire_type := u.read_tag()!
//         if number == 1 {
//             out.key = u.read_string()!
//!         } else {
//!             // a field this build does not know must be stepped over, so a
//!             // newer producer cannot break an older consumer
//!             u.skip_field(number, wire_type)!
//!         }
//!     }
//!     return out
//! }
// ```
//
// `v pbgen` writes that code from a `.proto` file, which is the usual way to
// use this module. Hand-writing it is the fallback, and it is what the gRPC
// example under `examples/grpc` does.
//
// # What the wire format requires
//
// A field is a tag followed by a value, and the tag is the field number shifted
// up three bits with a wire type in the low three:
//
//	wire 0 -> varint          bool, int32, int64, uint32, uint64, sint*, enum
//	wire 1 -> 64-bit          fixed64, sfixed64, double
//	wire 2 -> length-delimited string, bytes, embedded messages, packed repeats
//	wire 5 -> 32-bit          fixed32, sfixed32, float
//
// Two rules account for most of what looks odd about the encoding:
//
//   * A proto3 `int32` or `int64` has no compact negative form. The spec
//     requires a negative value to be sign-extended to 64 bits, so -1 costs
//     ten bytes. `sint32` and `sint64` exist for that reason and zigzag the
//     value first, which brings -1 back to one byte.
//   * A proto3 field holding its default -- zero, false, an empty string, empty
//     bytes -- is not written, because on the wire an absent field and a field
//     set to its default are the same thing. A message that never carried a
//     field therefore decodes to the default with nothing in the input saying
//     otherwise. `EncodeOpts.emit_defaults` writes them anyway.
//
// # Type mapping
//
//	protobuf  |  V        |  wire
//	----------+-----------+---------------
//	bool      |  bool     |  varint
//	int32     |  i32      |  varint, negative sign-extended to 10 bytes
//	int64     |  i64/int  |  varint, negative sign-extended to 10 bytes
//	uint32    |  u32      |  varint
//	uint64    |  u64      |  varint
//	sint32    |  i32      |  varint, zigzagged
//	sint64    |  i64/int  |  varint, zigzagged
//	fixed32   |  u32      |  4 little-endian bytes
//	sfixed32  |  i32      |  4 little-endian bytes
//	float     |  f32      |  4 little-endian bytes
//	fixed64   |  u64      |  8 little-endian bytes
//	sfixed64  |  i64/int  |  8 little-endian bytes
//	double    |  f64      |  8 little-endian bytes
//	enum      |  enum     |  varint of the enum's integer value
//	string    |  string   |  length-delimited, UTF-8
//	bytes     |  []u8     |  length-delimited
//	message   |  struct   |  length-delimited
//	repeated  |  []T      |  packed for numeric T, one tag per item otherwise
//	map<K, V> |  map[K]V  |  repeated Entry message with key = 1, value = 2
//
// A V type does not always determine the encoding: `i32` is `int32`, `sint32`, or
// `sfixed32` depending on the schema, and only the schema knows which.
// `ProtoScalar` names the thirteen, and `scalar_by_name` resolves a name as the
// schema spells it. A hand-written codec picks its `write_*` call to match; the
// generator picks the same call from the field's declared type.
//
// `i8`, `i16`, `u8`, and `u16` have no protobuf counterpart. Widen them to a
// 32- or 64-bit type, which costs nothing on the wire.
//
// # Not implemented
//
// Groups, the deprecated proto2 construct the spec kept only for
// compatibility. A tag can name one, so the wire types are listed, but a group's
// extent cannot be found without reading fields the reader does not understand,
// so this module refuses one rather than guessing. proto3 forbids them anyway.
module protobuf

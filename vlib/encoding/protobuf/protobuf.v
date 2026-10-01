// Package protobuf implements the Protocol Buffers binary wire format in pure
// V, with no C dependency and no `protoc` step at build time.
//
// Three layers of API are available:
//
//   * `encode[T]` / `decode[T]` — the comptime-driven API. A V struct describes
//     its own schema through `@[protobuf: n]` attributes on its fields, and the
//     field's Go type picks the wire encoding. This is what most callers want.
//
//   * `Packer` / `Unpacker` — the manual API, for a schema that is not known at
//     compile time, for writing a decoder that must survive fields it does not
//     recognise, or for a message whose shape is assembled on the fly.
//
//   * the primitives in this module — varints, zigzags, fixed widths, and field
//     tags — for building the other two.
//
// # Declaring a message
//
// A message is a struct whose every field carries its protobuf field number:
//
// ```v
// struct SearchRequest {
// 	query string @[protobuf: 1]
// 	page  i32    @[protobuf: 2]
// }
//
// req := SearchRequest{ query: 'vlang', page: 1 }
// wire := protobuf.encode[SearchRequest](req, protobuf.EncodeOpts{})!
// back := protobuf.decode[SearchRequest](wire, protobuf.DecodeOpts{})!
// assert back == req
// ```
//
// The attribute is required, not optional. Falling back to declaration order
// would mean renumbering a struct silently renumbers its wire format, which
// breaks interoperability with no compile error to catch it.
//
// # Type mapping
//
//	protobuf  |  V        |  wire
//	----------+-----------+---------------
//	bool      |  bool     |  varint
//	int32     |  i32      |  varint, negative sign-extended to 10 bytes
//	int64     |  i64      |  varint, negative sign-extended to 10 bytes
//	uint32    |  u32      |  varint
//	uint64    |  u64      |  varint
//	sint32    |  i32      |  varint, zigzagged
//	sint64    |  i64      |  varint, zigzagged
//	fixed32   |  u32      |  4 little-endian bytes
//	sfixed32  |  i32      |  4 little-endian bytes
//	float     |  f32      |  4 little-endian bytes
//	fixed64   |  u64      |  8 little-endian bytes
//	sfixed64  |  i64      |  8 little-endian bytes
//	double    |  f64      |  8 little-endian bytes
//	enum      |  enum     |  varint of the enum's integer value
//	string    |  string   |  length-delimited, UTF-8
//	bytes     |  []u8     |  length-delimited
//	message   |  struct   |  length-delimited
//	repeated  |  []T      |  packed for numeric T, one tag per item otherwise
//	map<K, V> |  map[K]V  |  repeated Entry message with key = 1, value = 2
//	optional  |  ?T       |  written when not none, so presence survives
//	oneof     |  ?T fields|  grouped by `@[protobuf_oneof: 'name']`
//
// `i8`, `i16`, `u8`, and `u16` have no protobuf counterpart and are rejected at
// compile time. Widen them to `i32` or `u32` first, which is a one-line change
// and costs nothing on the wire.
//
// # proto3 presence
//
// A singular field holding its default value — zero, false, an empty string,
// empty bytes — is not written, because on the wire an absent field and a field
// set to its default are the same thing. That means a round trip is not always
// the identity: decoding `SearchRequest{}` yields the same value you encoded,
// but decoding a message that never carried `page` also yields `0`, and nothing
// in the input said otherwise. Use a `?T` field when the difference matters.
// `EncodeOpts.emit_defaults` writes defaults anyway, for the proto2 style.
module protobuf

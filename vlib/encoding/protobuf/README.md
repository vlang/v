## Description

`encoding.protobuf` is the Protocol Buffers binary wire format, in pure V. It has
no C dependency and needs no `protoc` at build time.

It is two things:

- a **runtime** — a `Packer` you append fields to, an `Unpacker` you pull them
  off, and the varint and fixed-width primitives they are built from
- a **schema vocabulary** — `ProtoScalar` and `scalar_by_name`, which is what lets
  one description of a message say which of the several protobuf types a V `i32`
  should be treated as

## Generating a codec

The usual way in is `v pbgen`, which reads a `.proto` file and writes the struct,
its `encode` and `decode`, and the gRPC service declarations:

```sh
v pbgen -m kv -o kv/codec.v kv.proto
v pbgen -m kv -o kv/codec.v -grpc kv/service.v kv.proto
```

Only proto3 is supported. See `v help pbgen` for the options and
`cmd/tools/vpbgen/README.md` for the tool.

## What the generator emits

For a message, one struct, an `encode`, an `encode_with`, and a `decode_*`:

```v ignore
pub struct GetRequest {
pub mut:
	// key is `string key = 1`.
	key string
}

// encode serializes `msg` to the proto3 wire format. ...
pub fn (msg GetRequest) encode() ![]u8 {
	return msg.encode_with(protobuf.EncodeOpts{})
}

// encode_with serializes `msg` with `opts`. ...
pub fn (msg GetRequest) encode_with(opts protobuf.EncodeOpts) ![]u8 {
	mut packer := protobuf.new_packer(opts)
	if opts.emit_defaults || msg.key.len > 0 {
		packer.write_string(1, msg.key)!
	}
	return packer.bytes()
}

// decode_get_request parses the proto3 message `kv.GetRequest`. ...
pub fn decode_get_request(data []u8) !GetRequest {
	mut out := GetRequest{}
	mut unpacker := protobuf.new_unpacker(data, protobuf.DecodeOpts{})
	for !unpacker.eof() {
		number, wire_type := unpacker.read_tag()!
		match number {
			1 {
				protobuf.check_wire_type(1, wire_type, protobuf.WireType.length_delimited)!
				out.key = unpacker.read_string()!
			}
			else {
				// an unknown field, or one this build does not know
				unpacker.skip_field(number, wire_type)!
			}
		}
	}
	return out
}
```

Each field's number appears literally in every call its codec makes — the `1` in
`packer.write_string(1, msg.key)` — and in the doc comment above the field. It is
not inferred from the order of the fields, because a schema is free to number them
out of order and to leave gaps.

The struct carries no attribute for the number. An earlier version emitted
`@[protobuf: n]` and nothing read it; `encoding.cbor` reads its own attributes at
run time, so the shape looked right while being inert. An attribute that looks
load-bearing and is not is worse for a reader of the generated file than none.

Three naming rules are worth knowing, because they are what makes the generated
names predictable:

- A message keeps its proto name. `GetRequest` in package `kv` becomes
  `GetRequest`, not `KvGetRequest`. Only a name two declarations both want is
  qualified, as `OneItem` and `TwoItem`.
- A nested message flattens its chain, since V has no nested types: `Outer.Inner`
  becomes `OuterInner`.
- A one-letter capital name is refused. V reserves those for generic template
  types.

### Presence

Three shapes of presence show up in the generated struct:

| Schema | V field | Written |
| --- | --- | --- |
| `string key = 1;` | `key string` | when it is not the default |
| `optional bool flag = 1;` | `flag ?bool` | whenever it is set |
| `oneof { int32 a = 1; }` | `a ?i32` | whenever it is set |

The first two rows are why an absent field and a field set to its default are
different things in the second case and not the first. On the wire they are the
same, so proto3 cannot tell them apart unless the schema asks for presence.

Reading a member of a `oneof` clears the others, because the spec says the last
member on the wire wins.

## The wire format

A field is a tag followed by a value, and the tag is the field number shifted up
three bits with a wire type in the low three:

| Wire | Meaning | Carries |
| --- | --- | --- |
| 0 | varint | bool, int32, int64, uint32, uint64, sint\*, enum |
| 1 | 64-bit | fixed64, sfixed64, double |
| 2 | length-delimited | string, bytes, embedded messages, packed repeats |
| 5 | 32-bit | fixed32, sfixed32, float |

Two rules account for most of what looks odd about the encoding:

- **A negative `int32` or `int64` costs ten bytes.** The spec requires sign
  extension to 64 bits, and there is no compact negative varint form.
  `sint32` and `sint64` exist for this: they zigzag the value first, so -1 is one
  byte again.
- **A field holding its default is not written**, because on the wire an absent
  field and a field set to its default are the same thing. A message that never
  carried a field decodes to the default with nothing in the input saying so.

## Type mapping

| protobuf | V | Wire |
| --- | --- | --- |
| bool | bool | varint |
| int32 | i32 | varint, negative sign-extended to 10 bytes |
| int64 | i64 / int | varint, negative sign-extended to 10 bytes |
| uint32 | u32 | varint |
| uint64 | u64 | varint |
| sint32 | i32 | varint, zigzagged |
| sint64 | i64 / int | varint, zigzagged |
| fixed32 | u32 | 4 little-endian bytes |
| sfixed32 | i32 | 4 little-endian bytes |
| float | f32 | 4 little-endian bytes |
| fixed64 | u64 | 8 little-endian bytes |
| sfixed64 | i64 / int | 8 little-endian bytes |
| double | f64 | 8 little-endian bytes |
| enum | enum | varint of the enum's integer value |
| string | string | length-delimited, UTF-8 |
| bytes | []u8 | length-delimited |
| message | struct | length-delimited |
| repeated | []T | packed for numeric T, one tag per item otherwise |
| map<K, V> | map[K]V | repeated Entry message with key = 1, value = 2 |

A V type does not always determine the encoding: `i32` is `int32`, `sint32`, or
`sfixed32` depending on the schema, and only the schema knows which. That is what
`ProtoScalar` is for.

`i8`, `i16`, `u8`, and `u16` have no protobuf counterpart. Widen them to a 32- or
64-bit type, which costs nothing on the wire.

## Options

```v ignore
@[params]
pub struct EncodeOpts {
pub:
	// initial_cap is the capacity the buffer starts with.
	initial_cap int = 256
	// emit_defaults writes a field that holds its default value instead of
	// leaving it off the wire.
	emit_defaults bool
	// validate_utf8 rejects a `string` field whose bytes are not valid UTF-8.
	validate_utf8 bool
}
```

`emit_defaults` is the one to reach for when the peer expects explicit presence
where proto3 would not keep it. The options also reach nested messages and map
entries, so a field is held to the same rules however deep it sits.

`validate_utf8` is off by default because it costs a pass over the bytes, and a
`bytes` field is exempt either way: a `string` is defined to be UTF-8, so invalid
bytes are a schema error worth rejecting, while a `bytes` field is opaque.

## Writing a codec by hand

The manual API is `Packer` and `Unpacker` directly. The rule about defaults is the
caller's, deliberately: a writer that silently dropped values would make the
manual API impossible to use for a schema that keeps presence.

```v ignore
fn encode_get_request(msg GetRequest) ![]u8 {
	mut packer := protobuf.new_packer(protobuf.EncodeOpts{})
	if msg.key.len > 0 {
		packer.write_string(1, msg.key)!
	}
	return packer.bytes()
}
```

## Not implemented

- **Groups**, the deprecated proto2 construct the spec kept for compatibility. A
  tag can name one, so the wire types are listed, but a group's extent cannot be
  found without reading fields the reader does not understand, so this module
  refuses one rather than guessing. proto3 forbids them.
- **proto2**, and with it `required` and proto2 default values.
- **The reflection API.** An earlier version of this module had `encode[T]` and
  `decode[T]`, driven by `@[...]` attributes read off the message type at run
  time. V3 cannot express the generic decode a schema needs: an explicit type
  argument on a call whose type parameter comes from a field rather than from a
  generic reaches `__v_comptime_unsupported_late_generic_call`. The generator
  writes explicit calls instead, and the attribute readers went with it.

## Tests

`v test vlib/encoding/protobuf/`

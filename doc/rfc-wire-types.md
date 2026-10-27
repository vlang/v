# RFC: Compile-Time-Verified Wire Types for Binary Protocols

Status: RFC, docs only. No `@[wire]` in tree. Related work:
`vlib/encoding/binary` provides explicit little/big-endian
`put`/`at` helpers, but no declarative struct-level layout
contract. This RFC proposes that contract on top of those
conventions, not another generic serializer.

## Syntax (illustrative)

```v okfmt
@[wire: 'little_endian']
struct PacketHeader {
	version     u8
	kind        u8
	payload_len u16
	request_id  u32
}
```

Field order defines encoded order; multi-byte fields use the
declared byte order, never host endianness.

```v okfmt
header_bytes := header.encode_wire()
header := PacketHeader.decode_wire(header_bytes)!
```

Exact API follows V's encoding conventions.

## Semantics

1. Stable encoding: deterministic across architectures and
   backends; native alignment/padding never leaks into the wire.
2. Explicit byte order per type; host endianness is never an
   implicit protocol rule.
3. Compile-time validation: fixed-width ints, enums with explicit
   wire representation, fixed-size arrays, and nested wire types
   first; anything else is a compile error.
4. Safe decoding: length validated before any field read;
   truncated/invalid input returns `!T`, never an unchecked
   pointer cast over received bytes.
5. Explicit extensions only: reserved fields, discriminants, bit
   widths, versioned layouts each specified individually, never
   inferred from packing rules.

## Non-goals

No memory reinterpretation, no platform-dependent layouts, no
full type coverage in v1, no packed-struct assumption, no changes
to existing `encoding` packages.

## Acceptance

Byte order declared; identical bytes cross-platform for equal
values; layout independent of padding; unsupported fields are
compile errors; decode checks length; nesting documented;
tests cover known vectors, truncation, nesting, and
cross-platform behavior.
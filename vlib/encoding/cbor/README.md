## Description

`encoding.cbor` is an RFC 8949 Concise Binary Object Representation codec.

CBOR is a compact, schema-free binary format that supports the same value
model as JSON (numbers, strings, arrays, maps) plus byte strings, tagged
items, IEEE 754 floats at three widths, and a small set of "simple"
values (`true`, `false`, `null`, `undefined`). It is used by COSE/CWT
(IETF security stack), WebAuthn/FIDO2, the Matter smart-home protocol,
and many IoT stacks because messages are typically 30–60 % smaller than
JSON and parse without quoting/escaping.

Three layers of API are available:

* `encode[T]` / `decode[T]` — comptime-driven generic API. Works on
  primitives, strings, arrays, maps, structs (with `@[cbor: 'name']`,
  `@[skip]`, `@[cbor_rename_all: 'snake_case']`), enums, `time.Time`
  (auto-tagged), and any type implementing `Marshaler` / `Unmarshaler`.
* `Packer` / `Unpacker` — manual streaming API. Use when the schema
  isn't known at compile time, or when you need full control over tags,
  indefinite-length items and simple values.
* `Value` sumtype — dynamic representation for round-tripping unknown
  payloads or inspecting tagged data.

Defaults follow RFC 8949 *preferred serialisation* (§4.2.2): floats
shrink to the shortest IEEE 754 width that preserves their value, and
every length argument uses the shortest encoding. Set
`EncodeOpts.canonical = true` to additionally sort map keys for
hash/signature stability (§4.2.1, deterministic encoding). Set
`EncodeOpts.validate_utf8 = true` if callers may build strings from raw
bytes (e.g. `bytestr()`) — the streaming `pack_text` trusts its input
for performance, but `encode[T]` will then refuse to emit non-UTF-8
text strings the strict-by-default decoder would reject on the way back.

## Usage

### encode[T] / decode[T]

```v
import encoding.cbor
import time

struct Person {
	name     string
	age      int
	email    ?string
	birthday time.Time
}

fn main() {
	bob := Person{
		name:     'Bob'
		age:      30
		birthday: time.now()
	}

	bytes := cbor.encode[Person](bob, cbor.EncodeOpts{})!
	// bytes is []u8 — wire-ready CBOR

	back := cbor.decode[Person](bytes, cbor.DecodeOpts{})!
	assert back.name == 'Bob'
}
```

Optional fields (`?T`) encode as CBOR `null` when set to `none`. Enums
encode as their underlying integer.

### Struct attributes

```v oksyntax
@[cbor_rename_all: 'kebab-case']
struct Login {
	user_name string @[cbor: 'u'] // emit/read key "u" (overrides rename_all)
	password  string @[skip]      // never serialise
	remember  bool // becomes "remember"
	is_admin  bool // becomes "is-admin"
}
```

The `@[cbor_rename_all: '...']` attribute on a struct applies a global
rename strategy to every field that doesn't have an explicit
`@[cbor: '...']` override — supported strategies: `snake_case`,
`camelCase`, `PascalCase`, `kebab-case`, `SCREAMING_SNAKE_CASE`. Use
`@[cbor: '-']` as an alternative to `@[skip]`.

### Manual streaming with Packer / Unpacker

Use this when the schema is dynamic or when you need access to CBOR
features that don't map directly to V types (tags, indefinite-length
strings, custom simple values):

```v
import encoding.cbor

fn main() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_header(3)
	p.pack_uint(42)
	p.pack_text('hello')
	p.pack_bool(true)
	bytes := p.bytes()

	mut u := cbor.new_unpacker(bytes, cbor.DecodeOpts{})
	n := u.unpack_array_header()! // 3
	a := u.unpack_uint()! // 42
	b := u.unpack_text()! // 'hello'
	c := u.unpack_bool()! // true
	_ = n
	_ = a
	_ = b
	_ = c
}
```

### Dynamic values with `Value`

When the payload schema is unknown at compile time, decode into
`cbor.Value` and walk the sumtype:

```v
import encoding.cbor

fn main() {
	bytes := cbor.encode[map[string]int]({
		'a': 1
		'b': 2
	}, cbor.EncodeOpts{})!

	v := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{})!
	if val := v.get('a') {
		if i := val.as_int() {
			assert i == 1
		}
	}
}
```

`Value` covers every CBOR type: `IntNum`, `FloatNum`, `Text`, `Bytes`,
`Array`, `Map`, `Tag`, `Bool`, `Null`, `Undefined`, `Simple`. Re-encoding
a `Value` round-trips bit-for-bit when the source was already in
preferred form.

### Custom Marshaler / Unmarshaler

For types that need a custom on-wire representation, implement either
side of the interface:

```v oksyntax
import encoding.cbor

struct Color {
mut:
	r u8
	g u8
	b u8
}

pub fn (c Color) to_cbor() []u8 {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_header(3)
	p.pack_uint(c.r)
	p.pack_uint(c.g)
	p.pack_uint(c.b)
	return p.bytes().clone()
}

pub fn (mut c Color) from_cbor(data []u8) ! {
	mut u := cbor.new_unpacker(data, cbor.DecodeOpts{})
	n := u.unpack_array_header()!
	if n != 3 {
		return error('Color expects 3 elements')
	}
	c.r = u8(u.unpack_uint()!)
	c.g = u8(u.unpack_uint()!)
	c.b = u8(u.unpack_uint()!)
}
```

`to_cbor` must return exactly one well-formed CBOR data item — the
generic encoder copies the bytes verbatim. `from_cbor` receives a slice
already trimmed to one item.

### Canonical (deterministic) encoding

For hashing or signing, set `canonical: true` so that map keys are
sorted by length-then-lexicographic order (RFC 8949 §4.2.1):

```v oksyntax
import encoding.cbor

bytes := cbor.encode[map[string]int]({
	'b': 2
	'a': 1
}, cbor.EncodeOpts{ canonical: true })!
// keys are emitted in the order "a", "b" regardless of input order
```

### Tags and `time.Time`

Values of type `time.Time` round-trip losslessly: whole-second values
use tag 1 (epoch seconds, integer) for the smallest canonical wire,
and sub-second values use tag 0 (RFC 3339 string with nanosecond
precision) — necessary because a tag-1 float can't carry both a
10-digit unix epoch and 9 fractional digits. Decode accepts tag 0
(RFC 3339 text, any sub-second precision) or tag 1 (integer or float).
Custom tags can be emitted/read via `pack_tag` / `unpack_tag` or by
constructing a `Value` with `cbor.new_tag(number, content)`.

## Conformance

The test suite (`vlib/encoding/cbor/tests/`) covers every vector from
RFC 8949 Appendix A, plus indefinite-length strings, depth limits,
malformed-input rejection, UTF-8 validation, canonical ordering, and
tagged time round-trips.

```bash
v test vlib/encoding/cbor/tests/
```
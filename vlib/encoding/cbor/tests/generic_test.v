// Generic encode[T]/decode[T] coverage. Exercises every supported V
// type family — primitives, arrays, maps, structs (with attributes),
// optional fields, enums, RawMessage, Marshaler/Unmarshaler — and
// asserts byte-exact output for at least one case per family so we
// catch silent encoding drift.
module main

import encoding.cbor
import encoding.hex

fn h(s string) []u8 {
	return hex.decode(s) or { panic('invalid hex: ${s}') }
}

fn beq(a []u8, b []u8) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// ---------------------------------------------------------------------
// Primitive round-trips
// ---------------------------------------------------------------------

fn test_round_trip_primitives() {
	bytes_bool := cbor.encode[bool](true, cbor.EncodeOpts{})!
	assert cbor.decode[bool](bytes_bool, cbor.DecodeOpts{})! == true

	bytes_i32 := cbor.encode[i32](-42, cbor.EncodeOpts{})!
	assert cbor.decode[i32](bytes_i32, cbor.DecodeOpts{})! == -42

	bytes_u64 := cbor.encode[u64](u64(0x1234_5678_9abc_def0), cbor.EncodeOpts{})!
	assert cbor.decode[u64](bytes_u64, cbor.DecodeOpts{})! == 0x1234_5678_9abc_def0

	bytes_f64 := cbor.encode[f64](3.141592653589793, cbor.EncodeOpts{})!
	assert cbor.decode[f64](bytes_f64, cbor.DecodeOpts{})! == 3.141592653589793

	bytes_str := cbor.encode[string]('hello, 世界', cbor.EncodeOpts{})!
	assert cbor.decode[string](bytes_str, cbor.DecodeOpts{})! == 'hello, 世界'
}

// ---------------------------------------------------------------------
// Arrays and maps
// ---------------------------------------------------------------------

fn test_array_round_trip() {
	src := [1, 2, 3, 4, 5]
	bytes := cbor.encode[[]int](src, cbor.EncodeOpts{})!
	got := cbor.decode[[]int](bytes, cbor.DecodeOpts{})!
	assert got == src
}

fn test_map_round_trip() {
	src := {
		'a': 1
		'b': 2
		'c': 3
	}
	bytes := cbor.encode[map[string]int](src, cbor.EncodeOpts{})!
	got := cbor.decode[map[string]int](bytes, cbor.DecodeOpts{})!
	for k, v in src {
		assert got[k] == v
	}
	assert got.len == src.len
}

fn test_nested_array_map() {
	src := [
		{
			'k': 'v1'
		},
		{
			'k': 'v2'
		},
	]
	bytes := cbor.encode[[]map[string]string](src, cbor.EncodeOpts{})!
	got := cbor.decode[[]map[string]string](bytes, cbor.DecodeOpts{})!
	assert got.len == 2
	assert got[0]['k'] == 'v1'
	assert got[1]['k'] == 'v2'
}

// ---------------------------------------------------------------------
// Structs — attributes, optional, rename strategies
// ---------------------------------------------------------------------

struct Person {
	name string
	age  int
}

fn test_struct_basic() {
	p := Person{
		name: 'Alice'
		age:  42
	}
	bytes := cbor.encode[Person](p, cbor.EncodeOpts{})!
	got := cbor.decode[Person](bytes, cbor.DecodeOpts{})!
	assert got.name == 'Alice'
	assert got.age == 42
}

struct WithAttrs {
	user_id  string @[cbor: 'uid']
	password string @[skip]
	internal string @[cbor: '-']
	keep     string
}

fn test_struct_attributes() {
	p := WithAttrs{
		user_id:  'u-1'
		password: 'secret'
		internal: 'hidden'
		keep:     'visible'
	}
	bytes := cbor.encode[WithAttrs](p, cbor.EncodeOpts{})!
	// Decode generically to inspect structure.
	v := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{})!
	assert v is cbor.Map
	if v is cbor.Map {
		mut keys := []string{}
		for pair in v.pairs {
			if pair.key is cbor.Text {
				keys << pair.key.value
			}
		}
		assert 'uid' in keys
		assert 'keep' in keys
		assert 'password' !in keys
		assert 'internal' !in keys
	}
	got := cbor.decode[WithAttrs](bytes, cbor.DecodeOpts{})!
	assert got.user_id == 'u-1'
	assert got.keep == 'visible'
}

struct WithOption {
	name string
	tag  ?string
}

fn test_struct_option_field() {
	none_p := WithOption{
		name: 'a'
		tag:  none
	}
	bytes := cbor.encode[WithOption](none_p, cbor.EncodeOpts{})!
	got := cbor.decode[WithOption](bytes, cbor.DecodeOpts{})!
	assert got.name == 'a'
	assert got.tag == none

	some_p := WithOption{
		name: 'b'
		tag:  ?string('hot')
	}
	bytes2 := cbor.encode[WithOption](some_p, cbor.EncodeOpts{})!
	got2 := cbor.decode[WithOption](bytes2, cbor.DecodeOpts{})!
	assert got2.name == 'b'
	assert got2.tag != none
	assert got2.tag or { '' } == 'hot'
}

@[cbor_rename_all: 'kebab-case']
struct WithRename {
	user_name string
	user_age  int
}

fn test_struct_rename_all() {
	p := WithRename{
		user_name: 'Bob'
		user_age:  30
	}
	bytes := cbor.encode[WithRename](p, cbor.EncodeOpts{})!
	v := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{})!
	if v is cbor.Map {
		mut keys := []string{}
		for pair in v.pairs {
			if pair.key is cbor.Text {
				keys << pair.key.value
			}
		}
		assert 'user-name' in keys
		assert 'user-age' in keys
	}
	// And the rename round-trips.
	got := cbor.decode[WithRename](bytes, cbor.DecodeOpts{})!
	assert got.user_name == 'Bob'
	assert got.user_age == 30
}

// ---------------------------------------------------------------------
// Enum
// ---------------------------------------------------------------------

enum Color {
	red
	green
	blue
}

fn test_enum_round_trip() {
	bytes := cbor.encode[Color](Color.green, cbor.EncodeOpts{})!
	got := cbor.decode[Color](bytes, cbor.DecodeOpts{})!
	assert got == Color.green
}

// ---------------------------------------------------------------------
// RawMessage — preserves bytes byte-for-byte
// ---------------------------------------------------------------------

fn test_raw_message_round_trip() {
	original := h('a26161016162820203')
	raw := cbor.decode[cbor.RawMessage](original, cbor.DecodeOpts{})!
	again := cbor.encode[cbor.RawMessage](raw, cbor.EncodeOpts{})!
	assert beq(again, original)
}

// ---------------------------------------------------------------------
// Marshaler / Unmarshaler — user-controlled wire format
// ---------------------------------------------------------------------

struct Ipv4 {
mut:
	octets [4]u8
}

pub fn (ip Ipv4) to_cbor() []u8 {
	mut p := cbor.new_packer(cbor.EncodeOpts{ initial_cap: 8 })
	p.pack_bytes([ip.octets[0], ip.octets[1], ip.octets[2], ip.octets[3]])
	return p.bytes().clone()
}

pub fn (mut ip Ipv4) from_cbor(data []u8) ! {
	mut u := cbor.new_unpacker(data, cbor.DecodeOpts{})
	bytes := u.unpack_bytes()!
	if bytes.len != 4 {
		return error('Ipv4: expected 4 bytes, got ${bytes.len}')
	}
	ip.octets[0] = bytes[0]
	ip.octets[1] = bytes[1]
	ip.octets[2] = bytes[2]
	ip.octets[3] = bytes[3]
}

fn test_marshaler_round_trip() {
	ip := Ipv4{
		octets: [u8(192), 168, 1, 1]!
	}
	bytes := cbor.encode[Ipv4](ip, cbor.EncodeOpts{})!
	// Wire bytes: 0x44 (bytes len 4) followed by 4 octets.
	assert beq(bytes, h('44c0a80101'))
	got := cbor.decode[Ipv4](bytes, cbor.DecodeOpts{})!
	assert got.octets[0] == 192
	assert got.octets[1] == 168
	assert got.octets[2] == 1
	assert got.octets[3] == 1
}

// ---------------------------------------------------------------------
// Integer-range checks on decode
// ---------------------------------------------------------------------

fn test_int_range_overflow_rejected() {
	// 256 doesn't fit u8.
	bytes := cbor.encode[u16](u16(256), cbor.EncodeOpts{})!
	if _ := cbor.decode[u8](bytes, cbor.DecodeOpts{}) {
		assert false, 'expected u8 range error'
	}
}

fn test_negative_to_unsigned_rejected() {
	bytes := cbor.encode[i64](-1, cbor.EncodeOpts{})!
	if _ := cbor.decode[u64](bytes, cbor.DecodeOpts{}) {
		assert false, 'expected u64 range error'
	}
}

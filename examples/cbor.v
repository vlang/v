module main

import encoding.cbor
import encoding.hex
import time

struct Address {
	street string
	city   string
	zip    string @[cbor: 'postal_code']
}

struct User {
	name      string
	age       u32
	email     ?string
	tags      []string
	address   Address
	signed_up time.Time
	internal  string @[skip]
}

fn main() {
	user := User{
		name:      'Alice'
		age:       30
		email:     'alice@example.com'
		tags:      ['admin', 'beta']
		address:   Address{
			street: '1 Test Lane'
			city:   'Paris'
			zip:    '75000'
		}
		signed_up: time.parse_iso8601('2025-01-15T10:00:00Z') or { time.now() }
		internal:  'will not be encoded'
	}

	// 1. Generic typed encode/decode
	bytes := cbor.encode[User](user, cbor.EncodeOpts{})!
	println('encoded ${bytes.len} bytes: ${hex.encode(bytes)}')

	back := cbor.decode[User](bytes, cbor.DecodeOpts{})!
	println('round-trip name=${back.name} age=${back.age} city=${back.address.city}')

	// 2. Canonical encoding for stable hashing/signing
	mut m := map[string]int{}
	m['z'] = 26
	m['a'] = 1
	m['m'] = 13
	canonical := cbor.encode[map[string]int](m, cbor.EncodeOpts{
		canonical: true
	})!
	println('canonical map: ${hex.encode(canonical)}')

	// 3. Decode an unknown payload into a Value tree
	v := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{})!
	if name_val := v.get('name') {
		if s := name_val.as_string() {
			println('peeked name from Value tree: ${s}')
		}
	}

	// 4. Manual streaming: build a CBOR array of mixed types
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_header(3)
	p.pack_uint(42)
	p.pack_text('hello')
	p.pack_bool(true)
	stream_bytes := p.bytes()
	println('manual stream: ${hex.encode(stream_bytes)}')

	mut up := cbor.new_unpacker(stream_bytes, cbor.DecodeOpts{})
	n := up.unpack_array_header()!
	first := up.unpack_uint()!
	second := up.unpack_text()!
	third := up.unpack_bool()!
	println('unpacked array of ${n}: ${first}, "${second}", ${third}')
}

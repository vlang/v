// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// Tests case for PrintableString
fn test_encode_printablestring_basic() ! {
	s := 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

	ps := PrintableString.new(s)!
	buf := encode(ps)!

	mut out := [u8(0x13)]
	length := [u8(0x81), u8(s.len)]
	out << length
	out << s.bytes()
	// dump(out)
	assert out == buf

	psback, _ := PrintableString.decode(buf)!
	assert psback.tag().tag_number() == int(TagType.printablestring)
	assert psback.tag().tag_class() == .universal

	assert psback.value == s
}

struct EncodingTest[T] {
	input T
	exp   []u8
}

fn test_encode_printablestring_generic() {
	data := [
		// from dart asn1lib test
		EncodingTest[string]{'TheTestString', [u8(0x13), 13, 84, 104, 101, 84, 101, 115, 116, 83,
			116, 114, 105, 110, 103]},
		EncodingTest[string]{'Test User 1', [u8(0x13), 0x0b, 84, 101, 115, 116, 32, 85, 115, 101,
			114, 32, 49]},
	]

	for t in data {
		ps := PrintableString.new(string(t.input))!
		out := encode(ps)!
		// out := serialize_printablestring(string(t.input))!
		assert out == t.exp

		// decode back
		psback, _ := PrintableString.decode(out)!

		assert psback.value == t.input
		assert psback.tag().tag_number() == int(TagType.printablestring)
		assert psback.tag().is_constructed() == false
	}
}

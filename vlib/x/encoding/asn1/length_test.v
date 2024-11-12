// Copyright (c) 2022, 2023 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

struct TagAndLengthTest {
	bytes     []u8
	tag       Tag
	explength i64
	lastpos   int
	err       IError
}

fn test_tagandlength_handling() ! {
	// from golang asn.1 test
	bs := [
		// Context Specific should be in constructed form
		TagAndLengthTest{[u8(0x80), 0x01], Tag{.context_specific, false, 0}, 1, 2, none}, // 0b_10_0_00000
		TagAndLengthTest{[u8(0xa0), 0x01], Tag{.context_specific, true, 0}, 1, 2, none}, // 0b_10_1_00000
		TagAndLengthTest{[u8(0x02), 0x00], Tag{.universal, false, 2}, 0, 2, none},
		TagAndLengthTest{[u8(0xfe), 0x00], Tag{.private, true, 30}, 0, 2, none},
		TagAndLengthTest{[u8(0x1f), 0x1f, 0x00], Tag{.universal, false, 31}, 0, 3, none}, // high tag form
		TagAndLengthTest{[u8(0x1f), 0x81, 0x00, 0x01], Tag{.universal, false, 128}, 1, 4, none},
		// the last byte tells its length in long form
		TagAndLengthTest{[u8(0x1f), 0x81, 0x00, 0x81], Tag{.universal, false, 128}, 1, 4, error('Length: truncated length')},
		TagAndLengthTest{[u8(0x1f), 0x81, 0x80, 0x01, 0x00], Tag{.universal, false, 16385}, 0, 5, error('Tag number: base 128 integer too large')}, // 1x128^2 + 0x128^1 + 1x128*0
		TagAndLengthTest{[u8(0x00), 0x81, 0x80], Tag{.universal, false, 0}, 128, 3, none},
		// need one byte length
		TagAndLengthTest{[u8(0x00), 0x83, 0x01, 0x00], Tag{.universal, false, 0}, 2, 1, error('Length: truncated length')},
		// normal version above
		TagAndLengthTest{[u8(0x00), 0x83, 0x01, 0x01, 0x01], Tag{.universal, false, 0}, 65793, 5, none}, // length = 1x256^2 + 1x256^1 + 1x256^0
		TagAndLengthTest{[u8(0x1f), 0x85], Tag{.universal, false, 0}, 0, 2, error('Tag: truncated base 128 integer')},
		TagAndLengthTest{[u8(0x1f), 0x85, 0x81], Tag{.universal, false, 0}, 0, 0, error('Tag: truncated base 128 integer')},
		// this last bytes tell the length is in undefinite length, 0x80
		TagAndLengthTest{[u8(0x30), 0x80], Tag{.universal, true, 0x10}, 0, 2, error('Length: unsupported undefinite length')},
		// still truncated length part
		TagAndLengthTest{[u8(0x30), 0x81], Tag{.universal, true, 0x10}, 0, 2, error('Length: truncated length')},
		// still in uneeded form of length
		TagAndLengthTest{[u8(0x30), 0x81, 0x01], Tag{.universal, true, 0x10}, 1, 3, error('Length: dont needed in long form')},
		// its fullfill the der requirement
		TagAndLengthTest{[u8(0x30), 0x81, 0x80], Tag{.universal, true, 0x10}, 128, 3, none},
		// this tell two bytes of length contains leading spurious zero's
		TagAndLengthTest{[u8(0xa0), 0x82, 0x00, 0xff], Tag{.context_specific, true, 0}, 255, 1, error('Length: leading zeros')},
		TagAndLengthTest{[u8(0xa0), 0x82, 0x01, 0xff], Tag{.context_specific, true, 0}, 511, 4, none},
		// Superfluous zeros in the length should be an error.
		TagAndLengthTest{[u8(0xa0), 0x82, 0x00, 0xff], Tag{.context_specific, true, 0}, 0, 4, error('Length: leading zeros')}, //{}},
		// Lengths up to the maximum size of an int should work.
		TagAndLengthTest{[u8(0xa0), 0x84, 0x7f, 0xff, 0xff, 0xff], Tag{.context_specific, true, 0}, 0x7fffffff, 6, none}, //{2, 0, 0x7fffffff, true}},
		// Lengths that would overflow an int should be rejected.
		TagAndLengthTest{[u8(0xa0), 0x84, 0x80, 0x00, 0x00, 0x00], Tag{.context_specific, true, 0}, 2147483648, 6, error('Length: dont needed in long form')}, //{}},
		// Long length form may not be used for lengths that fit in short form.
		TagAndLengthTest{[u8(0xa0), 0x81, 0x7f], Tag{.context_specific, true, 0}, 0, 0, error('Length: dont needed in long form')}, //{}},
		// Tag numbers which would overflow int32 are rejected. (The number below is 2^31.)
		TagAndLengthTest{[u8(0x1f), 0x88, 0x80, 0x80, 0x80, 0x00, 0x00], Tag{.universal, false, 0}, 0, 0, error('Negative tag number')}, //{}},
		// Tag numbers that fit in an int32 are valid. (The number below is 2^31 - 1.) but its bigger than max_tag_bytes_length
		TagAndLengthTest{[u8(0x1f), 0x87, 0xFF, 0xFF, 0xFF, 0x7F, 0x00], Tag{.universal, false, 2147483647}, 0, 7, error('Tag number: base 128 integer too large')},
		// Long tag number form may not be used for tags that fit in short form.
		TagAndLengthTest{[u8(0x1f), 0x1e, 0x00], Tag{.universal, false, 0}, 0, 0, error('Tag: non-minimal tag')}, //{}},
	]

	for i, c in bs {
		// dump(i)
		tag, pos := Tag.decode(c.bytes) or {
			assert err == c.err
			continue
		}
		assert tag == c.tag
		length, idx := Length.decode_from_offset(c.bytes, pos) or {
			assert err == c.err
			continue
		}
		assert idx == c.lastpos
	}
}

struct LengthPackTest {
	value    int
	expected []u8
	err      IError
}

fn test_length_pack_and_unpack_tofrom_asn() ! {
	edata := [
		LengthPackTest{0, [u8(0x00)], none},
		LengthPackTest{10, [u8(0x0a)], none},
		LengthPackTest{127, [u8(0x7f)], none},
		LengthPackTest{255, [u8(0x81), 0xff], none},
		LengthPackTest{256, [u8(0x82), 0x01, 0x00], none},
		LengthPackTest{383, [u8(0x82), 0x01, 127], none},
		LengthPackTest{257, [u8(0x82), 0x01, 0x01], none},
		LengthPackTest{65535, [u8(0x82), 0xff, 0xff], none},
		LengthPackTest{65536, [u8(0x83), 0x01, 0x00, 0x00], none},
		LengthPackTest{16777215, [u8(0x83), 0xff, 0xff, 0xff], none},
	]
	for i, c in edata {
		mut dst := []u8{}
		s := Length.new(c.value)!
		s.encode(mut dst)!
		assert dst == c.expected

		length, idx := Length.decode(dst)!

		assert length == c.value
		assert idx == c.expected.len
	}
}

struct ByteLengthTest {
	value    int
	expected []u8
}

fn test_basic_simple_length_unpack() {
	data := [u8(0x82), 0x01, 0x7F]
	n, pos := Length.decode(data)!

	assert n == 383
	assert pos == 3

	data2 := [u8(0x82), 0x01, 0x31]
	n2, pos2 := Length.decode(data2)!
	assert n2 == 305
	assert pos2 == 3
}

fn test_length_pack_and_append() ! {
	bdata := [
		ByteLengthTest{1, [u8(1)]},
		ByteLengthTest{127, [u8(0x7f)]},
		ByteLengthTest{255, [u8(0xff)]},
		ByteLengthTest{256, [u8(0x01), 0x00]},
		ByteLengthTest{383, [u8(0x01), 127]},
		ByteLengthTest{257, [u8(0x01), 0x01]},
		ByteLengthTest{7967, [u8(0x1f), 0x1f]},
		ByteLengthTest{65535, [u8(0xff), 0xff]},
		ByteLengthTest{65537, [u8(0x01), 0x00, 0x01]},
		ByteLengthTest{16777215, [u8(0xff), 0xff, 0xff]},
	]

	for v in bdata {
		mut dst := []u8{}

		ln := Length.new(v.value)!
		ln.to_bytes(mut dst)

		assert dst == v.expected
	}
}

struct LengthTest {
	value    int
	expected int
}

fn test_length_bytes_len() ! {
	ldata := [
		LengthTest{1, 1},
		LengthTest{128, 1},
		LengthTest{255, 1},
		LengthTest{256, 2},
		LengthTest{383, 2},
		LengthTest{65535, 2},
		LengthTest{65536, 3},
		LengthTest{16777215, 3},
		LengthTest{16777216, 4},
		LengthTest{2147483647, 4}, // math.max_i32
	]

	for c in ldata {
		len := Length.new(c.value)!
		out := len.bytes_len()

		assert out == c.expected
	}
}

fn test_calc_length_of_length() ! {
	data := [
		LengthTest{1, 1},
		LengthTest{128, 2},
		LengthTest{255, 2},
		LengthTest{256, 3},
		LengthTest{383, 3},
		LengthTest{65535, 3},
		LengthTest{65536, 4},
		LengthTest{16777215, 4},
		LengthTest{16777216, 5},
		LengthTest{2147483647, 5}, // math.max_i32
	]

	for c in data {
		len := Length.new(c.value)!
		out := len.length_size()!

		assert out == c.expected
	}
}

// ASN.1 Test Suite from https://github.com/YuryStrozhevsky/asn1-test-suite
fn test_tc3_absence_standard_length_block() ! {
	value := []u8{}

	_, _ := Length.decode(value) or {
		assert err == error('Length: truncated length')
		return
	}
}

fn test_tc5_unnecessary_usage_long_of_length_form() ! {
	value := [u8(0x7f), 0xff, 0x7f, 0x81, 0x01, 0x40]

	tag, pos := Tag.decode(value)!
	// 0x9f == 0b0111_1111
	assert tag.class == .application
	assert tag.constructed == true
	assert pos == 3
	// the length bytes, [0x81, 0x01] dont needed in long form.
	_, _ := Length.decode_with_rule(value, pos, .der) or {
		assert err == error('Length: dont needed in long form')
		return
	}
}

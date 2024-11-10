// Copyright (c) 2022, 2024 blackshirt All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

import math.big

struct TwoFormTest {
	value    string
	expected string
}

const string_data = [
	TwoFormTest{'0', '\x00'},
	TwoFormTest{'25', '\x19'},
	TwoFormTest{'100', '\x64'},
	TwoFormTest{'-1042342234234123423435647768234', '\xF2\xD8\x02\xB6R\x7F\x99\xEE\x98#\x99\xA9V'},
	TwoFormTest{'-12095473475870063', '\xD5\a;\x20\x14\xA2\x91'},
	TwoFormTest{'12095473475870063', '*\xF8\xC4\xDF\xEB]o'},
	TwoFormTest{'12438789579431234124191998', '\nJ\x04"^\x91\x04\x8a\xb1\x18\xfe'},
	TwoFormTest{'-112233441191', '\xe5\xde]\x98Y'},
	TwoFormTest{'64206', '\x00\xfa\xce'},
	TwoFormTest{'-100', '\x9C'},
	TwoFormTest{'100', '\x64'},
	TwoFormTest{'255', '\x00\xFF'},
	TwoFormTest{'0', '\x00'},
	TwoFormTest{'-2', '\xfe'},
	TwoFormTest{'-1', '\xff'},
	TwoFormTest{'-16', '\xf0'},
	TwoFormTest{'-256', '\xff\x00'},
	TwoFormTest{'-255', '\xff\x01'},
	TwoFormTest{'-32768', '\x80\x00'},
	TwoFormTest{'-128', '\x80'},
	TwoFormTest{'-129', '\xff\x7f'},
	TwoFormTest{'-127', '\x81'},
]

fn test_integer_pack_n_unpack_from_n_into_2form() ! {
	for i, c in string_data {
		v := Integer.from_string(c.value)!
		out, _ := v.pack_into_twoscomplement_form()!

		assert out == c.expected.bytes()

		// unpack back
		b := Integer.unpack_from_twoscomplement_bytes(c.expected.bytes())!
		assert b.value.str() == c.value
	}
}

struct UnpackTest {
	val i64
	out []u8
}

// from python asn1tools
//
const unpack_data = [
	UnpackTest{32768, [u8(0x02), 0x03, 0x00, 0x80, 0x00]},
	UnpackTest{32767, [u8(0x02), 0x02, 0x7f, 0xff]},
	UnpackTest{256, [u8(0x02), 0x02, 0x01, 0x00]},
	UnpackTest{255, [u8(0x02), 0x02, 0x00, 0xff]},
	UnpackTest{128, [u8(0x02), 0x02, 0x00, 0x80]},
	UnpackTest{127, [u8(0x02), 0x01, 0x7f]},
	UnpackTest{1, [u8(0x02), 0x01, 0x01]},
	UnpackTest{0, [u8(0x02), 0x01, 0x00]},
	UnpackTest{-1, [u8(0x02), 0x01, 0xff]},
	UnpackTest{-128, [u8(0x02), 0x01, 0x80]},
	UnpackTest{-129, [u8(0x02), 0x02, 0xff, 0x7f]},
	UnpackTest{-256, [u8(0x02), 0x02, 0xff, 0x00]},
	UnpackTest{-32768, [u8(0x02), 0x02, 0x80, 0x00]},
	UnpackTest{-32769, [u8(0x02), 0x03, 0xff, 0x7f, 0xff]},
]

fn test_asn1_integer_unencode() ! {
	for i, c in unpack_data {
		n := Integer.from_i64(c.val)
		to := encode(n)!
		assert to == c.out
	}
}

struct ASNIntegerTest {
	bytes    []u8
	err      IError
	expected Integer
}

const integer_test_data = [
	ASNIntegerTest{[u8(0x00)], none, Integer.from_int(0)},
	ASNIntegerTest{[u8(0x7f)], none, Integer.from_int(127)},
	ASNIntegerTest{[u8(0x00), 0x80], none, Integer.from_int(128)},
	ASNIntegerTest{[u8(0x01), 0x00], none, Integer.from_int(256)},
	ASNIntegerTest{[u8(0x80)], none, Integer.from_int(-128)},
	ASNIntegerTest{[u8(0xff), 0x7f], none, Integer.from_int(-129)},
	ASNIntegerTest{[u8(0xff)], none, Integer.from_int(-1)},
	ASNIntegerTest{[u8(0x80), 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], none, Integer{
		value: big.integer_from_string('-9223372036854775808') or { panic(err) }
	}},
	ASNIntegerTest{[u8(0x80), 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], none, Integer{
		value: big.integer_from_string('-2361183241434822606848') or { panic(err) }
	}},
	ASNIntegerTest{[], error('Integer: check return false'), Integer.from_i64(0)},
	ASNIntegerTest{[u8(0x00), 0x7f], error('Integer: check return false'), Integer.from_int(127)}, // non-minimal form
	ASNIntegerTest{[u8(0xff), 0xf0], error('Integer: check return false'), Integer.from_int(-16)}, // non-minimal form
	ASNIntegerTest{[], error('Integer: check return false'), Integer.from_i64(0)}, // empty integer
	ASNIntegerTest{[u8(0x00)], none, Integer.from_i64(0)},
	ASNIntegerTest{[u8(0x7f)], none, Integer.from_int(127)},
	ASNIntegerTest{[u8(0x00), 0x80], none, Integer.from_int(128)},
	ASNIntegerTest{[u8(0x01), 0x00], none, Integer.from_int(256)},
	ASNIntegerTest{[u8(0x80)], none, Integer.from_int(-128)},
	ASNIntegerTest{[u8(0xff), 0x7f], none, Integer.from_int(-129)},
	ASNIntegerTest{[u8(0x80), 0x00, 0x00, 0x00], none, Integer.from_string('-2147483648') or {
		panic(err)
	}},
	ASNIntegerTest{[u8(0x80), 0x00, 0x00, 0x00, 0x00], none, Integer.from_i64(-549755813888)},
	ASNIntegerTest{[u8(0x00), 0x7f], error('Integer: check return false'), Integer.from_i64(0)},
	ASNIntegerTest{[u8(0xff), 0xf0], error('Integer: check return false'), Integer.from_i64(0)}, // not minimally
]

// from golang encoding/asn1 test
fn test_asn1_unpack_and_validate() {
	for i, v in integer_test_data {
		ret := Integer.unpack_and_validate(v.bytes) or {
			assert err == v.err
			continue
		}
		// compared directly, with ret == v.expected, failed with -cstrict option
		// assert ret == v.expected
		// > assert ret == v.expected
		// Left value (len: 28): `Integer -9223372036854775808`
		// Right value (len: 28):`Integer -9223372036854775808`
		assert ret.equal(v.expected)
	}
}

fn test_asn1_integer_simple_long_integer_pack_unpack() ! {
	num := Integer.from_hex('0102030405060708090a0b0c0d0e0f')!
	dst := encode(num)!

	expected := '\x02\x0f\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f'.bytes()
	assert dst == expected

	// unpack back
	val, pos := Integer.decode(expected)!
	assert val == num
	assert pos == 17

	// test with negative value
	negnum := Integer.from_hex('-0102030405060708090a0b0c0d0e0f')!
	out := encode(negnum)!
	expneg := '\x02\x0f\xfe\xfd\xfc\xfb\xfa\xf9\xf8\xf7\xf6\xf5\xf4\xf3\xf2\xf1\xf1'.bytes()
	assert out == expneg

	// unpack back a negative number
	val2, pos2 := Integer.decode(expneg)!
	assert val2 == negnum
	assert pos2 == 17
}

// from asn-one test cases
// FIXME: Its still need a fix, remove additional 0x00 before 0x8f in unpack operation
fn test_integer_large_int() ! {
	bytes := [u8(0x02), 0x81, 0x81, 0x00, 0x8f, 0xe2, 0x41, 0x2a, 0x08, 0xe8, 0x51, 0xa8, 0x8c,
		0xb3, 0xe8, 0x53, 0xe7, 0xd5, 0x49, 0x50, 0xb3, 0x27, 0x8a, 0x2b, 0xcb, 0xea, 0xb5, 0x42,
		0x73, 0xea, 0x02, 0x57, 0xcc, 0x65, 0x33, 0xee, 0x88, 0x20, 0x61, 0xa1, 0x17, 0x56, 0xc1,
		0x24, 0x18, 0xe3, 0xa8, 0x08, 0xd3, 0xbe, 0xd9, 0x31, 0xf3, 0x37, 0x0b, 0x94, 0xb8, 0xcc,
		0x43, 0x08, 0x0b, 0x70, 0x24, 0xf7, 0x9c, 0xb1, 0x8d, 0x5d, 0xd6, 0x6d, 0x82, 0xd0, 0x54,
		0x09, 0x84, 0xf8, 0x9f, 0x97, 0x01, 0x75, 0x05, 0x9c, 0x89, 0xd4, 0xd5, 0xc9, 0x1e, 0xc9,
		0x13, 0xd7, 0x2a, 0x6b, 0x30, 0x91, 0x19, 0xd6, 0xd4, 0x42, 0xe0, 0xc4, 0x9d, 0x7c, 0x92,
		0x71, 0xe1, 0xb2, 0x2f, 0x5c, 0x8d, 0xee, 0xf0, 0xf1, 0x17, 0x1e, 0xd2, 0x5f, 0x31, 0x5b,
		0xb1, 0x9c, 0xbc, 0x20, 0x55, 0xbf, 0x3a, 0x37, 0x42, 0x45, 0x75, 0xdc, 0x90, 0x65]
	expected_integer := Integer.from_string('101038645214968213029489864879507742420925199145132483818978980455132582258676381289000109319204510275496178360219909358646064503513889573494768497419381751359787623037449375660247011308028102339473875820259375735204357343091558075960601364303443174344509161224592926325506446708043127306053676664799729848421')!
	out, pos := Integer.decode(bytes)!

	assert pos == bytes.len

	assert out.tag() == expected_integer.tag() // success
	outbytes := out.bytes()
	expbytes := expected_integer.bytes()
	assert outbytes == expbytes // success

	// this direct assert fails
	// BUG?: there are some issues when compared out == expected directly, even internally its a same,
	// but it fails to assert, so we provide and use equality check
	// assert out == expected_integer
	// dump(out)
	// dump(expected_integer)
	assert out.equal(expected_integer)

	// pack back
	dst := encode(expected_integer)!
	assert dst == bytes

	dst2 := encode(out)!
	assert dst2 == bytes
}

// This test taken from: https://learn.microsoft.com/id-id/windows/win32/seccertenroll/about-integer
// 02 81 81          ; INTEGER (81 Bytes)
// |  00
// |  8f e2 41 2a 08 e8 51 a8  8c b3 e8 53 e7 d5 49 50
// |  b3 27 8a 2b cb ea b5 42  73 ea 02 57 cc 65 33 ee
// |  88 20 61 a1 17 56 c1 24  18 e3 a8 08 d3 be d9 31
// |  f3 37 0b 94 b8 cc 43 08  0b 70 24 f7 9c b1 8d 5d
// |  d6 6d 82 d0 54 09 84 f8  9f 97 01 75 05 9c 89 d4
// |  d5 c9 1e c9 13 d7 2a 6b  30 91 19 d6 d4 42 e0 c4
// |  9d 7c 92 71 e1 b2 2f 5c  8d ee f0 f1 17 1e d2 5f
// |  31 5b b1 9c bc 20 55 bf  3a 37 42 45 75 dc 90 65
fn test_integer_decode_encode_with_real_data() ! {
	data := [u8(0x02), 0x81, 0x81, 0x00, 0x8f, 0xe2, 0x41, 0x2a, 0x08, 0xe8, 0x51, 0xa8, 0x8c,
		0xb3, 0xe8, 0x53, 0xe7, 0xd5, 0x49, 0x50, 0xb3, 0x27, 0x8a, 0x2b, 0xcb, 0xea, 0xb5, 0x42,
		0x73, 0xea, 0x02, 0x57, 0xcc, 0x65, 0x33, 0xee, 0x88, 0x20, 0x61, 0xa1, 0x17, 0x56, 0xc1,
		0x24, 0x18, 0xe3, 0xa8, 0x08, 0xd3, 0xbe, 0xd9, 0x31, 0xf3, 0x37, 0x0b, 0x94, 0xb8, 0xcc,
		0x43, 0x08, 0x0b, 0x70, 0x24, 0xf7, 0x9c, 0xb1, 0x8d, 0x5d, 0xd6, 0x6d, 0x82, 0xd0, 0x54,
		0x09, 0x84, 0xf8, 0x9f, 0x97, 0x01, 0x75, 0x05, 0x9c, 0x89, 0xd4, 0xd5, 0xc9, 0x1e, 0xc9,
		0x13, 0xd7, 0x2a, 0x6b, 0x30, 0x91, 0x19, 0xd6, 0xd4, 0x42, 0xe0, 0xc4, 0x9d, 0x7c, 0x92,
		0x71, 0xe1, 0xb2, 0x2f, 0x5c, 0x8d, 0xee, 0xf0, 0xf1, 0x17, 0x1e, 0xd2, 0x5f, 0x31, 0x5b,
		0xb1, 0x9c, 0xbc, 0x20, 0x55, 0xbf, 0x3a, 0x37, 0x42, 0x45, 0x75, 0xdc, 0x90, 0x65]

	// try to decode
	val, pos := Integer.decode(data)!
	value := val.as_bigint()! // big.Integer
	assert pos == data.len

	// serialized back
	int_element := Integer.from_bigint(value)
	serialized_int := encode(int_element)!
	assert serialized_int == data

	// with decode
	el := decode(data)!
	assert el.equal(val)
}

// This is taken from https://letsencrypt.org/id/docs/a-warm-welcome-to-asn1-and-der/
// for integer encoding
fn test_integer_encoding() ! {
	data := [u8(0x02), 0x09, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
	// try to decode
	val, pos := Integer.decode(data)!
	value := val.as_bigint()! // big.Integer
	assert pos == data.len

	// serialized back
	int_element := Integer.from_bigint(value)
	serialized_int := encode(int_element)!
	assert serialized_int == data

	// with decode
	el := decode(data)!
	assert el.equal(val)
}

// taken from https://github.com/PeculiarVentures/ASN1.js test.
fn test_negative_number() ! {
	value := Integer.from_string('-9150748177064392952')!

	out := encode(value)!
	assert out.hex() == '02088102030405060708'

	back := decode(out)!
	back_value := back.into_object[Integer]()!
	assert back_value.value.str() == '-9150748177064392952'

	assert back_value.value == value.value
}

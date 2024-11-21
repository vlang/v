// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

struct OidWriteTest {
	inp []int
	exp []u8
	err IError
}

fn test_write_oid() ! {
	dt := [
		OidWriteTest{[], [], error('ObjectIdentifier: bad oid int array')}, // empty arc
		OidWriteTest{[0], [u8(0x00)], error('ObjectIdentifier: bad oid int array')}, // only root arc
		OidWriteTest{[0, 0], [u8(0x00)], none},
		OidWriteTest{[3, 0], [u8(0x00)], error('ObjectIdentifier: bad oid int array')}, // first arc, 3 is not allowed value
		OidWriteTest{[0, 40], [u8(0x00)], error('ObjectIdentifier: bad oid int array')}, // second arc, 40 is not allowed (its should <= 39)
		OidWriteTest{[1, 40], [u8(0x00)], error('ObjectIdentifier: bad oid int array')}, // second arc, 40 is not allowed (its should <= 39)
		OidWriteTest{[1, 2], [u8(0x2a)], none},
		OidWriteTest{[2, 5], [u8(0x55)], none},
		OidWriteTest{[1, 2, 840], [u8(0x2a), 0x86, 0x48], none},
		OidWriteTest{[1, 2, 840, 113549], [u8(0x2a), 0x86, 0x48, 0x86, 0xF7, 0x0D], none},
		OidWriteTest{[1, 2, 840, 113549, 1], [u8(0x2a), 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01], none},
	]

	for item in dt {
		oid := ObjectIdentifier.from_ints(item.inp) or {
			assert err == item.err
			continue
		}

		dst := oid.payload()!

		assert dst == item.exp
	}
}

struct BuildOidTest {
	inp []int
	out ObjectIdentifier
	err IError
}

fn test_oid_from_ints() ! {
	td := [
		BuildOidTest{[1, 2], ObjectIdentifier{
			value: [1, 2]
		}, none},
		BuildOidTest{[1, 2, 3], ObjectIdentifier{
			value: [1, 2, 3]
		}, none},
		BuildOidTest{[1, 4, 4], ObjectIdentifier{
			value: [1, 4, 4]
		}, none},
		BuildOidTest{[1, 39, 6, 256], ObjectIdentifier{
			value: [1, 39, 6, 256]
		}, none},
		// second >= 40 when first < 2 not allowed
		BuildOidTest{[1, 40, 4], ObjectIdentifier{
			value: [1, 40, 4]
		}, error('ObjectIdentifier: bad oid int array')},
		// first value bigger than 2 was not allowed
		BuildOidTest{[4, 5, 6], ObjectIdentifier{
			value: [4, 5, 6]
		}, error('ObjectIdentifier: bad oid int array')},
		// second value >= 40 was not allowed when first < 2
		BuildOidTest{[1, 40, 6], ObjectIdentifier{
			value: [1, 40, 6]
		}, error('ObjectIdentifier: bad oid int array')},
		BuildOidTest{[2, 50, 6], ObjectIdentifier{
			value: [2, 50, 6]
		}, error('ObjectIdentifier: bad oid int array')},
		BuildOidTest{[1, 4, 863123683], ObjectIdentifier{
			value: [1, 4, 863123683]
		}, error('overflow parse_int result')},
		BuildOidTest{[4, 0xab, 4], ObjectIdentifier{
			value: [4, 0xab, 4]
		}, error('ObjectIdentifier: bad oid int array')},
		BuildOidTest{[4, 0x0c, 4], ObjectIdentifier{
			value: [4, 0x0c, 4]
		}, error('ObjectIdentifier: bad oid int array')},
		BuildOidTest{[2], ObjectIdentifier{
			value: [2]
		}, error('ObjectIdentifier: bad oid int array')},
	]
	for i, c in td {
		s := ObjectIdentifier.from_ints(c.inp) or {
			assert err == c.err
			continue
		}
		assert s == c.out
	}
}

struct OidStrTest {
	inp string
	out ObjectIdentifier
	err IError
}

fn test_oid_from_string() ! {
	td := [
		OidStrTest{'1.2.840.113549', ObjectIdentifier{
			value: [1, 2, 840, 113549]
		}, none},
		OidStrTest{'1.3.6.1.3', ObjectIdentifier{
			value: [1, 3, 6, 1, 3]
		}, none},
		OidStrTest{'1.2', ObjectIdentifier{
			value: [1, 2]
		}, none},
		OidStrTest{'1.4.4', ObjectIdentifier{
			value: [1, 4, 4]
		}, none},
		OidStrTest{'1.4.x', ObjectIdentifier{
			value: [1, 4, 4]
		}, error('common_parse_uint: syntax error x')}, // invalid char
		OidStrTest{'4.4.4', ObjectIdentifier{
			value: [4, 4, 4]
		}, error('ObjectIdentifier: bad oid string')},
		OidStrTest{'1.4.863123683', ObjectIdentifier{
			value: [1, 4, 863123683]
		}, none},
		OidStrTest{'4.ab.4', ObjectIdentifier{
			value: [4, 0xab, 4]
		}, error('common_parse_uint: syntax error ab')}, // invalid char
		OidStrTest{'4.c.4', ObjectIdentifier{
			value: [4, 0x0c, 4]
		}, error('common_parse_uint: syntax error c')}, // invalid char
		OidStrTest{'2', ObjectIdentifier{
			value: [2]
		}, error('ObjectIdentifier: bad string oid length')},
	]
	for s in td {
		v := ObjectIdentifier.new(s.inp) or {
			assert err == s.err
			continue
		}
		assert v == s.out
	}
}

fn test_serialize_oid_basic() {
	// https://www.oss.com/asn1/resources/asn1-made-simple/asn1-quick-reference/object-identifier.html
	inp := [1, 0, 8571, 2, 1]
	exp := [u8(6), 5, 0x28, 0xC2, 0x7B, 0x02, 0x01]
	oid := ObjectIdentifier.from_ints(inp)!

	out := encode(oid)!

	assert out == exp
}

struct OidSerializeTest {
	inp []int
	exp []u8
	err IError
}

fn test_serialize_decode_oid() {
	td := [
		OidSerializeTest{[0, 0], [u8(0x06), 0x01, 0x00], none},
		OidSerializeTest{[1, 2, 3], [u8(0x06), 0x02, 0x2a, 0x03], none},
		OidSerializeTest{[1, 3, 6, 1, 3], [u8(0x06), 0x04, 0x2b, 0x06, 1, 3], none},
		OidSerializeTest{[2, 999, 1234], [u8(0x06), 0x04, 0x88, 0x37, 0x89, 0x52], none},
		OidSerializeTest{[2, 999, 3], [u8(0x06), 0x03, 0x88, 0x37, 0x03], none}, // Example of ITU-T X.690
		// from https://learn.microsoft.com/en-us/windows/win32/seccertenroll/about-object-identifier
		OidSerializeTest{[1, 3, 6, 1, 4, 1, 311, 21, 20], [u8(0x06), 0x09, 0x2b, 0x06, 0x01, 0x04,
			0x01, 0x82, 0x37, 0x15, 0x14], none},
		// from rust-asn1 test data
		OidSerializeTest{[1, 2, 840, 113549], [u8(0x06), 0x06, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d], none},
		OidSerializeTest{[1, 2, 3, 4], [u8(0x06), 0x03, 0x2a, 0x03, 0x04], none},
		OidSerializeTest{[1, 2, 840, 133549, 1, 1, 5], [u8(0x06), 0x09, 0x2a, 0x86, 0x48, 0x88,
			0x93, 0x2d, 0x01, 0x01, 0x05], none},
		OidSerializeTest{[2, 100, 3], [u8(0x06), 0x03, 0x81, 0x34, 0x03], none},
		OidSerializeTest{[1, 100, 3], [u8(0x06), 0x03, 0x81, 0x34, 0x03], error('ObjectIdentifier: bad oid int array')},
		OidSerializeTest{[4, 100, 3], [u8(0x06), 0x03, 0x81, 0x34, 0x03], error('ObjectIdentifier: bad oid int array')},
	]
	for t in td {
		// dump(t.inp)
		oid := ObjectIdentifier.from_ints(t.inp) or {
			assert err == t.err
			continue
		}
		out := encode(oid) or {
			assert err == t.err
			continue
		}

		assert out == t.exp
		// dump(out)
		// decode back
		oidback, next := ObjectIdentifier.decode(out)!

		assert oidback.tag().tag_number() == int(TagType.oid)
		assert oidback == oid
	}
}

fn test_oid_encode_decode() ! {
	inp := '1.2.840.113549'

	src := ObjectIdentifier.new(inp)!

	out := encode(src)!
	exp := [u8(0x06), 0x06, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d]

	assert out == exp

	oidback, _ := ObjectIdentifier.decode(out)!

	assert oidback.str() == '${inp}'
	assert oidback.tag().tag_number() == 6
}

fn test_tc21_long_format_of_oid_encoding_should_error_in_der() ! {
	data := [u8(0x06), 0x06, 0x80, 0x80, 0x51, 0x80, 0x80, 0x01]

	_, _ := ObjectIdentifier.decode(data) or {
		assert err == error('integer is not minimaly encoded')
		return
	}
}

fn test_tc22_too_big_value_oid() ! {
	data := [u8(0x06), 0x10, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x0f,
		0x85, 0x03, 0x02, 0x02, 0x03]

	_, _ := ObjectIdentifier.decode(data) or {
		assert err == error('integer is not minimaly encoded')
		return
	}
}

// Taken from https://letsencrypt.org/id/docs/a-warm-welcome-to-asn1-and-der/
//
// OID 1.2.840.113549.1.1.11 (representing sha256WithRSAEncryption) is encoded like so:
// 06 09 2a 86 48 86 f7 0d 01 01 0b
fn test_sha256withrsaencryption_oid() ! {
	oid := ObjectIdentifier.new('1.2.840.113549.1.1.11')!
	expected := [u8(0x06), 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]

	out := encode(oid)!
	assert out == expected

	// decode back
	ob, _ := ObjectIdentifier.decode(out)!
	assert ob.equal(oid)
}

// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

struct BooleanTest {
	inp []u8
	out bool
	err IError
}

fn test_encode_decode_boolean_in_der_rule() {
	bd := [
		BooleanTest{[u8(1), 0x01, 0xff], true, none},
		BooleanTest{[u8(1), 0x01, 0x00], false, none},
		BooleanTest{[u8(1), 0x01, 0x10], false, error('Boolean: in DER, other than 0xff is not allowed for true value')}, // invalid value
		BooleanTest{[u8(1), 0x02, 0x00], false, error('Boolean: should have length 1')}, // bad length
		BooleanTest{[u8(1), 0x00, 0x00], false, error('Boolean: should have length 1')}, // bad length
		BooleanTest{[u8(2), 0x01, 0x00], false, error('Unexpected non-boolean tag')}, // bad tag number
	]
	for c in bd {
		out, _ := Boolean.decode(c.inp) or {
			assert err == c.err
			continue
		}
		// out.value is now u8, call .value() instead
		assert out.value() == c.out
	}
}

fn test_parse_boolean_with_parser() ! {
	data := [u8(0x01), 0x01, 0xff]
	mut p := Parser.new(data)
	out := Boolean.parse(mut p)!
	assert out.str() == 'Boolean (TRUE)'
}

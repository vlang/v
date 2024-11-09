// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

struct IA5StringTest {
	src string
	out []u8
	err IError
}

fn test_ia5string_handling() ! {
	data := [
		IA5StringTest{'test', [u8(22), 4, 116, 101, 115, 116], none},
		IA5StringTest{'abc', '\x16\x03abc'.bytes(), none},
		IA5StringTest{`🚀`.str(), []u8{}, error('IA5String: contains non-ascii chars')},
		IA5StringTest{')', '\x16\x01)'.bytes(), none},
		IA5StringTest{'\x13\x03ab\x00', []u8{}, error('IA5String: contains non-ascii chars')},
	]

	for c in data {
		s := IA5String.new(c.src) or {
			assert err == c.err
			continue
		}
		out := encode(s) or {
			assert err == c.err
			continue
		}
		assert out == c.out

		// unpack back
		mut p := Parser.new(out)
		ret := IA5String.parse(mut p) or {
			assert err == c.err
			continue
		}

		assert ret.tag().tag_number() == 22
		assert ret.tag().tag_class() == TagClass.universal
		assert ret.tag().is_constructed() == false
	}
}

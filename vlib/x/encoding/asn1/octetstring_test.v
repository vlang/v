// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

struct OctetStringTest {
	inp string
	exp []u8
	err IError
}

fn test_octetstring_handling() ! {
	data := [
		OctetStringTest{'', [u8(0x04), 0x00], none},
		OctetStringTest{'abc', [u8(0x04), 0x03, 97, 98, 99], none},
	]

	for o in data {
		os := OctetString.new(o.inp)!
		out := encode(os) or {
			assert err == o.err
			continue
		}
		assert out == o.exp

		outback, _ := OctetString.decode(out)!
		assert outback.value == o.inp
	}
}

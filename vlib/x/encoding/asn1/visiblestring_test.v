// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// VisibleString
struct VisibleTest {
	inp string
	out []u8
	err IError
}

fn test_visible_string_handling() {
	vb := [
		VisibleTest{'', [u8(26), 0], none},
		VisibleTest{'abc', [u8(26), 0x03, 97, 98, 99], none},
		VisibleTest{'abc\x1A', [u8(26), 0x03, 97, 98, 99, 26], error('VisibleString: contains control chars')},
		VisibleTest{'abc\x5A', [u8(26), 0x04, 97, 98, 99, 0x5a], none},
	]

	for i, c in vb {
		vs := VisibleString.new(c.inp) or {
			assert err == c.err
			continue
		}
		mut out := encode(vs) or {
			assert err == c.err
			continue
		}

		assert out == c.out

		// back
		vsback, idx := VisibleString.decode(out) or {
			assert err == c.err
			continue
		}

		assert vsback.tag().tag_number() == int(TagType.visiblestring)
		assert vsback.value == c.inp
	}
}

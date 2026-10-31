struct WideCase {
	units []u16  // UTF-16 code units
	hex   string // the bytes that the resulting string has to hold
}

const wide_cases = [
	// A high surrogate followed by a low surrogate is one code point.
	WideCase{
		units: [u16(0xd83d), 0xde00]
		hex:   'f09f9880'
	},
	WideCase{
		units: [u16(0xd800), 0xdc00] // the first pair, U+10000
		hex:   'f0908080'
	},
	WideCase{
		units: [u16(0xdbff), 0xdfff] // the last pair, U+10FFFF
		hex:   'f48fbfbf'
	},
	WideCase{
		units: [u16(`a`), u16(`b`), 0xd83d, 0xde00] // a pair at the end of the input
		hex:   '6162f09f9880'
	},
	WideCase{
		units: [u16(0xd83d), 0xde00, u16(`a`)]
		hex:   'f09f988061'
	},
	WideCase{
		units: [u16(0xd83d), 0xde00, 0xd83d, 0xde01] // two pairs
		hex:   'f09f9880f09f9881'
	},
	// An unpaired surrogate is kept in its 3 byte form, like wtf8_from_wide does.
	WideCase{
		units: [u16(0xd83d)] // a lone high surrogate, which is also the last unit
		hex:   'eda0bd'
	},
	WideCase{
		units: [u16(0xde00)] // a lone low surrogate
		hex:   'edb880'
	},
	WideCase{
		units: [u16(`a`), 0xd83d, u16(`b`)]
		hex:   '61eda0bd62'
	},
	WideCase{
		units: [u16(0xde00), 0xd83d] // a low surrogate followed by a high one
		hex:   'edb880eda0bd'
	},
	WideCase{
		units: [u16(0xd83d), 0xd83d, 0xde00] // a lone high surrogate, then a pair
		hex:   'eda0bdf09f9880'
	},
	// The Basic Multilingual Plane is unchanged.
	WideCase{
		units: [u16(`A`), 0xe9, 0x20ac, 0xd7ff, 0xe000, 0xffff]
		hex:   '41c3a9e282aced9fbfee8080efbfbf'
	},
	WideCase{
		units: []u16{}
		hex:   ''
	},
]

fn test_string_from_wide2_decodes_surrogate_pairs() {
	for c in wide_cases {
		s := unsafe { string_from_wide2(c.units.data, c.units.len) }
		assert s.bytes().hex() == c.hex, c.units.str()
	}
}

fn test_string_from_wide_decodes_surrogate_pairs() {
	for c in wide_cases {
		mut terminated := c.units.clone()
		terminated << u16(0)
		s := unsafe { string_from_wide(terminated.data) }
		assert s.bytes().hex() == c.hex, c.units.str()
	}
}

fn test_string_from_wide2_stops_at_the_given_length() {
	units := [u16(`a`), 0xd83d, 0xde00, u16(`b`)]
	// The low surrogate is outside of the given length: the high one is unpaired.
	assert unsafe { string_from_wide2(units.data, 2) }.bytes().hex() == '61eda0bd'
	assert unsafe { string_from_wide2(units.data, 3) }.bytes().hex() == '61f09f9880'
	assert unsafe { string_from_wide2(units.data, 4) } == 'a😀b'
}

fn test_string_from_wide_keeps_text_of_the_basic_multilingual_plane() {
	for text in ['', 'abc 123', 'Проба', 'é € 中文 한국어'] {
		mut units := []u16{}
		for r in text.runes() {
			units << u16(r)
		}
		assert unsafe { string_from_wide2(units.data, units.len) } == text
		units << u16(0)
		assert unsafe { string_from_wide(units.data) } == text
	}
}

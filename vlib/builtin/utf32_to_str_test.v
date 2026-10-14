// The UTF-8 encoding of U+FFFD, the replacement character.
const replacement_bytes = [u8(0xef), 0xbf, 0xbd]

struct ValidCodePoint {
	code  u32
	bytes []u8 // the expected UTF-8 bytes
}

// Valid code points at both ends of every encoded length, and on both sides of the surrogate range.
const valid_code_points = [
	ValidCodePoint{
		code:  0x00
		bytes: [u8(0x00)]
	},
	ValidCodePoint{
		code:  0x41
		bytes: [u8(0x41)]
	},
	ValidCodePoint{
		code:  0x7f
		bytes: [u8(0x7f)]
	},
	ValidCodePoint{
		code:  0x80
		bytes: [u8(0xc2), 0x80]
	},
	ValidCodePoint{
		code:  0x7ff
		bytes: [u8(0xdf), 0xbf]
	},
	ValidCodePoint{
		code:  0x800
		bytes: [u8(0xe0), 0xa0, 0x80]
	},
	ValidCodePoint{
		code:  0xd7ff // the last code point before the surrogates
		bytes: [u8(0xed), 0x9f, 0xbf]
	},
	ValidCodePoint{
		code:  0xe000 // the first code point after the surrogates
		bytes: [u8(0xee), 0x80, 0x80]
	},
	ValidCodePoint{
		code:  0xfffd
		bytes: [u8(0xef), 0xbf, 0xbd]
	},
	ValidCodePoint{
		code:  0xffff
		bytes: [u8(0xef), 0xbf, 0xbf]
	},
	ValidCodePoint{
		code:  0x10000
		bytes: [u8(0xf0), 0x90, 0x80, 0x80]
	},
	ValidCodePoint{
		code:  0x10ffff // the last valid code point
		bytes: [u8(0xf4), 0x8f, 0xbf, 0xbf]
	},
]

// Surrogate halves and values above 0x10FFFF are not Unicode scalar values.
const invalid_code_points = [u32(0xd800), 0xdbff, 0xdc00, 0xdfff, 0x110000, 0x7fffffff, 0x80000000,
	0xffffffff]

fn encode(code u32) []u8 {
	mut buf := [5]u8{}
	n := unsafe { utf32_decode_to_buffer(code, mut &buf[0]) }
	return buf[..n].clone()
}

fn test_utf32_decode_to_buffer_keeps_valid_code_points() {
	for v in valid_code_points {
		assert encode(v.code) == v.bytes, 'code 0x${v.code:x}'
	}
}

fn test_utf32_decode_to_buffer_writes_the_replacement_character_for_an_invalid_code_point() {
	for code in invalid_code_points {
		assert encode(code) == replacement_bytes, 'code 0x${code:x}'
	}
}

fn test_utf32_to_str_no_malloc_terminates_the_buffer() {
	for v in valid_code_points {
		mut buf := [u8(0xaa), 0xaa, 0xaa, 0xaa, 0xaa]!
		s := unsafe { utf32_to_str_no_malloc(v.code, mut &buf[0]) }
		assert s.bytes() == v.bytes, 'code 0x${v.code:x}'
		assert buf[v.bytes.len] == 0, 'code 0x${v.code:x}'
	}
	for code in invalid_code_points {
		mut buf := [u8(0xaa), 0xaa, 0xaa, 0xaa, 0xaa]!
		s := unsafe { utf32_to_str_no_malloc(code, mut &buf[0]) }
		assert s.bytes() == replacement_bytes, 'code 0x${code:x}'
		assert buf[3] == 0, 'code 0x${code:x}'
	}
}

fn test_utf32_to_str() {
	for v in valid_code_points {
		assert utf32_to_str(v.code).bytes() == v.bytes, 'code 0x${v.code:x}'
	}
	for code in invalid_code_points {
		assert utf32_to_str(code).bytes() == replacement_bytes, 'code 0x${code:x}'
	}
}

fn test_rune_to_string_conversions_agree() {
	mut codes := []u32{}
	mut expected := [][]u8{}
	for v in valid_code_points {
		codes << v.code
		expected << v.bytes
	}
	for code in invalid_code_points {
		codes << code
		expected << replacement_bytes
	}
	for i, code in codes {
		r := rune(code)
		want := expected[i]
		assert r.str().bytes() == want, 'code 0x${code:x}'
		assert '${r}'.bytes() == want, 'code 0x${code:x}'
		assert r.bytes() == want, 'code 0x${code:x}'
		assert [r].string().bytes() == want, 'code 0x${code:x}'
		assert r.repeat(1).bytes() == want, 'code 0x${code:x}'
		mut twice := want.clone()
		twice << want
		assert r.repeat(2).bytes() == twice, 'code 0x${code:x}'
	}
}

fn test_an_invalid_rune_does_not_disturb_its_neighbours() {
	s := [rune(`a`), 0xd800, `€`, 0x110000, `😀`].string()
	mut want := [u8(`a`)]
	want << replacement_bytes
	want << '€'.bytes()
	want << replacement_bytes
	want << '😀'.bytes()
	assert s.bytes() == want
	// The result is valid UTF-8: decoding it gives U+FFFD for each invalid rune.
	assert s.runes() == [rune(`a`), 0xfffd, `€`, 0xfffd, `😀`]
}

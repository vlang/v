import strings

// The UTF-8 encoding of U+FFFD, the replacement character.
const replacement = 'efbfbd'

struct ValidRune {
	r   rune
	hex string // the expected UTF-8 bytes
}

// Valid runes at both ends of every encoded length, and on both sides of the surrogate range.
const valid_runes = [
	ValidRune{
		r:   0x00
		hex: '00'
	},
	ValidRune{
		r:   0x41
		hex: '41'
	},
	ValidRune{
		r:   0x7f
		hex: '7f'
	},
	ValidRune{
		r:   0x80
		hex: 'c280'
	},
	ValidRune{
		r:   0x7ff
		hex: 'dfbf'
	},
	ValidRune{
		r:   0x800
		hex: 'e0a080'
	},
	ValidRune{
		r:   0xd7ff // the last rune before the surrogates
		hex: 'ed9fbf'
	},
	ValidRune{
		r:   0xe000 // the first rune after the surrogates
		hex: 'ee8080'
	},
	ValidRune{
		r:   0xfffd
		hex: 'efbfbd'
	},
	ValidRune{
		r:   0xffff
		hex: 'efbfbf'
	},
	ValidRune{
		r:   0x10000
		hex: 'f0908080'
	},
	ValidRune{
		r:   0x10ffff // the last valid rune
		hex: 'f48fbfbf'
	},
]

// Surrogate halves and values above 0x10FFFF are not Unicode scalar values.
const invalid_runes = [rune(0xd800), 0xdbff, 0xdc00, 0xdfff, 0x110000, 0x7fffffff, 0x80000000,
	rune(-1)]

fn hex_of(mut sb strings.Builder) string {
	return sb.str().bytes().hex()
}

fn test_write_rune_keeps_valid_runes() {
	for v in valid_runes {
		mut sb := strings.new_builder(8)
		sb.write_rune(v.r)
		assert hex_of(mut sb) == v.hex, 'rune 0x${u32(v.r):x}'
	}
	mut sb := strings.new_builder(8)
	sb.write_rune(`A`)
	sb.write_rune(`é`)
	sb.write_rune(`€`)
	sb.write_rune(`😀`)
	assert sb.str() == 'Aé€😀'
}

fn test_write_rune_writes_the_replacement_character_for_an_invalid_rune() {
	for r in invalid_runes {
		mut sb := strings.new_builder(8)
		sb.write_rune(r)
		assert sb.len == 3, 'rune 0x${u32(r):x}'
		assert hex_of(mut sb) == replacement, 'rune 0x${u32(r):x}'
	}
}

fn test_write_runes_keeps_valid_runes() {
	mut runes := []rune{}
	mut expected := ''
	for v in valid_runes {
		runes << v.r
		expected += v.hex
	}
	mut sb := strings.new_builder(8)
	sb.write_runes(runes)
	assert hex_of(mut sb) == expected
}

fn test_write_runes_writes_the_replacement_character_for_an_invalid_rune() {
	mut sb := strings.new_builder(8)
	sb.write_runes([rune(0x41), 0xd800, 0x42, 0x110000])
	assert sb.len == 8
	assert hex_of(mut sb) == '41' + replacement + '42' + replacement

	sb.write_runes(invalid_runes)
	assert sb.len == 3 * invalid_runes.len
	assert hex_of(mut sb) == replacement.repeat(invalid_runes.len)

	// An invalid rune does not disturb its neighbours.
	sb.write_runes([`a`, rune(0xdfff), `€`, rune(-1), `😀`])
	assert sb.str() == 'a' + [u8(0xef), 0xbf, 0xbd].bytestr() + '€' +
		[u8(0xef), 0xbf, 0xbd].bytestr() + '😀'
}

fn test_write_repeated_rune_keeps_valid_runes() {
	for v in valid_runes {
		mut sb := strings.new_builder(8)
		sb.write_repeated_rune(v.r, 3)
		assert hex_of(mut sb) == v.hex.repeat(3), 'rune 0x${u32(v.r):x}'
	}
}

fn test_write_repeated_rune_writes_the_replacement_character_for_an_invalid_rune() {
	for r in invalid_runes {
		mut sb := strings.new_builder(8)
		sb.write_repeated_rune(r, 1)
		assert hex_of(mut sb) == replacement, 'rune 0x${u32(r):x}'
		sb.write_repeated_rune(r, 4)
		assert sb.len == 12, 'rune 0x${u32(r):x}'
		assert hex_of(mut sb) == replacement.repeat(4), 'rune 0x${u32(r):x}'
		sb.write_repeated_rune(r, 0)
		sb.write_repeated_rune(r, -1)
		assert sb.len == 0, 'rune 0x${u32(r):x}'
	}
}

fn test_every_way_of_writing_a_rune_gives_the_same_bytes() {
	mut all := []rune{}
	for v in valid_runes {
		all << v.r
	}
	all << invalid_runes
	for r in all {
		mut one := strings.new_builder(8)
		one.write_rune(r)
		mut many := strings.new_builder(8)
		many.write_runes([r])
		mut repeated := strings.new_builder(8)
		repeated.write_repeated_rune(r, 1)
		expected := hex_of(mut one)
		assert hex_of(mut many) == expected, 'rune 0x${u32(r):x}'
		assert hex_of(mut repeated) == expected, 'rune 0x${u32(r):x}'
		assert r.str().bytes().hex() == expected, 'rune 0x${u32(r):x}'
		assert [r].string().bytes().hex() == expected, 'rune 0x${u32(r):x}'
	}
}

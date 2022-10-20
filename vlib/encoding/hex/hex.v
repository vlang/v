module hex

import strings

// decode converts a hex string into an array of bytes. The expected
// input format is 2 ASCII characters for each output byte. If the provided
// string length is not a multiple of 2, an implicit `0` is prepended to it.
pub fn decode(s string) ![]u8 {
	mut hex_str := s
	if hex_str.len >= 2 {
		if s[0] == `0` && (s[1] == `x` || s[1] == `X`) {
			hex_str = s[2..]
		}
	}
	if hex_str.len == 0 {
		return []u8{}
	} else if hex_str.len == 1 {
		return [char2nibble(hex_str[0])!]
	} else if hex_str.len == 2 {
		n1 := char2nibble(hex_str[0])!
		n0 := char2nibble(hex_str[1])!
		return [(n1 << 4) | n0]
	}
	// calculate the first byte depending on if hex_str.len is odd
	mut val := char2nibble(hex_str[0])!
	if hex_str.len & 1 == 0 {
		val = (val << 4) | char2nibble(hex_str[1])!
	}
	// set cap to hex_str.len/2 rounded up
	mut bytes := []u8{len: 1, cap: (hex_str.len + 1) >> 1, init: val}
	// iterate over every 2 bytes
	// the start index depends on if hex_str.len is odd
	for i := 2 - (hex_str.len & 1); i < hex_str.len; i += 2 {
		n1 := char2nibble(hex_str[i])!
		n0 := char2nibble(hex_str[i + 1])!
		bytes << (n1 << 4) | n0
	}
	return bytes
}

// encode converts an array of bytes into a string of ASCII hex bytes. The
// output will always be a string with length a multiple of 2.
[manualfree]
pub fn encode(bytes []u8) string {
	mut sb := strings.new_builder(bytes.len * 2)
	for b in bytes {
		sb.write_string(b.hex())
	}
	res := sb.str()
	unsafe { sb.free() }
	return res
}

// char2nibble converts an ASCII hex character to it's hex value
fn char2nibble(b u8) !u8 {
	match b {
		`0`...`9` { return b - u8(`0`) }
		`A`...`F` { return b - u8(`A`) + 10 }
		`a`...`f` { return b - u8(`a`) + 10 }
		else { return error('invalid hex char $b.ascii_str()') }
	}
}

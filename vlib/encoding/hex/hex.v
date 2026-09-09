module hex

const hex_digits = '0123456789abcdef'
const hex_digits_upper = '0123456789ABCDEF'

// EncodeParams configures optional output formatting for encode.
//
// If `uppercase` is set, `encode` emits `A-F` instead of `a-f`.
// If `with_prefix` is non-empty, `encode` prepends that exact string.
@[params]
pub struct EncodeParams {
pub mut:
	uppercase   bool
	with_prefix string
}

// decode converts a hex string into an array of bytes.
// The expected input format is 2 ASCII characters for each output byte.
// If the provided string length is not a multiple of 2, the first digit is
// decoded as if an implicit `0` preceded it. An optional `0x` or `0X` prefix
// is accepted.
@[direct_array_access]
pub fn decode(s string) ![]u8 {
	if s.len == 0 {
		return []u8{}
	}

	mut offset := 0
	mut hex_bytes := if s.len >= 2 {
		if s[0] == `0` && (s[1] == `x` || s[1] == `X`) {
			offset = 2
			s[2..].bytes()
		} else {
			s.bytes()
		}
	} else {
		s.bytes()
	}

	if hex_bytes.len == 0 {
		return []u8{}
	}

	mut bytes := []u8{cap: (hex_bytes.len + 1) >> 1}
	mut start := 0
	if hex_bytes.len & 1 == 1 {
		bytes << char2nibble(hex_bytes[0], offset)!
		start = 1
	}

	for i := start; i < hex_bytes.len; i += 2 {
		n1 := char2nibble(hex_bytes[i], offset + i)!
		n0 := char2nibble(hex_bytes[i + 1], offset + i + 1)!
		bytes << (n1 << 4) | n0
	}
	return bytes
}

// encode converts an array of bytes into a string of ASCII hex bytes. The
// output will always be a string whose length will be a multiple of 2.
// If `EncodeParams.uppercase` is set, the output hex characters are emitted in
// uppercase.
// If `EncodeParams.with_prefix` is non-empty, the output string is prefixed
// with the provided string.
@[direct_array_access]
pub fn encode(bytes []u8, params EncodeParams) string {
	if bytes.len == 0 {
		return ''
	}
	mut res := []u8{}
	if params.with_prefix != '' {
		res << params.with_prefix.bytes()
	}
	for _, b in bytes {
		res << nibble2char(b >> 4, params)
		res << nibble2char(b & 0xf, params)
	}
	return res.bytestr()
}

// nibble2char converts a 4-bit hex value to its ASCII character
@[inline]
fn nibble2char(nibble u8, params EncodeParams) u8 {
	if params.uppercase {
		return hex_digits_upper[nibble]
	}
	return hex_digits[nibble]
}

// char2nibble converts an ASCII hex character to its hex value
@[inline]
fn char2nibble(b u8, index int) !u8 {
	match b {
		`0`...`9` { return b - u8(`0`) }
		`A`...`F` { return b - u8(`A`) + 10 }
		`a`...`f` { return b - u8(`a`) + 10 }
		else { return error('invalid hex char ${b.ascii_str()} at index ${index}') }
	}
}

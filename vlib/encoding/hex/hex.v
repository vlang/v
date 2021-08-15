module hex

import strconv
import strings

// decode converts a hex string into an array of bytes. The expected
// input format is 2 ASCII characters for each output byte. If the provided
// string length is not a multiple of 2, an implicit `0` is prepended to it.
pub fn decode(s string) ?[]byte {
	if s.len == 0 {
		return []byte{}
	} else if s.len <= 2 {
		return [byte(strconv.parse_uint(s, 16, 8) ?)]
	}
	// calculate the first byte depending on if s.len is odd
	val := byte(strconv.parse_uint(s[..2 - (s.len & 1)], 16, 8) ?)
	// set cap to s.len/2 rounded up
	mut bytes := []byte{len: 1, cap: (s.len + 1) >> 1, init: val}
	// iterate over every 2 bytes
	// the start index depends on if s.len is odd
	for i := 2 - (s.len & 1); i < s.len; i += 2 {
		bytes << byte(strconv.parse_uint(s[i..i + 2], 16, 8) ?)
	}
	return bytes
}

// encode converts an array of bytes into a string of ASCII hex bytes. The
// output will always be a string with length a multiple of 2.
[manualfree]
pub fn encode(bytes []byte) string {
	mut sb := strings.new_builder(bytes.len << 1)
	for b in bytes {
		sb.write_string(b.hex())
	}
	res := sb.str()
	unsafe { sb.free() }
	return res
}

module iconv

// Module iconv provides functions convert between vstring(UTF8) to/from different encodings.

// vstring_to_encoding convert V string `str` to `tocode` encoding string
// tips: use `iconv --list` check for supported encodings
pub fn vstring_to_encoding(str string, tocode string) []u8 {
	return conv(tocode, 'UTF-8', str.str, str.len)
}

// encoding_to_vstring converts the given `bytes` using `fromcode` encoding, to a V string (encoded with UTF-8)
// tips: use `iconv --list` check for supported encodings
pub fn encoding_to_vstring(bytes []u8, fromcode string) string {
	dst := conv('UTF-8', fromcode, bytes.data, bytes.len)
	defer {
		unsafe { dst.free() }
	}
	if dst.len == 0 {
		return ''
	}
	return unsafe { cstring_to_vstring(dst.data) }
}

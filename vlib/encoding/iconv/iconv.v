module iconv

// Module iconv provides functions convert between vstring(UTF8) to/from different encodings.

// vstring_to_encoding convert V string `str` to `tocode` encoding string
// tips: use `iconv --list` check for supported encodings
pub fn vstring_to_encoding(str string, tocode string) []u8 {
	// convert vstring to []u8
	mut src := []u8{len: str.len}
	unsafe { vmemcpy(src.data, str.str, str.len) }
	defer {
		unsafe { src.free() }
	}
	return conv(tocode, 'UTF-8', mut src)
}

// encoding_to_vstring converts the given `bytes` using `fromcode` encoding, to a V string (encoded with UTF-8)
// tips: use `iconv --list` check for supported encodings
pub fn encoding_to_vstring(bytes []u8, fromcode string) string {
	// clone bytes => src
	mut src := bytes.clone()
	defer {
		unsafe { src.free() }
	}
	mut dst := conv('UTF-8', fromcode, mut src)
	defer {
		unsafe { dst.free() }
	}
	if dst.len == 0 {
		return ''
	}
	dst << [u8(0)] // add tail zero, to build a vstring
	return unsafe { cstring_to_vstring(dst.data) }
}

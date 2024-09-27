module iconv

// Module iconv provides functions convert between vstring(UTF8) to/from different encodings.

#include <iconv.h>

fn C.iconv_open(tocode &u8, fromcode &u8) int
fn C.iconv_close(cd int) int
fn C.iconv(cd int, inbuf &&u8, inbytesleft &usize, outbuf &&u8, outbytesleft &usize) usize

// conv convert `fromcode` encoding string to `tocode` encoding string
fn conv(tocode string, fromcode string, src &u8, src_len int) []u8 {
	if isnil(src) || src_len <= 0 {
		return []u8{}
	}
	mut dst := []u8{len: src_len * 4} // this should be enough to hold the dst encoding string
	cd := C.iconv_open(tocode.str, fromcode.str)
	if cd == -1 {
		return []u8{}
	}
	defer { C.iconv_close(cd) }

	mut src_ptr := unsafe { src }
	mut dst_ptr := &u8(dst.data)
	mut src_left := usize(src_len)
	mut dst_left := usize(dst.len)
	res := C.iconv(cd, &src_ptr, &src_left, &dst_ptr, &dst_left)
	if res == usize(-1) {
		return []u8{}
	}

	// resize dst buf to real length
	dst.trim(dst.len - int(dst_left))
	return dst
}

// vstring_to_encoding convert vstring `str` to `tocode` encoding string
// tips: use `iconv --list` check for supported encodings
pub fn vstring_to_encoding(tocode string, str string) []u8 {
	return conv(tocode, 'UTF-8', str.str, str.len)
}

// encoding_to_vstring convert `fromcode` encoding string to vstring
// tips: use `iconv --list` check for supported encodings
pub fn encoding_to_vstring(fromcode string, str []u8) string {
	dst := conv('UTF-8', fromcode, str.data, str.len)
	defer {
		unsafe { dst.free() }
	}
	if dst.len == 0 {
		return ''
	}
	return unsafe { cstring_to_vstring(dst.data) }
}

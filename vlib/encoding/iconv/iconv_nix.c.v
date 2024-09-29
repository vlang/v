module iconv

// Module iconv provides functions convert between vstring(UTF8) to/from different encodings.

#include <iconv.h>
#flag darwin -liconv

fn C.iconv_open(tocode &u8, fromcode &u8) voidptr
fn C.iconv_close(cd voidptr) int
fn C.iconv(cd voidptr, inbuf &&u8, inbytesleft &usize, outbuf &&u8, outbytesleft &usize) usize

// conv convert `fromcode` encoding string to `tocode` encoding string
fn conv(tocode string, fromcode string, mut src []u8) []u8 {
	if src.len == 0 {
		return []u8{}
	}

	mut dst := []u8{len: (src.len + 1) * 4} // this should be enough to hold the dst encoding string
	cd := C.iconv_open(tocode.str, fromcode.str)
	if isize(cd) == -1 {
		unsafe { dst.free() }
		return []u8{}
	}
	defer { C.iconv_close(cd) }

	mut src_ptr := &u8(src.data)
	mut dst_ptr := &u8(dst.data)
	mut src_left := usize(src.len)
	mut dst_left := usize(dst.len)
	res := C.iconv(cd, &src_ptr, &src_left, &dst_ptr, &dst_left)
	if res == usize(-1) {
		unsafe { dst.free() }
		return []u8{}
	}

	// resize dst buf to real length
	dst.trim(dst.len - int(dst_left))
	return dst
}

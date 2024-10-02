module iconv

import os

// Module iconv provides functions convert between vstring(UTF8) to/from different encodings.

#include <iconv.h>
#flag darwin -liconv

fn C.iconv_open(tocode &u8, fromcode &u8) voidptr
fn C.iconv_close(cd voidptr) int
fn C.iconv(cd voidptr, inbuf &&u8, inbytesleft &usize, outbuf &&u8, outbytesleft &usize) usize

// conv convert `fromcode` encoding string to `tocode` encoding string
@[direct_array_access]
fn conv(tocode string, fromcode string, src &u8, src_len int) ![]u8 {
	if src_len < 0 {
		return error('src length error')
	}

	// macos-12 workaround : UTF16LE<=>UTF16BE
	mut is_src_swap := false
	mut is_dst_swap := false

	mut src_encoding := fromcode.to_upper()
	mut dst_encoding := tocode.to_upper()
	mut cd := C.iconv_open(dst_encoding.str, src_encoding.str)
	if isize(cd) == -1 {
		$if macos {
			// macos-12 workaround: platform not support UTF16LE/UTF16BE, only UTF16, which is big-endian, followed by bytes swap
			if src_encoding in ['UTF16LE', 'UTF-16LE'] {
				src_encoding = 'UTF16'
				is_src_swap = true
			} else if src_encoding in ['UTF16BE', 'UTF-16BE'] {
				src_encoding = 'UTF16'
			}

			if dst_encoding in ['UTF16LE', 'UTF-16LE'] {
				dst_encoding = 'UTF16'
				is_dst_swap = true
			} else if dst_encoding in ['UTF16BE', 'UTF-16BE'] {
				dst_encoding = 'UTF16'
			}
			cd = C.iconv_open(dst_encoding.str, src_encoding.str)
			if isize(cd) == -1 {
				os.system('/usr/bin/iconv --list') // debug
				return error('macos can\'t convert from ${src_encoding} to ${dst_encoding}')
			}
		} $else {
			return error('platform can\'t convert from ${src_encoding} to ${dst_encoding}')
		}
	}
	defer { C.iconv_close(cd) }

	mut dst := []u8{len: (src_len + 1) * 4} // this should be enough to hold the dst encoding string

	mut src_ptr := &u8(src)
	if is_src_swap {
		// macos-12 workaround: UTF16LE=>UTF16BE
		mut src_swap := []u8{len: src_len}
		unsafe { vmemcpy(src_swap.data, src, src_len) }
		mut sptr := unsafe { &u16(src_swap.data) }
		for i in 0 .. src_len / 2 {
			unsafe {
				sptr[i] = reverse_u16(sptr[i])
			}
		}
		src_ptr = &u8(src_swap.data)
	}
	mut dst_ptr := &u8(dst.data)
	mut src_left := usize(src_len)
	mut dst_left := usize(dst.len)
	res := C.iconv(cd, &src_ptr, &src_left, &dst_ptr, &dst_left)
	if res == usize(-1) {
		return error('convert encoding string fail, iconv return ${res}')
	}

	// resize dst buf to real length
	dst.trim(dst.len - int(dst_left))

	if dst_encoding in ['UTF16', 'UTF-16']! {
		// To compatible with Windows(Little Endian default), remove the first FFFE/FEFF(BOM)
		if dst.len <= 2 {
			return dst // error('convert to UTF16 length too short? no BOM?')
		}
		if (dst[0] == u8(0xFF) && dst[1] == u8(0xFE)) || (dst[0] == u8(0xFE) && dst[1] == u8(0xFF)) {
			dst.delete_many(0, 2)
		}
	}

	if dst_encoding in ['UTF32', 'UTF-32']! {
		// remove the first FFFE0000/0000FEFF(BOM)
		if dst.len <= 4 {
			return dst // error('convert to UTF32 length too short? no BOM?')
		}
		if (dst[0] == u8(0xFF) && dst[1] == u8(0xFE) && dst[2] == u8(0x00) && dst[3] == u8(0x00))
			|| (dst[0] == u8(0x00) && dst[1] == u8(0x00) && dst[2] == u8(0xFE)
			&& dst[3] == u8(0xFF)) {
			dst.delete_many(0, 4)
		}
	}

	if is_dst_swap {
		// macos-12 workaround: UTF16BE=>UTF16LE
		mut dptr := unsafe { &u16(dst.data) }
		for i in 0 .. dst.len / 2 {
			unsafe {
				dptr[i] = reverse_u16(dptr[i])
			}
		}
	}
	return dst
}

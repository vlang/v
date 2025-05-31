module iconv

// Module iconv provides functions to convert between vstring(UTF8) and other encodings.
import os

@[inline]
fn reverse_u16(src u16) u16 {
	return u16(src >> 8 | src << 8)
}

@[inline]
fn reverse_u32(src u32) u32 {
	return u32(src >> 24 | ((src >> 8) & 0x0000_FF00) | ((src << 8) & 0x00FF_0000) | src << 24)
}

// vstring_to_encoding convert V string `str` to `tocode` encoding string
// tips: use `iconv --list` check for supported encodings
pub fn vstring_to_encoding(str string, tocode string) ![]u8 {
	mut encoding_name := tocode.to_upper()
	if encoding_name in ['UTF16', 'UTF32', 'UTF-16', 'UTF-32']! {
		return error('please use UTF16-LE/UTF-16BE/UTF-32LE/UTF-32BE instead')
	}
	if encoding_name == 'LOCAL' {
		$if windows {
			encoding_name = 'ANSI'
		} $else {
			encoding_name = 'UTF-8'
		}
	}
	return conv(encoding_name, 'UTF-8', str.str, str.len)
}

// encoding_to_vstring converts the given `bytes` using `fromcode` encoding, to a V string (encoded with UTF-8)
// tips: use `iconv --list` check for supported encodings
pub fn encoding_to_vstring(bytes []u8, fromcode string) !string {
	mut encoding_name := fromcode.to_upper()
	if encoding_name in ['UTF16', 'UTF32', 'UTF-16', 'UTF-32']! {
		return error('please use UTF16-LE/UTF-16BE/UTF-32LE/UTF-32BE instead')
	}
	if encoding_name == 'LOCAL' {
		$if windows {
			encoding_name = 'ANSI'
		} $else {
			encoding_name = 'UTF-8'
		}
	}
	mut dst := conv('UTF-8', encoding_name, bytes.data, bytes.len)!
	dst << 0 // add a tail zero, to build a vstring
	return unsafe { cstring_to_vstring(dst.data) }
}

// create_utf_string_with_bom will create a utf8/utf16/utf32 string with BOM header
// for utf8, it will prepend 0xEFBBBF to the `src`
// for utf16le, it will prepend 0xFFFE to the `src`
// for utf16be, it will prepend 0xFEFF to the `src`
// for utf32le, it will prepend 0xFFFE0000 to the `src`
// for utf32be, it will prepend 0x0000FEFF to the `src`
pub fn create_utf_string_with_bom(src []u8, utf_type string) []u8 {
	mut clone := src.clone()
	mut encoding_name := utf_type.to_upper()
	if encoding_name == 'LOCAL' {
		$if windows {
			encoding_name = 'ANSI'
		} $else {
			encoding_name = 'UTF-8'
		}
	}
	match encoding_name {
		'UTF8', 'UTF-8' {
			clone.prepend([u8(0xEF), 0xBB, 0xBF])
		}
		'UTF16LE', 'UTF-16LE' {
			clone.prepend([u8(0xFF), 0xFE])
		}
		'UTF16BE', 'UTF-16BE' {
			clone.prepend([u8(0xFE), 0xFF])
		}
		'UTF32LE', 'UTF-32LE' {
			clone.prepend([u8(0xFF), 0xFE, 0, 0])
		}
		'UTF32BE', 'UTF-32BE' {
			clone.prepend([u8(0), 0, 0xFE, 0xFF])
		}
		else {}
	}
	return clone
}

// remove_utf_string_with_bom will remove a utf8/utf16/utf32 string's BOM header
// for utf8, it will remove 0xEFBBBF from the `src`
// for utf16le, it will remove 0xFFFE from the `src`
// for utf16be, it will remove 0xFEFF from the `src`
// for utf32le, it will remove 0xFFFE0000 from the `src`
// for utf32be, it will remove 0x0000FEFF from the `src`
@[direct_array_access]
pub fn remove_utf_string_with_bom(src []u8, utf_type string) []u8 {
	mut clone := src.clone()
	mut encoding_name := utf_type.to_upper()
	if encoding_name == 'LOCAL' {
		$if windows {
			encoding_name = 'ANSI'
		} $else {
			encoding_name = 'UTF-8'
		}
	}
	match encoding_name {
		'UTF8', 'UTF-8' {
			if clone.len > 3 {
				if clone[0] == u8(0xEF) && clone[1] == u8(0xBB) && clone[2] == u8(0xBF) {
					clone.delete_many(0, 3)
				}
			}
		}
		'UTF16LE', 'UTF-16LE' {
			if clone.len > 2 {
				if clone[0] == u8(0xFF) && clone[1] == u8(0xFE) {
					clone.delete_many(0, 2)
				}
			}
		}
		'UTF16BE', 'UTF-16BE' {
			if clone.len > 2 {
				if clone[0] == u8(0xFE) && clone[1] == u8(0xFF) {
					clone.delete_many(0, 2)
				}
			}
		}
		'UTF32LE', 'UTF-32LE' {
			if clone.len > 4 {
				if clone[0] == u8(0xFF) && clone[1] == u8(0xFE) && clone[2] == u8(0)
					&& clone[3] == u8(0) {
					clone.delete_many(0, 4)
				}
			}
		}
		'UTF32BE', 'UTF-32BE' {
			if clone.len > 4 {
				if clone[0] == u8(0) && clone[1] == u8(0) && clone[2] == u8(0xFE)
					&& clone[3] == u8(0xFF) {
					clone.delete_many(0, 4)
				}
			}
		}
		else {}
	}
	return clone
}

// write_file_encoding write_file convert `text` into `encoding` and writes to a file with the given `path`. If `path` already exists, it will be overwritten.
// For `encoding` in UTF8/UTF16/UTF32, if `bom` is true, then a BOM header will write to the file.
pub fn write_file_encoding(path string, text string, encoding string, bom bool) ! {
	encoding_bytes := vstring_to_encoding(text, encoding)!
	if bom && encoding.to_upper().starts_with('UTF') {
		encoding_bom_bytes := create_utf_string_with_bom(encoding_bytes, encoding)
		os.write_file_array(path, encoding_bom_bytes)!
	} else {
		os.write_file_array(path, encoding_bytes)!
	}
}

// read_file_encoding reads the file in `path` with `encoding` and returns the contents
pub fn read_file_encoding(path string, encoding string) !string {
	encoding_bytes := os.read_file_array[u8](path)
	encoding_without_bom_bytes := remove_utf_string_with_bom(encoding_bytes, encoding)
	return encoding_to_vstring(encoding_without_bom_bytes, encoding)!
}

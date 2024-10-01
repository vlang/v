module iconv

// Module iconv provides functions to convert between vstring(UTF8) and other encodings.
import os

pub enum UTF_Type {
	utf8    // UTF-8
	utf16le // UTF-16 Little-Endian
	utf16be // UTF-16 Big-Endian
	utf32le // UTF-32 Little-Endian
	utf32be // UTF-32 Big-Endian
}

// vstring_to_encoding convert V string `str` to `tocode` encoding string
// tips: use `iconv --list` check for supported encodings
pub fn vstring_to_encoding(str string, tocode string) ![]u8 {
	return conv(tocode, 'UTF-8', str.str, str.len)
}

// encoding_to_vstring converts the given `bytes` using `fromcode` encoding, to a V string (encoded with UTF-8)
// tips: use `iconv --list` check for supported encodings
pub fn encoding_to_vstring(bytes []u8, fromcode string) !string {
	mut dst := conv('UTF-8', fromcode, bytes.data, bytes.len)!
	dst << [u8(0)] // Windows: add tail zero, to build a vstring
	return unsafe { cstring_to_vstring(dst.data) }
}

// create_utf_string_with_bom will create a utf8/utf16/utf32 string with BOM header
// for .utf8, it will prepend 0xEFBBBF to the `src`
// for .utf16le, it will prepend 0xFFFE to the `src`
// for .utf16be, it will prepend 0xFEFF to the `src`
// for .utf32le, it will prepend 0xFFFE0000 to the `src`
// for .utf32be, it will prepend 0x0000FEFF to the `src`
pub fn create_utf_string_with_bom(src []u8, utf_type UTF_Type) []u8 {
	mut clone := src.clone()
	match utf_type {
		.utf8 {
			clone.prepend([u8(0xEF), 0xBB, 0xBF])
		}
		.utf16le {
			clone.prepend([u8(0xFF), 0xFE])
		}
		.utf16be {
			clone.prepend([u8(0xFE), 0xFF])
		}
		.utf32le {
			clone.prepend([u8(0xFF), 0xFE, 0, 0])
		}
		.utf32be {
			clone.prepend([u8(0), 0, 0xFE, 0xFF])
		}
	}
	return clone
}

// write_file_utf_string_with_bom will write `src` utf string to `path`, it will prepend a BOM header
pub fn write_file_utf_string_with_bom(path string, src []u8, utf_type UTF_Type) ! {
	clone := create_utf_string_with_bom(src, utf_type)
	os.write_file_array(path, clone)!
}

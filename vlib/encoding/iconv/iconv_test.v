// vtest build: !docker-ubuntu-musl // needs libiconv to be installed
import encoding.iconv
import os

fn test_vstring_to_encoding() {
	empty_utf8 := iconv.vstring_to_encoding('', 'UTF-8')!
	assert empty_utf8 == []

	abc_utf8 := iconv.vstring_to_encoding('abc', 'UTF-8')!
	assert abc_utf8 == [u8(97), 98, 99]

	abc_utf16le := iconv.vstring_to_encoding('abc', 'UTF-16LE')!
	assert abc_utf16le == [u8(97), 0, 98, 0, 99, 0]

	abc_utf16be := iconv.vstring_to_encoding('abc', 'UTF-16BE')!
	assert abc_utf16be == [u8(0), 97, 0, 98, 0, 99]

	abc_utf32le := iconv.vstring_to_encoding('abc', 'UTF-32LE')!
	assert abc_utf32le == [u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]

	abc_utf32be := iconv.vstring_to_encoding('abc', 'UTF-32BE')!
	assert abc_utf32be == [u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99]

	abc_local := iconv.vstring_to_encoding('abc', 'LOCAL')!
	// Windows LOCAL: ANSI encoding
	// Linux LOCAL: UTF-8 encoding
	assert abc_local == [u8(97), 98, 99]

	if abc_not_exist := iconv.vstring_to_encoding('abc', 'encoding_not_exist') {
		assert false, 'encoding_not_exist'
	}

	if ch_str := iconv.vstring_to_encoding('V大法好abc', 'GB2312') {
		assert ch_str == [u8(86), 180, 243, 183, 168, 186, 195, 97, 98, 99]
	} else {
		// some platforms do not support GB2312, skip
		assert true
	}
}

fn test_encoding_to_vstring() {
	empty_utf8 := iconv.encoding_to_vstring([], 'UTF-8')!
	assert empty_utf8 == ''

	abc_utf8 := iconv.encoding_to_vstring([u8(97), 98, 99], 'UTF-8')!
	assert abc_utf8 == 'abc'

	abc_utf16le := iconv.encoding_to_vstring([u8(97), 0, 98, 0, 99, 0], 'UTF-16LE')!
	assert abc_utf16le == 'abc'

	abc_utf16be := iconv.encoding_to_vstring([u8(0), 97, 0, 98, 0, 99], 'UTF-16BE')!
	assert abc_utf16be == 'abc'

	abc_utf32le := iconv.encoding_to_vstring([u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0],
		'UTF-32LE')!
	assert abc_utf32le == 'abc'

	abc_utf32be := iconv.encoding_to_vstring([u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99],
		'UTF-32BE')!
	assert abc_utf32be == 'abc'

	abc_local := iconv.encoding_to_vstring([u8(97), 98, 99], 'LOCAL')!
	// Windows LOCAL: ANSI encoding
	// Linux LOCAL: UTF-8 encoding
	assert abc_local == 'abc'

	if abc_not_exist := iconv.encoding_to_vstring([u8(97), 98, 99], 'encoding_not_exist') {
		assert false, 'encoding_not_exist'
	}

	if ch_str := iconv.encoding_to_vstring([u8(86), 180, 243, 183, 168, 186, 195, 97, 98, 99],
		'GB2312')
	{
		assert ch_str == 'V大法好abc'
	} else {
		// some platforms do not support GB2312, skip
		assert true
	}
}

fn test_create_utf_string_with_bom() {
	// bug ? vfmt create strange format here
	// vfmt off
	assert iconv.create_utf_string_with_bom([u8(97), 98, 99], 'UTF-8') == [u8(0xEF), 0xBB, 0xBF,	97, 98, 99]
	assert iconv.create_utf_string_with_bom([u8(97), 0, 98, 0, 99, 0], 'UTF-16LE') == [u8(0xFF),	0xFE, 97, 0, 98, 0, 99, 0]
	assert iconv.create_utf_string_with_bom([u8(0), 97, 0, 98, 0, 99], 'UTF-16BE') == [u8(0xFE), 0xFF, 0, 97, 0, 98, 0, 99]
	assert iconv.create_utf_string_with_bom([u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0], 'UTF-32LE') == [u8(0xFF), 0xFE, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]
	assert iconv.create_utf_string_with_bom([u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99], 'UTF-32BE') == [u8(0), 0, 0xFE, 0xFF, 0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99]
	// vfmt on
}

fn test_remove_utf_string_with_bom() {
	// bug ? vfmt create strange format here
	// vfmt off
	assert iconv.remove_utf_string_with_bom([u8(0xEF), 0xBB, 0xBF, 97, 98, 99], 'UTF-8') == [u8(97), 98, 99]
	assert iconv.remove_utf_string_with_bom([u8(0xFF), 0xFE, 97, 0, 98, 0, 99, 0], 'UTF-16LE') == [u8(97), 0, 98, 0, 99, 0]
	assert iconv.remove_utf_string_with_bom([u8(0xFE), 0xFF, 0, 97, 0, 98, 0, 99], 'UTF-16BE') == [u8(0), 97, 0, 98, 0, 99]
	assert iconv.remove_utf_string_with_bom([u8(0xFF), 0xFE, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0], 'UTF-32LE') == [u8(97), 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0]
	assert iconv.remove_utf_string_with_bom([u8(0), 0, 0xFE, 0xFF, 0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99], 'UTF-32BE') == [u8(0), 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99]
	// vfmt on
}

fn my_test_read_file_encoding_write_file_encoding(txt string, encoding string, bom bool, bytes []u8) ! {
	iconv.write_file_encoding('iconv_tmp.txt', txt, encoding, bom)!
	// read bytes directly from file
	mut bytes_ref := os.read_file_array[u8]('iconv_tmp.txt')
	assert bytes_ref == bytes
	if bom {
		bytes_ref = iconv.remove_utf_string_with_bom(bytes_ref, encoding)
	}
	str_ref := iconv.encoding_to_vstring(bytes_ref, encoding)!
	assert str_ref.bytes() == txt.bytes()
	str_conv := iconv.read_file_encoding('iconv_tmp.txt', encoding)!
	assert str_conv == txt
	os.rm('iconv_tmp.txt')!
}

fn test_read_file_encoding_write_file_encoding() ! {
	// vfmt off
	// UTF-8
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-8',false,[u8(86), 229, 164, 167, 230, 179, 149, 229, 165, 189, 97, 98, 99])!
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-8',true,[u8(0xEF), 0xBB, 0xBF, 86, 229, 164, 167, 230, 179, 149, 229, 165, 189, 97, 98, 99])!

	// UTF-16LE
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-16LE',false,[u8(86), 0, 39, 89, 213, 108, 125, 89, 97, 0, 98, 0, 99, 0])!
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-16LE',true,[u8(0xFF), 0xFE, 86, 0, 39, 89, 213, 108, 125, 89, 97, 0, 98, 0, 99, 0])!

	// UTF-16BE
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-16BE',false,[u8(0), 86, 89, 39, 108, 213, 89, 125, 0, 97, 0, 98, 0, 99])!
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-16BE',true,[u8(0xFE), 0xFF, 0, 86, 89, 39, 108, 213, 89, 125, 0, 97, 0, 98, 0, 99])!

	// UTF-32LE
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-32LE',false,[u8(86), 0, 0, 0, 39, 89, 0, 0, 213, 108, 0, 0, 125, 89, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0])!
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-32LE',true,[u8(0xFF), 0xFE, 0, 0, 86, 0, 0, 0, 39, 89, 0, 0, 213, 108, 0, 0, 125, 89, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0])!

	// UTF-32BE
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-32BE',false,[u8(0), 0, 0, 86, 0, 0, 89, 39, 0, 0, 108, 213, 0, 0, 89, 125, 0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99])!
	my_test_read_file_encoding_write_file_encoding('V大法好abc','UTF-32BE',true,[u8(0), 0, 0xFE, 0xFF, 0, 0, 0, 86, 0, 0, 89, 39, 0, 0, 108, 213, 0, 0, 89, 125, 0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99])!
	// vfmt on
}

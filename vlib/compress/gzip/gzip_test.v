module gzip

import hash.crc32
import os

const test_ftext = u8(0b0000_0001)
const test_fhcrc = u8(0b0000_0010)
const test_fextra = u8(0b0000_0100)
const test_fname = u8(0b0000_1000)
const test_fcomment = u8(0b0001_0000)
const samples_folder = os.join_path(os.dir(@FILE), 'samples')

fn test_gzip() {
	uncompressed := 'Hello world!'
	compressed := compress(uncompressed.bytes())!
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn assert_decompress_error(data []u8, reason string) ! {
	decompress(data) or {
		assert err.msg() == reason
		return
	}
	return error('did not error')
}

fn test_gzip_invalid_too_short() {
	assert_decompress_error([]u8{}, 'invalid gzip stream: too short')!
}

fn test_gzip_invalid_magic_numbers() {
	assert_decompress_error([]u8{len: 100}, 'invalid gzip stream: bad magic')!
}

fn test_gzip_invalid_compression() {
	mut data := []u8{len: 100}
	data[0] = 0x1f
	data[1] = 0x8b
	assert_decompress_error(data, 'invalid gzip stream: unsupported compression method')!
}

fn test_gzip_with_ftext() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= test_ftext
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fname() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= test_fname
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fcomment() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= test_fcomment
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fname_fcomment() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= (test_fname | test_fcomment)
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fextra() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= test_fextra
	// XLEN is 2-byte little-endian value
	xlen := u16(2)
	compressed.insert(10, u8(xlen))
	compressed.insert(11, u8(xlen >> 8))
	compressed.insert(12, `h`)
	compressed.insert(13, `i`)
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_hcrc() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= test_fhcrc
	// FHCRC is 2-byte CRC-16 (low 16 bits of CRC32) in little-endian format
	checksum := crc32.sum(compressed[..10])
	crc16 := u16(checksum & 0xffff)
	compressed.insert(10, u8(crc16))
	compressed.insert(11, u8(crc16 >> 8))
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_invalid_hcrc() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= test_fhcrc
	// FHCRC is 2-byte CRC-16 (low 16 bits of CRC32) in little-endian format
	checksum := crc32.sum(compressed[..10])
	crc16 := u16(checksum & 0xffff)
	compressed.insert(10, u8(crc16))
	compressed.insert(11, u8((crc16 >> 8) + 1)) // corrupt high byte
	assert_decompress_error(compressed, 'invalid gzip stream: header crc16 mismatch')!
}

fn test_gzip_with_invalid_checksum() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[compressed.len - 5] += 1
	assert_decompress_error(compressed, 'invalid gzip stream: crc32 mismatch')!
}

fn test_gzip_with_invalid_length() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[compressed.len - 1] += 1
	assert_decompress_error(compressed, 'invalid gzip stream: size mismatch')!
}

fn test_gzip_with_invalid_flags() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= 0b1000_0000
	assert_decompress_error(compressed, 'invalid gzip stream: reserved flags set')!
}

fn test_gzip_decompress_callback() {
	uncompressed := '321323'.repeat(10_000)
	gz := compress(uncompressed.bytes())!
	mut size := 0
	mut ref := &size
	decoded := decompress_with_callback(gz, fn (chunk []u8, ref &int) int {
		unsafe {
			*ref += chunk.len
		}
		return chunk.len
	}, ref)!
	assert decoded == size
	assert decoded == uncompressed.len
}

fn test_gzip_decompress_callback_rejects_non_gzip() {
	z := [u8(0x78), 0x9c, 0x03, 0x00, 0x00, 0x00, 0x01]
	decompress_with_callback(z, fn (chunk []u8, _ voidptr) int {
		return chunk.len
	}, unsafe { nil }) or {
		assert err.msg() == 'invalid gzip stream: too short'
		return
	}
	assert false
}

fn s(fname string) string {
	return os.join_path(samples_folder, fname)
}

fn read_and_decode_file(fpath string) !([]u8, string) {
	compressed := os.read_bytes(fpath)!
	decoded := decompress(compressed)!
	content := decoded.bytestr()
	return compressed, content
}

fn test_reading_and_decoding_a_known_gziped_file() {
	compressed, content := read_and_decode_file(s('known.gz'))!
	assert compressed#[0..3] == [u8(31), 139, 8]
	assert compressed#[-5..] == [u8(127), 115, 1, 0, 0]
	assert content.contains('## Description')
	assert content.contains('## Examples:')
	assert content.ends_with('```\n')
}

fn test_decoding_all_samples_files() {
	for gz_file in os.walk_ext(samples_folder, '.gz') {
		_, content := read_and_decode_file(gz_file)!
		assert content.len > 0, 'decoded content should not be empty: `${content}`'
	}
}

fn test_reading_gzip_files_compressed_with_different_options() {
	_, content1 := read_and_decode_file(s('readme_level_1.gz'))!
	_, content5 := read_and_decode_file(s('readme_level_5.gz'))!
	_, content9 := read_and_decode_file(s('readme_level_9.gz'))!
	_, content9_rsyncable := read_and_decode_file(s('readme_level_9_rsyncable.gz'))!
	assert content9_rsyncable == content9
	assert content9 == content5
	assert content5 == content1
}

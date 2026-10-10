module gzip

import encoding.binary
import encoding.hex
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

// decompress_error returns the error message of decompress() for `data`, '' for no error.
fn decompress_error(data []u8) string {
	decompress(data) or { return err.msg() }
	return ''
}

// callback_error returns the error message of decompress_with_callback() for `data`,
// '' for no error.
fn callback_error(data []u8) string {
	decompress_with_callback(data, fn (chunk []u8, _ voidptr) int {
		return chunk.len
	}, unsafe { nil }) or { return err.msg() }
	return ''
}

fn must_decode_hex(s string) []u8 {
	return hex.decode(s) or { panic(err) }
}

// block_type_payloads returns raw DEFLATE payloads of 'abacabad' x 16, one per block type and
// indexed by it: 0 stored, 1 fixed Huffman, 2 dynamic Huffman. The reference zlib made them.
fn block_type_payloads() [][]u8 {
	mut stored := [u8(0x01), 0x80, 0x00, 0x7f, 0xff] // BFINAL=1, BTYPE=00, LEN=128, NLEN=~LEN
	stored << 'abacabad'.repeat(16).bytes()
	return [
		stored,
		must_decode_hex('4b4c4a4c4e4c4a4c491c201a00'),
		must_decode_hex('05c10101000008c3a0acccf7cf20c8c9e464723239999c4c4e26279393c9c9e464723239999c4c4e262793933d'),
	]
}

// gzip_stream wraps a raw DEFLATE payload that decodes to `plain` in a gzip container.
fn gzip_stream(payload []u8, plain []u8) []u8 {
	mut out := [u8(0x1f), 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff]
	out << payload
	out << binary.little_endian_get_u32(crc32.sum(plain))
	out << binary.little_endian_get_u32(u32(plain.len))
	return out
}

// valid_streams returns gzip streams with every kind of payload: one per DEFLATE block type,
// what compress() writes for a few sizes, and the multi-block files written by gzip(1).
fn valid_streams() ![][]u8 {
	plain := 'abacabad'.repeat(16).bytes()
	mut streams := [][]u8{}
	for btype, payload in block_type_payloads() {
		assert (payload[0] >> 1) & 3 == btype
		streams << gzip_stream(payload, plain)
		assert decompress(streams.last())! == plain
	}
	// the empty input, short ones, and more than one callback chunk
	for size in [0, 1, 12, 40_000] {
		streams << compress('abcdefghij'.repeat(4_000)[..size].bytes())!
	}
	for name in ['known.gz', 'readme_level_1.gz', 'readme_level_9.gz'] {
		streams << os.read_bytes(s(name))!
	}
	for good in streams {
		assert decompress_error(good) == ''
		assert callback_error(good) == ''
	}
	return streams
}

// assert_trailer_field_error checks that flipping any single bit of the 4 trailer bytes at
// `offset_from_end`, with the payload intact, is reported as `reason`.
fn assert_trailer_field_error(offset_from_end int, reason string) ! {
	for good in valid_streams()! {
		start := good.len - offset_from_end
		for i in start .. start + 4 {
			for bit in 0 .. 8 {
				mut bad := good.clone()
				bad[i] ^= u8(1) << bit
				assert decompress_error(bad) == reason
				assert callback_error(bad) == reason
			}
		}
	}
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

fn test_gzip_wrong_crc32_alone_is_a_crc32_mismatch() {
	assert_trailer_field_error(8, 'invalid gzip stream: crc32 mismatch')!
}

fn test_gzip_wrong_isize_alone_is_a_size_mismatch() {
	assert_trailer_field_error(4, 'invalid gzip stream: size mismatch')!
}

fn test_gzip_damaged_payload_is_reported_as_such_whatever_the_trailer() {
	good := must_decode_hex('1f8b08000000000000fff348cdc9c95728cf2fca495104009519851b0c000000')
	assert decompress(good)! == 'Hello world!'.bytes()
	// Inverting the first payload byte turns the block header into the one of a dynamic
	// Huffman block, and what follows is not a valid code length table.
	mut damaged := good.clone()
	damaged[10] ^= 0xff
	payload_error := decompress_error(damaged)
	assert payload_error != ''
	assert payload_error !in ['invalid gzip stream: crc32 mismatch',
		'invalid gzip stream: size mismatch']
	assert callback_error(damaged) == payload_error
	// Nothing was decoded, so there is nothing to compare the trailer with:
	// a wrong crc32 or isize on top of that does not change the error.
	for i in [damaged.len - 8, damaged.len - 4] {
		mut both := damaged.clone()
		both[i] ^= 0x01
		assert decompress_error(both) == payload_error
		assert callback_error(both) == payload_error
	}
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

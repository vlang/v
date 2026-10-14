module zlib

import encoding.binary
import encoding.hex
import hash.adler32

fn must_decode_hex(s string) []u8 {
	return hex.decode(s) or { panic(err) }
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

// zlib_stream wraps a raw DEFLATE payload that decodes to `plain` in a zlib container.
fn zlib_stream(payload []u8, plain []u8) []u8 {
	mut out := [u8(0x78), 0x9c]
	out << payload
	out << binary.big_endian_get_u32(adler32.sum(plain))
	return out
}

fn test_zlib_roundtrip_text() {
	data := 'Hello world!'.bytes()
	compressed := compress(data)!
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_zlib_roundtrip_empty() {
	data := []u8{}
	compressed := compress(data)!
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_zlib_roundtrip_binary() {
	data := [u8(0), 1, 2, 3, 127, 128, 254, 255]
	compressed := compress(data)!
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_zlib_roundtrip_large() {
	data := 'abcdefgh'.repeat(1000).bytes()
	compressed := compress(data)!
	assert compressed.len < data.len
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_zlib_decompress_known_python_vector() {
	compressed := must_decode_hex('789ccb48cdc9c95728cf2fca49e102001e720467')
	decompressed := decompress(compressed)!
	assert decompressed == 'hello world\n'.bytes()
}

fn test_zlib_invalid_too_short() {
	assert_decompress_error([]u8{}, 'invalid zlib stream: too short')!
}

fn test_zlib_invalid_header_checksum() {
	assert_decompress_error([u8(0x78), 0x9d, 0x00, 0x00, 0x00, 0x01],
		'invalid zlib stream: bad header checksum')!
}

fn test_zlib_invalid_truncated_payload() {
	decompress([u8(0x78), 0x9c, 0x03, 0x00, 0x00, 0x00, 0x01]) or {
		assert err.msg().contains('unexpected end of stream')
		return
	}
	assert false
}

fn test_zlib_invalid_inserted_bytes_before_adler() {
	enc := compress('zlib edge-case regression'.repeat(5).bytes())!
	mut bad := []u8{cap: enc.len + 1}
	bad << enc[..enc.len - 4]
	bad << u8(0x7f)
	bad << enc[enc.len - 4..]
	assert_decompress_error(bad, 'invalid zlib stream: trailing data before adler32')!
}

fn test_zlib_wrong_adler32_alone_is_an_adler32_mismatch() {
	plain := 'abacabad'.repeat(16).bytes()
	mut streams := [][]u8{}
	for btype, payload in block_type_payloads() {
		assert (payload[0] >> 1) & 3 == btype
		streams << zlib_stream(payload, plain)
		assert decompress(streams.last())! == plain
	}
	// what compress() writes: the empty input, short ones, and more than one callback chunk
	for size in [0, 1, 12, 40_000] {
		streams << compress('abcdefghij'.repeat(4_000)[..size].bytes())!
	}
	for good in streams {
		assert decompress_error(good) == ''
		assert callback_error(good) == ''
		for i in good.len - 4 .. good.len {
			for bit in 0 .. 8 {
				mut bad := good.clone()
				bad[i] ^= u8(1) << bit
				assert decompress_error(bad) == 'invalid zlib stream: adler32 mismatch'
				assert callback_error(bad) == 'invalid zlib stream: adler32 mismatch'
			}
		}
	}
}

fn test_zlib_damaged_payload_is_reported_as_such_whatever_the_adler32() {
	good := must_decode_hex('789cf348cdc9c95728cf2fca495104001d09045e')
	assert decompress(good)! == 'Hello world!'.bytes()
	// Inverting the first payload byte turns the block header into the one of a dynamic
	// Huffman block, and what follows is not a valid code length table.
	mut damaged := good.clone()
	damaged[2] ^= 0xff
	payload_error := decompress_error(damaged)
	assert payload_error != ''
	assert payload_error != 'invalid zlib stream: adler32 mismatch'
	assert callback_error(damaged) == payload_error
	// Nothing was decoded, so there is nothing to compare the adler32 with:
	// a wrong adler32 on top of that does not change the error.
	damaged[damaged.len - 1] ^= 0x01
	assert decompress_error(damaged) == payload_error
	assert callback_error(damaged) == payload_error
}

fn test_zlib_decompress_callback() {
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

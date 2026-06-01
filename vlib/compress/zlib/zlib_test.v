module zlib

import encoding.hex

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

module gzip

import hash.crc32

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
	assert_decompress_error([]u8{}, 'data is too short, not gzip compressed?')!
}

fn test_gzip_invalid_magic_numbers() {
	assert_decompress_error([]u8{len: 100}, 'wrong magic numbers, not gzip compressed?')!
}

fn test_gzip_invalid_compression() {
	mut data := []u8{len: 100}
	data[0] = 0x1f
	data[1] = 0x8b
	assert_decompress_error(data, 'gzip data is not compressed with DEFLATE')!
}

fn test_gzip_with_ftext() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= ftext
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fname() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= fname
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fcomment() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= fcomment
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fname_fcomment() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= (fname | fcomment)
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
	compressed[3] |= fextra
	compressed.insert(10, 2)
	compressed.insert(11, `h`)
	compressed.insert(12, `i`)
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_hcrc() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= fhcrc
	checksum := crc32.sum(compressed[..10])
	compressed.insert(10, u8(checksum >> 24))
	compressed.insert(11, u8(checksum >> 16))
	compressed.insert(12, u8(checksum >> 8))
	compressed.insert(13, u8(checksum))
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_invalid_hcrc() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= fhcrc
	checksum := crc32.sum(compressed[..10])
	compressed.insert(10, u8(checksum >> 24))
	compressed.insert(11, u8(checksum >> 16))
	compressed.insert(12, u8(checksum >> 8))
	compressed.insert(13, u8(checksum + 1))
	assert_decompress_error(compressed, 'header checksum verification failed')!
}

fn test_gzip_with_invalid_checksum() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[compressed.len - 5] += 1
	assert_decompress_error(compressed, 'checksum verification failed')!
}

fn test_gzip_with_invalid_length() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[compressed.len - 1] += 1
	assert_decompress_error(compressed, 'length verification failed, got 12, expected 16777228')!
}

fn test_gzip_with_invalid_flags() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[3] |= 0b1000_0000
	assert_decompress_error(compressed, 'reserved flags are set, unsupported field detected')!
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

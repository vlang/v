module gzip

import hash.crc32

fn test_gzip() ? {
	uncompressed := 'Hello world!'
	compressed := compress(uncompressed.bytes())?
	decompressed := decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}

fn assert_decompress_error(data []u8, reason string) ? {
	decompress(data) or {
		assert err.msg() == reason
		return
	}
	return error('did not error')
}

fn test_gzip_invalid_too_short() ? {
	assert_decompress_error([]u8{}, 'data is too short, not gzip compressed?')?
}

fn test_gzip_invalid_magic_numbers() ? {
	assert_decompress_error([]u8{len: 100}, 'wrong magic numbers, not gzip compressed?')?
}

fn test_gzip_invalid_compression() ? {
	mut data := []u8{len: 100}
	data[0] = 0x1f
	data[1] = 0x8b
	assert_decompress_error(data, 'gzip data is not compressed with DEFLATE')?
}

fn test_gzip_with_ftext() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[4] |= 0b0000_0001 // FTEXT
	decompressed := decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fname() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[4] |= 0b0000_1000
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	decompressed := decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fcomment() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[4] |= 0b0001_0000
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	decompressed := decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fname_fcomment() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[4] |= 0b0001_1000
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	compressed.insert(10, `h`)
	compressed.insert(11, `i`)
	compressed.insert(12, 0x00)
	decompressed := decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_fextra() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[4] |= 0b0000_0100
	compressed.insert(10, 2)
	compressed.insert(11, `h`)
	compressed.insert(12, `i`)
	decompressed := decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_hcrc() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[4] |= 0b0000_0010
	checksum := crc32.sum(compressed[..10])
	compressed.insert(10, u8(checksum >> 24))
	compressed.insert(11, u8(checksum >> 16))
	compressed.insert(12, u8(checksum >> 8))
	compressed.insert(13, u8(checksum))
	decompressed := decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}

fn test_gzip_with_invalid_hcrc() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[4] |= 0b0000_0010
	checksum := crc32.sum(compressed[..10])
	compressed.insert(10, u8(checksum >> 24))
	compressed.insert(11, u8(checksum >> 16))
	compressed.insert(12, u8(checksum >> 8))
	compressed.insert(13, u8(checksum + 1))
	assert_decompress_error(compressed, 'header checksum verification failed')?
}

fn test_gzip_with_invalid_checksum() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[compressed.len - 5] += 1
	assert_decompress_error(compressed, 'checksum verification failed')?
}

fn test_gzip_with_invalid_length() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[compressed.len - 1] += 1
	assert_decompress_error(compressed, 'length verification failed, got 12, expected 13')?
}

fn test_gzip_with_invalid_flags() ? {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())?
	compressed[4] |= 0b1000_0000
	assert_decompress_error(compressed, 'reserved flags are set, unsupported field detected')?
}

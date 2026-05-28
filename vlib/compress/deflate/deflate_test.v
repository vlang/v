module deflate

fn test_zlib_roundtrip() {
	data := 'Hello world!'.bytes()
	compressed := compress(data)!
	assert compressed[0] == 0x78 && compressed[1] == 0x9c // zlib header
	assert decompress(compressed)! == data
}

fn test_gzip_roundtrip() {
	data := 'Hello gzip!'.repeat(10).bytes()
	compressed := compress(data, format: .gzip)!
	assert compressed[0] == 0x1f && compressed[1] == 0x8b // gzip magic
	assert decompress(compressed)! == data
}

fn test_raw_deflate_roundtrip() {
	data := 'raw deflate'.repeat(20).bytes()
	raw := compress(data, format: .raw_deflate)!
	decoded := decompress(raw)! // auto-detected as raw
	assert decoded == data
}

fn test_decompress_auto_detects_all_formats() {
	data := 'multi-format detection test'.repeat(5).bytes()
	assert decompress(compress(data)!)! == data
	assert decompress(compress(data, format: .gzip)!)! == data
	assert decompress(compress(data, format: .raw_deflate)!)! == data
}

fn test_wrapper_helpers_match_unified_api() {
	data := 'wrapper compatibility'.repeat(8).bytes()
	assert compress(data)! == compress(data, format: .zlib)!
	assert compress_gzip(data)! == compress(data, format: .gzip)!
	assert compress_raw(data)! == compress(data, format: .raw_deflate)!
}

fn test_roundtrip_repeated() {
	data := 'abcabc'.repeat(100).bytes()
	compressed := compress(data)!
	assert compressed.len < data.len
	assert decompress(compressed)! == data
}

fn test_bad_compression_method_fails() {
	bad := [u8(0x79), 0x18, 0x00, 0x00, 0x00, 0x00]
	decompress(bad) or {
		assert err.msg().len > 0
		return
	}
	assert false
}

fn test_corrupt_checksum_fails() {
	mut enc := compress(('hello world').repeat(10).bytes())!
	// flip a byte in the adler32 footer
	enc[enc.len - 1] ^= 0xff
	decompress(enc) or {
		assert err.msg().contains('adler32')
		return
	}
	assert false
}

fn test_truncated_zlib_payload_fails() {
	decompress([u8(0x78), 0x9c, 0x03, 0x00, 0x00, 0x00, 0x01]) or {
		assert err.msg().contains('unexpected end of stream')
		return
	}
	assert false
}

fn test_zlib_inserted_bytes_before_adler_fails() {
	enc := compress('zlib injected trailer bytes'.repeat(4).bytes())!
	mut bad := []u8{cap: enc.len + 2}
	bad << enc[..enc.len - 4]
	bad << [u8(0xaa), 0x55]
	bad << enc[enc.len - 4..]
	decompress(bad) or {
		assert err.msg() == 'invalid zlib stream: trailing data before adler32'
		return
	}
	assert false
}

fn test_gzip_inserted_bytes_before_trailer_fails() {
	enc := compress('gzip injected trailer bytes'.repeat(4).bytes(), format: .gzip)!
	mut bad := []u8{cap: enc.len + 1}
	bad << enc[..enc.len - 8]
	bad << u8(0x42)
	bad << enc[enc.len - 8..]
	decompress(bad) or {
		assert err.msg() == 'invalid gzip stream: trailing data before trailer'
		return
	}
	assert false
}

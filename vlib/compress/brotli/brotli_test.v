module brotli

fn skip_without_brotli() bool {
	if is_available() {
		return false
	}
	eprintln('skipping compress.brotli tests; libbrotli is not available')
	return true
}

fn test_brotli_roundtrip_text() {
	if skip_without_brotli() {
		return
	}
	data := 'Hello Brotli! '.repeat(1000).bytes()
	compressed := compress(data, mode: .text, quality: 5)!
	assert compressed.len < data.len
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_brotli_roundtrip_empty() {
	if skip_without_brotli() {
		return
	}
	data := []u8{}
	compressed := compress(data)!
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_brotli_roundtrip_binary() {
	if skip_without_brotli() {
		return
	}
	data := []u8{len: 4096, init: u8(index * 31 + (index >> 2))}
	compressed := compress(data, quality: 3)!
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_brotli_invalid_params() {
	if skip_without_brotli() {
		return
	}
	compress('bad params'.bytes(), quality: 12) or {
		assert err.msg() == 'brotli: quality must be between 0 and 11'
		return
	}
	assert false
}

fn test_brotli_invalid_stream() {
	if skip_without_brotli() {
		return
	}
	decompress('not brotli'.bytes()) or {
		assert err.msg().starts_with('brotli:')
		return
	}
	assert false
}

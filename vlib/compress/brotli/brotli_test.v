module brotli

fn skip_without_brotli() bool {
	if is_available() {
		return false
	}
	eprintln('skipping compress.brotli tests; libbrotli is not available')
	return true
}

struct BrotliTestCallbackSink {
mut:
	data  []u8
	calls int
}

fn collect_brotli_test_chunk(chunk []u8, userdata voidptr) int {
	mut sink := unsafe { &BrotliTestCallbackSink(userdata) }
	sink.data << chunk
	sink.calls++
	return chunk.len
}

fn abort_brotli_test_chunk(_ []u8, userdata voidptr) int {
	mut sink := unsafe { &BrotliTestCallbackSink(userdata) }
	sink.calls++
	return 0
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

fn test_brotli_callback_full_decode_and_abort() {
	if skip_without_brotli() {
		return
	}
	data := 'callback output '.repeat(8192).bytes()
	compressed := compress(data, quality: 4)!
	mut full := BrotliTestCallbackSink{}
	count := decompress_with_callback(compressed, collect_brotli_test_chunk, voidptr(&full))!
	assert count == data.len
	assert full.data == data
	assert full.calls > 1

	mut aborted := BrotliTestCallbackSink{}
	partial_count := decompress_with_callback(compressed, abort_brotli_test_chunk,
		voidptr(&aborted))!
	assert partial_count == 0
	assert aborted.calls == 1
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

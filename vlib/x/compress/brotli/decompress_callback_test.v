module brotli

fn test_decompress_with_callback_matches_decompress() {
	for size in [0, 1, 12, 40_000, 100_000] {
		plain := 'abcdefghij'.repeat(10_000)[..size].bytes()
		compressed := compress(plain)!
		expected := decompress(compressed)!
		assert expected == plain
		mut collected := []u8{}
		mut ref := &collected
		delivered := decompress_with_callback(compressed, fn (chunk []u8, ref &[]u8) int {
			unsafe {
				*ref << chunk
			}
			return chunk.len
		}, ref)!
		assert delivered == expected.len
		assert collected == expected
	}
}

fn test_decompress_with_callback_stops_early_without_error() {
	plain := 'abcdefghij'.repeat(10_000).bytes()
	compressed := compress(plain)!
	mut calls := 0
	mut ref := &calls
	delivered := decompress_with_callback(compressed, fn (chunk []u8, calls &int) int {
		unsafe {
			*calls += 1
		}
		return 0
	}, ref)!
	assert calls == 1
	assert delivered == 0
}

fn test_decompress_with_callback_propagates_decode_errors() {
	bad := [u8(0x00), 0x01, 0x02]
	decompress(bad) or { assert err.msg() != '' }
	decompress_with_callback(bad, fn (chunk []u8, _ voidptr) int {
		return chunk.len
	}, unsafe { nil }) or {
		assert err.msg() != ''
		return
	}
	assert false, 'invalid input must fail in both decompress and decompress_with_callback'
}

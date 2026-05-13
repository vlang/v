module snappy

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn round_trip(label string, input []u8) ! {
	compressed := compress(input)
	decompressed := decompress(compressed)!
	assert decompressed == input, '${label}: round-trip mismatch (input len=${input.len})'
	bound := max_compressed_length(input.len)
	assert compressed.len <= bound, '${label}: compressed size ${compressed.len} exceeds bound ${bound}'
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

fn test_empty() {
	round_trip('empty', []u8{}) or { panic(err) }
}

fn test_single_byte() {
	round_trip('single byte', [u8(0x42)]) or { panic(err) }
}

fn test_two_bytes() {
	round_trip('two bytes', [u8(0xde), 0xad]) or { panic(err) }
}

fn test_all_zeros() {
	// Highly compressible — long run of the same byte.
	input := []u8{len: 4096, init: 0}
	compressed := compress(input)
	decompressed := decompress(compressed) or { panic(err) }
	assert decompressed == input, 'all-zeros round-trip failed'
	// Should compress to much less than input.
	assert compressed.len < input.len / 4, 'all-zeros should compress well'
}

fn test_all_same_nonzero() {
	input := []u8{len: 1024, init: u8(0xff)}
	round_trip('all-0xff', input) or { panic(err) }
}

fn test_incompressible() {
	// Pseudo-random bytes — poor compressibility.
	mut input := []u8{len: 512}
	mut seed := u32(0xdeadbeef)
	for i in 0 .. input.len {
		seed = seed * 1664525 + 1013904223 // LCG
		input[i] = u8(seed >> 24)
	}
	round_trip('pseudo-random', input) or { panic(err) }
}

fn test_repeated_pattern() {
	// Short repeating pattern — exercises the copy back-reference paths.
	pattern := 'abcdefgh'.bytes()
	mut input := []u8{cap: pattern.len * 200}
	for _ in 0 .. 200 {
		input << pattern
	}
	compressed := compress(input)
	decompressed := decompress(compressed) or { panic(err) }
	assert decompressed == input, 'repeated-pattern round-trip failed'
	assert compressed.len < input.len / 2, 'repeated pattern should compress well'
}

fn test_lorem_ipsum() {
	lorem := 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' +
		'Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ' +
		'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ' +
		'nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ' +
		'reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ' +
		'pariatur. Excepteur sint occaecat cupidatat non proident, sunt in ' +
		'culpa qui officia deserunt mollit anim id est laborum.'
	mut input := []u8{cap: lorem.len * 50}
	for _ in 0 .. 50 {
		input << lorem.bytes()
	}
	round_trip('lorem-ipsum-x50', input) or { panic(err) }
}

fn test_multi_block() {
	// Larger than one 64 KiB block to exercise the block-splitting path.
	mut input := []u8{len: 200_000, init: u8(index & 0xff)}
	round_trip('multi-block', input) or { panic(err) }
}

fn test_max_compressed_length_bound() {
	for n in [0, 1, 100, 1000, 65536, 200_000] {
		bound := max_compressed_length(n)
		mut input := []u8{len: n, init: u8(index & 0xff)}
		compressed := compress(input)
		assert compressed.len <= bound, 'max_compressed_length(${n})=${bound} exceeded by ${compressed.len}'
	}
}

fn test_decompress_invalid_varint() {
	// A stream that never terminates the varint (all bytes have MSB set).
	bad := [u8(0xff), 0xff, 0xff, 0xff, 0xff, 0xff]
	decompress(bad) or { return } // expected — an error is correct
	panic('expected decompress to fail on invalid varint')
}

fn test_decompress_truncated_literal() {
	// Header says 10 bytes uncompressed; literal tag claims 10 bytes
	// but the data is truncated.
	bad := [u8(10), u8(9 << 2), u8(0x41)] // varint(10) + tag + 1 byte
	decompress(bad) or { return }
	panic('expected decompress to fail on truncated literal')
}

fn test_decompress_bad_offset() {
	// A COPY_2 that references before the start of output.
	//   varint(5)  tag=COPY_2(len=5, _)  offset=999 (past output)
	bad := [u8(5), u8(2 | ((5 - 1) << 2)), u8(0xe7), u8(0x03)]
	decompress(bad) or { return }
	panic('expected decompress to fail on out-of-range offset')
}

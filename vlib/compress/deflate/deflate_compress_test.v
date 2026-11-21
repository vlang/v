module deflate

fn roundtrip(data []u8, level int) ! {
	out := compress(data, format: .raw_deflate, level: level)!
	back := decompress_raw_with_consumed(out)!
	assert back.decoded == data, 'level ${level} did not round-trip on ${data.len} bytes'
	assert back.consumed == out.len, 'level ${level} left ${out.len - back.consumed} bytes unread'
}

fn test_every_level_round_trips() {
	cases := {
		'empty':          []u8{}
		'one byte':       [u8(42)]
		'two bytes':      [u8(1), 2]
		'a run':          []u8{len: 1000, init: u8(65)}
		'text':           'the quick brown fox jumps over the lazy dog. '.repeat(200).bytes()
		'incompressible': incompressible(20000)
		'long match':     []u8{len: 600, init: u8(7)}
	}
	for name, data in cases {
		for level in 0 .. 10 {
			roundtrip(data, level) or { assert false, '${name}: ${err}' }
		}
	}
}

fn test_level_zero_stores() {
	// Level 0 must not compress at all: the output is the input plus a
	// header every 65535 bytes.
	data := 'compress me'.repeat(1000).bytes()
	out := compress(data, format: .raw_deflate, level: 0)!
	assert out.len >= data.len
	assert out.len <= data.len + 16
	// BFINAL=1, BTYPE=00 in the first byte.
	assert out[0] & 0x07 == 0x01
}

fn test_incompressible_data_barely_grows() {
	// A container hands already-compressed members to the encoder, and the
	// one thing it must never do is make them bigger. Without a stored
	// block to fall back on, this grows by about 5 %.
	data := incompressible(200_000)
	for level in [1, 6, 9] {
		out := compress(data, format: .raw_deflate, level: level)!
		overhead := f64(out.len - data.len) / f64(data.len)
		assert overhead < 0.001, 'level ${level} grew incompressible data by ${overhead * 100:.2f}%'
	}
}

fn test_higher_levels_do_not_compress_worse() {
	data := 'module main\nfn main() { println("hello") }\n'.repeat(500).bytes()
	mut previous := int(1) << 30
	for level in [1, 4, 6, 9] {
		out := compress(data, format: .raw_deflate, level: level)!
		assert out.len <= previous + previous / 20, 'level ${level} is much worse than the level before it'
		previous = out.len
	}
}

fn test_a_single_distinct_byte() {
	// One literal and one match symbol: both alphabets end up with fewer
	// than two used symbols, which is the case a Huffman code cannot express
	// and that several readers reject if it is written literally.
	data := []u8{len: 5000, init: u8(0xaa)}
	for level in [1, 6, 9] {
		roundtrip(data, level)!
	}
}

fn test_literals_only_leaves_the_distance_code_empty() {
	// Data with no repeat at all produces a block with no distance symbol,
	// so the distance alphabet has no used code.
	mut data := []u8{cap: 256}
	for i in 0 .. 256 {
		data << u8(i)
	}
	for level in [1, 6, 9] {
		roundtrip(data, level)!
	}
}

fn test_maximum_length_match() {
	// A 258 byte match is the longest the format can express, and has its
	// own code with no extra bits - encoding it as 284 plus extra bits is
	// the classic off-by-one and some decoders reject it.
	mut data := 'prefix'.bytes()
	data << []u8{len: 258, init: u8(3)}
	data << []u8{len: 258, init: u8(3)}
	data << 'suffix'.bytes()
	for level in [1, 6, 9] {
		roundtrip(data, level)!
	}
}

fn test_matches_across_a_block_boundary() {
	// Blocks close every 16 384 symbols; a match that starts before the
	// boundary and reaches past it must still resolve.
	mut data := []u8{cap: 200_000}
	mut state := u32(12345)
	for _ in 0 .. 40_000 {
		state = state * 1103515245 + 12345
		data << u8(state >> 16)
		data << u8(state >> 20)
	}
	// Repeat the whole thing so the second half is one long series of
	// matches reaching back across every block boundary.
	data << data.clone()
	for level in [1, 6, 9] {
		roundtrip(data, level)!
	}
}

fn test_invalid_level_is_refused() {
	if _ := compress([u8(1)], format: .raw_deflate, level: 10) {
		assert false, 'level 10 should not be accepted'
	} else {
		assert err.msg().contains('out of the range')
	}
	if _ := compress([u8(1)], format: .raw_deflate, level: -1) {
		assert false, 'level -1 should not be accepted'
	} else {
		assert err.msg().contains('out of the range')
	}
}

fn test_zlib_and_gzip_wrappers_carry_the_level() {
	data := 'wrap me up'.repeat(400).bytes()
	fast := compress(data, format: .gzip, level: 1)!
	best := compress(data, format: .gzip, level: 9)!
	assert best.len <= fast.len
	assert decompress(best)! == data
	assert decompress(compress(data, format: .zlib, level: 9)!)! == data
}

// incompressible builds `n` bytes with no exploitable structure, from a
// fixed seed so that a failure can be reproduced.
fn incompressible(n int) []u8 {
	mut out := []u8{cap: n}
	mut state := u64(0x2545_f491_4f6c_dd1d)
	for out.len < n {
		state ^= state << 13
		state ^= state >> 7
		state ^= state << 17
		out << u8(state >> 24)
	}
	return out
}

fn test_code_tables_match_the_base_tables() {
	for length in min_match .. max_match + 1 {
		mut want := 0
		for i := length_bases.len - 1; i >= 0; i-- {
			if length >= length_bases[i] {
				want = i
				break
			}
		}
		assert length_code(length) == want, 'length ${length}'
	}
	for dist in 1 .. window_size + 1 {
		assert dist_code(dist) == dist_code_of(dist), 'distance ${dist}'
	}
}

// kraft_sum returns the Kraft sum of `lens` in units of 2^-max_len: a complete
// prefix code sums to exactly 2^max_len, an over-subscribed one to more.
fn kraft_sum(lens []int, max_len int) u64 {
	mut total := u64(0)
	for l in lens {
		assert l <= max_len, 'length ${l} exceeds ${max_len}'
		if l > 0 {
			total += u64(1) << u64(max_len - l)
		}
	}
	return total
}

fn check_lengths(freq []u32, max_len int) ! {
	mut c := new_compressor(6)!
	mut lens := []int{len: freq.len}
	c.huffman_lengths(freq, max_len, mut lens)
	mut used := 0
	for i, f in freq {
		if f > 0 {
			used++
			assert lens[i] > 0, 'symbol ${i} has a frequency but no code'
		} else {
			assert lens[i] == 0, 'symbol ${i} has no frequency but a code'
		}
	}
	if used > 1 {
		assert kraft_sum(lens, max_len) == u64(1) << u64(max_len), 'incomplete or over-subscribed code for ${freq}'
	}
}

fn test_length_limiting_keeps_the_code_complete() {
	// These precode frequencies made the former repair, which counted
	// clamped leaves instead of the Kraft sum, produce an over-subscribed
	// code (sum 129/128) that zlib rejects.
	check_lengths([u32(34), 1, 6, 5, 13, 4, 5, 6, 130, 1, 24, 14, 1, 6, 1, 11, 14, 26, 10],
		max_precode_bits)!
}

fn test_length_limiting_on_skewed_frequencies() {
	// Fibonacci frequencies build the deepest possible tree, so they always
	// overrun the limit; every permutation must still give a complete code.
	mut fib := []u32{len: 30}
	fib[0] = 1
	fib[1] = 1
	for i in 2 .. fib.len {
		fib[i] = fib[i - 1] + fib[i - 2]
	}
	mut state := u32(7)
	for n in [19, 30] {
		for _ in 0 .. 200 {
			mut freq := fib[..n].clone()
			for i := n - 1; i > 0; i-- {
				state = state * 1103515245 + 12345
				j := int((state >> 16) % u32(i + 1))
				freq[i], freq[j] = freq[j], freq[i]
			}
			check_lengths(freq, max_precode_bits)!
			mut wide := []u32{len: num_litlen_codes}
			for i, f in freq {
				wide[i * 9] = f
			}
			check_lengths(wide, max_code_bits)!
		}
	}
}

fn test_zlib_header_carries_the_level_and_a_valid_check() {
	data := 'level'.repeat(100).bytes()
	for level in 0 .. 10 {
		z := compress(data, format: .zlib, level: level)!
		assert (u32(z[0]) * 256 + u32(z[1])) % 31 == 0, 'level ${level}: bad FCHECK'
		want := if level < 2 {
			0
		} else if level < 6 {
			1
		} else if level == 6 {
			2
		} else {
			3
		}
		assert int(z[1] >> 6) == want, 'level ${level}: FLEVEL ${z[1] >> 6}'
		assert decompress_zlib(z)! == data
	}
}

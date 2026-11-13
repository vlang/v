module bits

// The C backend computes len_16/32/64 with count-leading-zeros builtins; they
// must agree with the portable table versions for every u16, at every bit
// boundary and for a pseudo-random sample of wider values.
fn test_len_builtins_match_table_versions() {
	for x in 0 .. 65536 {
		assert len_16(u16(x)) == len_16_default(u16(x))
		assert len_32(u32(x)) == len_32_default(u32(x))
		assert len_64(u64(x)) == len_64_default(u64(x))
	}
	for shift in 0 .. 64 {
		p := u64(1) << shift
		for v in [p - 1, p, p + 1, p | (p >> 1), max_u64 >> shift] {
			assert len_64(v) == len_64_default(v), 'len_64(${v})'
			assert leading_zeros_64(v) == 64 - len_64_default(v)
			if shift < 32 {
				w := u32(v)
				assert len_32(w) == len_32_default(w), 'len_32(${w})'
				assert leading_zeros_32(w) == 32 - len_32_default(w)
			}
		}
	}
	mut state := u64(0x9e3779b97f4a7c15)
	for _ in 0 .. 10000 {
		state = state * 6364136223846793005 + 1442695040888963407
		v := state >> (state & 63)
		assert len_64(v) == len_64_default(v)
		assert len_32(u32(v)) == len_32_default(u32(v))
	}
	assert len_64(0) == 0 && len_32(0) == 0 && len_16(0) == 0
	assert len_64(max_u64) == 64 && len_32(max_u32) == 32 && len_16(max_u16) == 16
}

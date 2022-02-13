import net.conv

fn check<T>(f fn (a T) T, finv fn (b T) T, x T) {
	a := f(x)
	b := finv(a)
	assert b == x
	// eprintln('> x: ${x:10} | a: ${a:10} | b: ${b:10}')
	$if little_endian {
		assert a != b
	}
	$if big_endian {
		assert a == b
	}
}

fn test_htn64_nth64() {
	assert 0 == conv.htn64(0)
	assert 0 == conv.nth64(0)
	assert 0xFFFF_FFFF_FFFF_FFFF == conv.nth64(0xFFFF_FFFF_FFFF_FFFF)
	assert 0xFFFF_FFFF_FFFF_FFFF == conv.htn64(0xFFFF_FFFF_FFFF_FFFF)
	for x in [u64(1), 2, 128, 65536, 2147483648] {
		check(conv.htn64, conv.nth64, x)
	}
}

fn test_htn32_nth32() {
	assert 0 == conv.htn32(0)
	assert 0 == conv.nth32(0)
	assert 0xFFFF_FFFF == conv.nth32(0xFFFF_FFFF)
	assert 0x0101_0101 == conv.htn32(0x0101_0101)
	for x in [u32(1), 2, 128, 65536, 2147483648] {
		check(conv.htn32, conv.nth32, x)
	}
}

fn test_htn16_nth16() {
	assert 0 == conv.htn16(0)
	assert 0 == conv.nth16(0)
	assert 0xFFFF == conv.nth16(0xFFFF)
	assert 0x0101 == conv.htn16(0x0101)
	for x in [u16(1), 2, 128, 65534] {
		check(conv.htn16, conv.nth16, x)
	}
}

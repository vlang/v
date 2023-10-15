import net.conv

fn check[T](f fn (a T) T, finv fn (b T) T, x T) {
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

fn test_hton64_ntoh64() {
	assert 0 == conv.hton64(0)
	assert 0 == conv.ntoh64(0)
	assert 0xFFFF_FFFF_FFFF_FFFF == conv.ntoh64(0xFFFF_FFFF_FFFF_FFFF)
	assert 0xFFFF_FFFF_FFFF_FFFF == conv.hton64(0xFFFF_FFFF_FFFF_FFFF)
	for x in [u64(1), 2, 128, 65536, 2147483648] {
		check(conv.hton64, conv.ntoh64, x)
	}
}

fn test_hton32_ntoh32() {
	assert 0 == conv.hton32(0)
	assert 0 == conv.ntoh32(0)
	assert 0xFFFF_FFFF == conv.ntoh32(0xFFFF_FFFF)
	assert 0x0101_0101 == conv.hton32(0x0101_0101)
	for x in [u32(1), 2, 128, 65536, 2147483648] {
		check(conv.hton32, conv.ntoh32, x)
	}
}

fn test_hton16_ntoh16() {
	assert 0 == conv.hton16(0)
	assert 0 == conv.ntoh16(0)
	assert 0xFFFF == conv.ntoh16(0xFFFF)
	assert 0x0101 == conv.hton16(0x0101)
	for x in [u16(1), 2, 128, 65534] {
		check(conv.hton16, conv.ntoh16, x)
	}
}

fn test_varinttou64_u64tovarint() {
	b0 := conv.u64tovarint(0) or { panic(err) }
	assert b0 == [u8(0)]
	b1 := conv.u64tovarint(1) or { panic(err) }
	assert b1 == [u8(1)]
	mp := {
		u64(128):         [u8(0b01000000), 0b10000000]
		1024:             [u8(0b01000100), 0b00000000]
		0xffff:           [u8(0b10000000), 0, 0xff, 0xff]
		u64(1) << 62 - 1: [u8(0xff), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
	}
	for k, v in mp {
		println('${k:b}:${v}')
		n, len := conv.varinttou64(v) or { panic(err) }
		assert n == k
		rn := conv.u64tovarint(k) or { panic(err) }
		assert rn == v
	}
}

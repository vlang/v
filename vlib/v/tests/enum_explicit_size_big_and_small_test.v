enum SmallEnum as i8 {
	a = -1
	b = -4
	c
	d = 5
	e
}

enum BigEnum as u64 {
	a = 0xABCD_EF09_1234_5678
	b = 0xFFFF_FFFF_FFFF_FFF0
	c
	d = 5
	e
}

enum BigIEnum as i64 {
	a = -999_999_999_999
	b = -900_000_000_000
	c
	d = 900_000_000_000
	e
}

fn test_small_enum() {
	dump(sizeof(SmallEnum))
	$if tinyc {
		// TODO: TCC currently ignores `__attribute__((packed))` for enums, and uses an int instead, even with -fshort-enums :-|
		assert sizeof(SmallEnum) == 4
	} $else {
		assert sizeof(SmallEnum) == 1
	}
	dump(i8(SmallEnum.a))
	dump(i8(SmallEnum.b))
	dump(i8(SmallEnum.c))
	dump(i8(SmallEnum.d))
	dump(i8(SmallEnum.e))
	assert i8(SmallEnum.a) == -1
	assert i8(SmallEnum.b) == -4
	assert i8(SmallEnum.c) == -3
	assert i8(SmallEnum.d) == 5
	assert i8(SmallEnum.e) == 6
}

fn test_big_enum() {
	assert sizeof(BigIEnum) == 8
	assert BigEnum.a.str() == 'a'
	assert BigEnum.b.str() == 'b'
	assert BigEnum.c.str() == 'c'
	assert BigEnum.d.str() == 'd'
	assert BigEnum.e.str() == 'e'
	dump(u64(BigEnum.a))
	dump(u64(BigEnum.b))
	dump(u64(BigEnum.c))
	dump(u64(BigEnum.d))
	dump(u64(BigEnum.e))
	assert sizeof(BigEnum) == 8
	assert u64(BigEnum.a) == 0xABCD_EF09_1234_5678
	assert u64(BigEnum.b) == 0xFFFF_FFFF_FFFF_FFF0
	assert u64(BigEnum.c) == 0xFFFF_FFFF_FFFF_FFF1
	assert u64(BigEnum.d) == 5
	assert u64(BigEnum.e) == 6
}

fn test_big_ienum() {
	assert sizeof(BigIEnum) == 8
	assert BigIEnum.a.str() == 'a'
	assert BigIEnum.b.str() == 'b'
	assert BigIEnum.c.str() == 'c'
	assert BigIEnum.d.str() == 'd'
	assert BigIEnum.e.str() == 'e'
	dump(i64(BigIEnum.a))
	dump(i64(BigIEnum.b))
	dump(i64(BigIEnum.c))
	dump(i64(BigIEnum.d))
	dump(i64(BigIEnum.e))
	assert sizeof(BigIEnum) == 8
	assert i64(BigIEnum.a) == -999999999999
	assert i64(BigIEnum.b) == -900000000000
	assert i64(BigIEnum.c) == -899999999999
	assert i64(BigIEnum.d) == 900000000000
	assert i64(BigIEnum.e) == 900000000001
}

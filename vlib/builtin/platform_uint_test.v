module builtin

fn test_platform_uint_preserves_unsigned_range() {
	assert sizeof(uint) == sizeof(int)
	assert sizeof(uint) == sizeof(voidptr)
	maximum := ~uint(0)
	assert maximum > uint(max_int)
	assert maximum >> (sizeof(uint) * 8 - 1) == 1
	assert maximum + uint(1) == 0
	$if x64 || arm64 {
		assert maximum.str() == '18446744073709551615'
		assert uint(1) << 40 == u64(1099511627776)
	} $else $if x32 || arm32 {
		assert maximum.str() == '4294967295'
	}
	mut values := map[uint]int{}
	values[maximum] = -1
	assert values[maximum] == -1
}

fn test_platform_uint_pointer_round_trip() {
	value := 42
	address := uint(&value)
	// Recover and dereference the pointer from its integer address.
	unsafe {
		assert &int(address) == &value
		assert *(&int(address)) == value
	}
}

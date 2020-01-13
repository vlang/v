fn test_common_atoi() {
	// test common cases
	assert "70zzz".int() == 70
	assert "2901issue".int() == 2901
	assert '234232w'.int() == 234232
	assert '-9009x'.int() == -9009
	assert '0y'.int() == 0
	
	// test lead zeros
	assert '0000012'.int() == 12
	assert '-0000012'.int() == -12
	assert '0x001F'.int() == 31
	assert '-0x001F'.int() == -31
	assert '0x001f'.int() == 31
	assert '0o00011'.int() == 9
	assert '0b00001001'.int() == 9

	// test underscore in string
	assert '-10_000'.int() == -10000
	assert '-0x00_0_f_ff'.int() == -0xfff
	assert '10_000_000'.int() == 10000000

	for n in -10000 .. 100000 {
		s := n.str()+"z"
		assert s.int() == n
	}
}

fn test_unsigned_cast() {
	// tests for u16

    // test common cases
	assert "70zzz".u16() == 70
	assert "2901issue".u16() == 2901
	assert '0y'.u16() == 0
	
	// test lead zeros
	assert '0000012'.u16() == 12
	assert '0x001F'.u16() == 31
	assert '0x001f'.u16() == 31
	assert '0o00011'.u16() == 9
	assert '0b00001001'.u16() == 9

	// test underscore in string
	assert '-10_000'.u16() == -10000
	assert '-0x00_0_f_ff'.u16() == -0xfff

	for n in -10000 .. 100000 {
		s := n.str()+"z"
		assert s.u16() == n
	}

	// tests for u32

    // test common cases
	assert "70zzz".u32() == 70
	assert "2901issue".u32() == 2901
	assert '234232w'.u32() == 234232
	assert '-9009x'.u32() == -9009
	assert '0y'.u32() == 0
	
	// test lead zeros
	assert '0000012'.u32() == 12
	assert '-0000012'.u32() == -12
	assert '0x001F'.u32() == 31
	assert '-0x001F'.u32() == -31
	assert '0x001f'.u32() == 31
	assert '0o00011'.u32() == 9
	assert '0b00001001'.u32() == 9

	// test underscore in string
	assert '-10_000'.u32() == -10000
	assert '-0x00_0_f_ff'.u32() == -0xfff
	assert '10_000_000'.u32() == 10000000

	for n in -10000 .. 100000 {
		s := n.str()+"z"
		assert s.u32() == n
	}

	// tests for u64

    // test common cases
	assert "70zzz".u64() == 70
	assert "2901issue".u64() == 2901
	assert '234232w'.u64() == 234232
	assert '-9009x'.u64() == -9009
	assert '0y'.u64() == 0
	
	// test lead zeros
	assert '0000012'.u64() == 12
	assert '-0000012'.u64() == -12
	assert '0x001F'.u64() == 31
	assert '-0x001F'.u64() == -31
	assert '0x001f'.u64() == 31
	assert '0o00011'.u64() == 9
	assert '0b00001001'.u64() == 9

	// test underscore in string
	assert '-10_000'.u64() == -10000
	assert '-0x00_0_f_ff'.u64() == -0xfff
	assert '10_000_000'.u64() == 10000000

	for n in -10000 .. 100000 {
		s := n.str()+"z"
		assert s.u64() == n
	}
}

fn test_signed_cast() {
	// tests for i64

    // test common cases
	assert "70zzz".i64() == 70
	assert "2901issue".i64() == 2901
	assert '234232w'.i64() == 234232
	assert '-9009x'.i64() == -9009
	assert '0y'.i64() == 0
	
	// test lead zeros
	assert '0000012'.i64() == 12
	assert '-0000012'.i64() == -12
	assert '0x001F'.i64() == 31
	assert '-0x001F'.i64() == -31
	assert '0x001f'.i64() == 31
	assert '0o00011'.i64() == 9
	assert '0b00001001'.i64() == 9

	// test underscore in string
	assert '-10_000'.i64() == -10000
	assert '-0x00_0_f_ff'.i64() == -0xfff
	assert '10_000_000'.i64() == 10000000

	for n in -10000 .. 100000 {
		s := n.str()+"z"
		assert s.i64() == n
	}
}

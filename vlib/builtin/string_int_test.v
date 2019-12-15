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

fn test_common_atoi() {
	assert "70zzz".int() == 70
	assert "2901issue".int() == 2901
	assert '234232w'.int() == 234232
	assert '-9009x'.int() == -9009
	assert '0y'.int() == 0
	for n in -10000 .. 100000 {
		s := n.str()+"z"
		assert s.int() == n
	}
}

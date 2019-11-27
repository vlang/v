fn test_flexible_atoi() {
	assert '234232w'.int() == 234232
	assert '-9009x'.int() == -9009
	assert '0y'.int() == 0
	for n in -10000 .. 100000 {
		s := n.str()+"z"
		assert s.int() == n
	}
}

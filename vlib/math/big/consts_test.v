import math.big

fn test_named_consts() {
	assert big.zero_int == big.integer_from_int(0)
	assert big.one_int == big.integer_from_int(1)
	assert big.two_int == big.integer_from_int(2)
	assert big.three_int == big.integer_from_int(3)
}

fn test_indexed_consts() {
	assert big.c0 == big.integer_from_int(0)
	assert big.c1 == big.integer_from_int(1)
	assert big.c2 == big.integer_from_int(2)
	assert big.c3 == big.integer_from_int(3)
	assert big.c4 == big.integer_from_int(4)
	assert big.c5 == big.integer_from_int(5)
	assert big.c6 == big.integer_from_int(6)
	assert big.c7 == big.integer_from_int(7)
	assert big.c8 == big.integer_from_int(8)
	assert big.c9 == big.integer_from_int(9)
	assert big.c10 == big.integer_from_int(10)
	assert big.c11 == big.integer_from_int(11)
	assert big.c12 == big.integer_from_int(12)
	assert big.c13 == big.integer_from_int(13)
	assert big.c14 == big.integer_from_int(14)
	assert big.c15 == big.integer_from_int(15)
	assert big.c16 == big.integer_from_int(16)
	assert big.c17 == big.integer_from_int(17)
	assert big.c18 == big.integer_from_int(18)
	assert big.c19 == big.integer_from_int(19)
	assert big.c20 == big.integer_from_int(20)
}

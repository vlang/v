import math.big

fn test_dividing_big_numbers() {
	exponent := u32(249_999)
	// Preserve the operands 4 * 10^exponent and 2 * 10^exponent.
	// Build the power of two with a shift to keep the intermediate products smaller.
	base := big.integer_from_int(5).pow(exponent)
	a := base.left_shift(exponent + 2)
	b := base.left_shift(exponent + 1)
	assert a.bit_len() == 830481
	assert b.bit_len() == 830480
	c := a / b
	assert c.str() == '2'
	assert c.bit_len() == 2
}

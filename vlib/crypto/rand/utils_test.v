import math.big
import crypto.rand

fn test_int_big() {
	z := big.integer_from_int(0)
	rand.int_big(z) or { assert true }

	rand.int_big(big.integer_from_int(-1)) or { assert true }

	m := big.integer_from_int(1).left_shift(128)
	l := rand.int_big(m)! // actual large number
	assert l < m
	assert l > z
}

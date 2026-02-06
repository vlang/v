import math.big
import crypto.rand

fn test_int_big() {
	z := big.integer_from_int(0)
	if _ := rand.int_big(z) {
		assert false
	} else {
		assert true
	}

	n := big.integer_from_int(-1)
	if _ := rand.int_big(n) {
		assert false
	} else {
		assert true
	}

	m := big.integer_from_int(1).left_shift(128)
	l := rand.int_big(m)! // actual large number
	assert l < m
	assert l > n
}

import crypto.sha256
import math.big

fn test_exponentiation() {
	exponent := u32(66_777)
	result := big.integer_from_int(2).pow(exponent)
	assert result == big.integer_from_int(1).left_shift(exponent)

	decimal := result.str()
	assert decimal.len == 20_102
	// The hash covers every decimal digit. Generated with Python:
	// hashlib.sha256(str(1 << 66777).encode()).hexdigest()
	assert sha256.hexhash(decimal) == '52e360b1abe62e4949116fecc34f595d563ab39589e0b600bbc4a5bc49732732'
}

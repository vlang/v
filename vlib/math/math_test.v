import math

fn test_gcd() {
	assert math.gcd(6, 9) == 3
	assert math.gcd(6, -9) == 3
	assert math.gcd(-6, -9) == 3
	assert math.gcd(0, 0) == 0
}

fn test_lcm() {
	assert math.lcm(2, 3) == 6
	assert math.lcm(-2, 3) == 6
	assert math.lcm(-2, -3) == 6
	assert math.lcm(0, 0) == 0
}

fn test_digits() {
	digits_in_10th_base := math.digits(125, 10)
	assert digits_in_10th_base[0] == 5
	assert digits_in_10th_base[1] == 2
	assert digits_in_10th_base[2] == 1

	digits_in_16th_base := math.digits(15, 16)
	assert digits_in_16th_base[0] == 15

	negative_digits := math.digits(-4, 2)
	assert negative_digits[2] == -1
}

fn test_factorial() {
	assert math.factorial(5) == 120
	assert math.factorial(0) == 1
}

fn test_is_prime(){
     assert math.is_prime(-1) == false
     assert math.is_prime(0) == false
     assert math.is_prime(1) == false

     assert math.is_prime(2) == true
     assert math.is_prime(3) == true
     assert math.is_prime(17) == true

     assert math.is_prime(4) == false
     assert math.is_prime(9) == false
     assert math.is_prime(14) == false
}

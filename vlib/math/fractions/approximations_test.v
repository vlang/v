// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import math.fractions
import math

fn test_half() {
	float_val := 0.5
	fract_val := fractions.approximate(float_val)
	assert fract_val.equals(fractions.fraction(1, 2))
}

fn test_third() {
	float_val := 1.0 / 3.0
	fract_val := fractions.approximate(float_val)
	assert fract_val.equals(fractions.fraction(1, 3))
}

fn test_minus_one_twelfth() {
	float_val := -1.0 / 12.0
	fract_val := fractions.approximate(float_val)
	assert fract_val.equals(fractions.fraction(-1, 12))
}

fn test_zero() {
	float_val := 0.0
	println('Pre')
	fract_val := fractions.approximate(float_val)
	println('Post')
	assert fract_val.equals(fractions.fraction(0, 1))
}

fn test_minus_one() {
	float_val := -1.0
	fract_val := fractions.approximate(float_val)
	assert fract_val.equals(fractions.fraction(-1, 1))
}

fn test_thirty_three() {
	float_val := 33.0
	fract_val := fractions.approximate(float_val)
	assert fract_val.equals(fractions.fraction(33, 1))
}

fn test_millionth() {
	float_val := 1.0 / 1000000.0
	fract_val := fractions.approximate(float_val)
	assert fract_val.equals(fractions.fraction(1, 1000000))
}

fn test_minus_27_by_57() {
	float_val := -27.0 / 57.0
	fract_val := fractions.approximate(float_val)
	assert fract_val.equals(fractions.fraction(-27, 57))
}

fn test_29_by_104() {
	float_val := 29.0 / 104.0
	fract_val := fractions.approximate(float_val)
	assert fract_val.equals(fractions.fraction(29, 104))
}

fn test_140710_232() {
	float_val := 140710.232
	fract_val := fractions.approximate(float_val)
	// Approximation will match perfectly for upto 3 places after the decimal
	// The result will be within default_eps of original value
	assert fract_val.f64() == float_val
}

fn test_pi_1_digit() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-2).equals(fractions.fraction(22, 7))
}

fn test_pi_2_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-3).equals(fractions.fraction(22, 7))
}

fn test_pi_3_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-4).equals(fractions.fraction(333, 106))
}

fn test_pi_4_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-5).equals(fractions.fraction(355, 113))
}

fn test_pi_5_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-6).equals(fractions.fraction(355, 113))
}

fn test_pi_6_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-7).equals(fractions.fraction(355, 113))
}

fn test_pi_7_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-8).equals(fractions.fraction(103993,
		33102))
}

fn test_pi_8_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-9).equals(fractions.fraction(103993,
		33102))
}

fn test_pi_9_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-10).equals(fractions.fraction(104348,
		33215))
}

fn test_pi_10_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-11).equals(fractions.fraction(312689,
		99532))
}

fn test_pi_11_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-12).equals(fractions.fraction(1146408,
		364913))
}

fn test_pi_12_digits() {
	assert fractions.approximate_with_eps(math.pi, 5.0e-13).equals(fractions.fraction(4272943,
		1360120))
}

fn test_phi_1_digit() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-2).equals(fractions.fraction(5, 3))
}

fn test_phi_2_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-3).equals(fractions.fraction(21, 13))
}

fn test_phi_3_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-4).equals(fractions.fraction(55, 34))
}

fn test_phi_4_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-5).equals(fractions.fraction(233,
		144))
}

fn test_phi_5_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-6).equals(fractions.fraction(610,
		377))
}

fn test_phi_6_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-7).equals(fractions.fraction(1597,
		987))
}

fn test_phi_7_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-8).equals(fractions.fraction(6765,
		4181))
}

fn test_phi_8_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-9).equals(fractions.fraction(17711,
		10946))
}

fn test_phi_9_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-10).equals(fractions.fraction(75025,
		46368))
}

fn test_phi_10_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-11).equals(fractions.fraction(196418,
		121393))
}

fn test_phi_11_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-12).equals(fractions.fraction(514229,
		317811))
}

fn test_phi_12_digits() {
	assert fractions.approximate_with_eps(math.phi, 5.0e-13).equals(fractions.fraction(2178309,
		1346269))
}

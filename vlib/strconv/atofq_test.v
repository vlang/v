import strconv
import math

fn test_atof_quick_parses_signed_decimals() {
	assert strconv.atof_quick('1.5') == 1.5
	assert strconv.atof_quick('-1.5') == -1.5
	assert strconv.atof_quick('+2.25') == 2.25
	assert strconv.atof_quick('.5') == 0.5
}

fn test_atof_quick_skips_leading_spaces() {
	assert strconv.atof_quick('  3.5') == 3.5
}

fn test_atof_quick_parses_zero() {
	assert strconv.atof_quick('0') == 0.0
	assert strconv.atof_quick('-0') == -0.0
}

fn test_atof_quick_parses_exponents() {
	assert strconv.atof_quick('1e3') == 1000.0
	assert strconv.atof_quick('1E3') == 1000.0
	assert strconv.atof_quick('1.5e-2') == 0.015
}

fn test_atof_quick_parses_signed_infinity() {
	assert strconv.atof_quick('inf') == math.inf(1)
	assert strconv.atof_quick('-inf') == math.inf(-1)
}

fn test_atof_quick_ignores_trailing_garbage() {
	// documented limitation: parsing stops at the first unexpected byte
	assert strconv.atof_quick('42abc') == 42.0
}

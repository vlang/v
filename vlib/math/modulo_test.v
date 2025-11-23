import math

fn test_modulo() {
	assert math.modulo_euclid(5, 1) == 0
	assert math.modulo_floored(5, 1) == 0
	assert math.modulo_truncated(5, 1) == 0
	assert math.modulo_euclid(5, 2) == 1
	assert math.modulo_floored(5, 2) == 1
	assert math.modulo_truncated(5, 2) == 1
	assert math.modulo_euclid(5, 3) == 2
	assert math.modulo_floored(5, 3) == 2
	assert math.modulo_truncated(5, 3) == 2
	assert math.modulo_euclid(5, 4) == 1
	assert math.modulo_floored(5, 4) == 1
	assert math.modulo_truncated(5, 4) == 1
	assert math.modulo_euclid(5, 5) == 0
	assert math.modulo_floored(5, 5) == 0
	assert math.modulo_truncated(5, 5) == 0
	assert math.modulo_euclid(-5, 1) == 0
	assert math.modulo_floored(-5, 1) == 0
	assert math.modulo_truncated(-5, 1) == 0
	assert math.modulo_euclid(-5, 2) == 1
	assert math.modulo_floored(-5, 2) == 1
	assert math.modulo_truncated(-5, 2) == -1
	assert math.modulo_euclid(-5, 3) == 1
	assert math.modulo_floored(-5, 3) == 1
	assert math.modulo_truncated(-5, 3) == -2
	assert math.modulo_euclid(-5, 4) == 3
	assert math.modulo_floored(-5, 4) == 3
	assert math.modulo_truncated(-5, 4) == -1
	assert math.modulo_euclid(-5, 5) == 0
	assert math.modulo_floored(-5, 5) == 0
	assert math.modulo_truncated(-5, 5) == 0
	assert math.modulo_euclid(1, 5) == 1
	assert math.modulo_floored(1, 5) == 1
	assert math.modulo_truncated(1, 5) == 1
	assert math.modulo_euclid(2, 5) == 2
	assert math.modulo_floored(2, 5) == 2
	assert math.modulo_truncated(2, 5) == 2
	assert math.modulo_euclid(3, 5) == 3
	assert math.modulo_floored(3, 5) == 3
	assert math.modulo_truncated(3, 5) == 3
	assert math.modulo_euclid(4, 5) == 4
	assert math.modulo_floored(4, 5) == 4
	assert math.modulo_truncated(4, 5) == 4
	assert math.modulo_euclid(-1, 5) == 4
	assert math.modulo_floored(-1, 5) == 4
	assert math.modulo_truncated(-1, 5) == -1
	assert math.modulo_euclid(-2, 5) == 3
	assert math.modulo_floored(-2, 5) == 3
	assert math.modulo_truncated(-2, 5) == -2
	assert math.modulo_euclid(-3, 5) == 2
	assert math.modulo_floored(-3, 5) == 2
	assert math.modulo_truncated(-3, 5) == -3
	assert math.modulo_euclid(-4, 5) == 1
	assert math.modulo_floored(-4, 5) == 1
	assert math.modulo_truncated(-4, 5) == -4
	assert math.modulo_euclid(-1, -5) == 4
	assert math.modulo_floored(-1, -5) == -1
	assert math.modulo_truncated(-1, -5) == -1
	assert math.modulo_euclid(-2, -5) == 3
	assert math.modulo_floored(-2, -5) == -2
	assert math.modulo_truncated(-2, -5) == -2
	assert math.modulo_euclid(-3, -5) == 2
	assert math.modulo_floored(-3, -5) == -3
	assert math.modulo_truncated(-3, -5) == -3
	assert math.modulo_euclid(-4, -5) == 1
	assert math.modulo_floored(-4, -5) == -4
	assert math.modulo_truncated(-4, -5) == -4
}

fn test_divide_both_positive() {
	assert math.divide_euclid(10, 2) == math.DivResult[int]{5, 0}
	assert math.divide_floored(10, 2) == math.DivResult[int]{5, 0}
	assert math.divide_truncated(10, 2) == math.DivResult[int]{5, 0}

	assert math.divide_euclid(10, 3) == math.DivResult[int]{3, 1}
	assert math.divide_floored(10, 3) == math.DivResult[int]{3, 1}
	assert math.divide_truncated(10, 3) == math.DivResult[int]{3, 1}

	assert math.divide_euclid(10, 5) == math.DivResult[int]{2, 0}
	assert math.divide_floored(10, 5) == math.DivResult[int]{2, 0}
	assert math.divide_truncated(10, 5) == math.DivResult[int]{2, 0}

	assert math.divide_euclid(10, 7) == math.DivResult[int]{1, 3}
	assert math.divide_floored(10, 7) == math.DivResult[int]{1, 3}
	assert math.divide_truncated(10, 7) == math.DivResult[int]{1, 3}
}

fn test_divide_positive_divident_and_negative_divisor() {
	assert math.divide_euclid(10, -2) == math.DivResult[int]{-5, 0}
	assert math.divide_floored(10, -2) == math.DivResult[int]{-5, 0}
	assert math.divide_truncated(10, -2) == math.DivResult[int]{-5, 0}

	assert math.divide_euclid(10, -3) == math.DivResult[int]{-3, 1}
	assert math.divide_floored(10, -3) == math.DivResult[int]{-4, -2}
	assert math.divide_truncated(10, -3) == math.DivResult[int]{-3, 1}

	assert math.divide_euclid(10, -5) == math.DivResult[int]{-2, 0}
	assert math.divide_floored(10, -5) == math.DivResult[int]{-2, 0}
	assert math.divide_truncated(10, -5) == math.DivResult[int]{-2, 0}

	assert math.divide_euclid(10, -7) == math.DivResult[int]{-1, 3}
	assert math.divide_floored(10, -7) == math.DivResult[int]{-2, -4}
	assert math.divide_truncated(10, -7) == math.DivResult[int]{-1, 3}
}

fn test_divide_negative_divident_and_positive_divisor() {
	assert math.divide_euclid(-10, 2) == math.DivResult[int]{-5, 0}
	assert math.divide_floored(-10, 2) == math.DivResult[int]{-5, 0}
	assert math.divide_truncated(-10, 2) == math.DivResult[int]{-5, 0}

	assert math.divide_euclid(-10, 3) == math.DivResult[int]{-4, 2}
	assert math.divide_floored(-10, 3) == math.DivResult[int]{-4, 2}
	assert math.divide_truncated(-10, 3) == math.DivResult[int]{-3, -1}

	assert math.divide_euclid(-10, 5) == math.DivResult[int]{-2, 0}
	assert math.divide_floored(-10, 5) == math.DivResult[int]{-2, 0}
	assert math.divide_truncated(-10, 5) == math.DivResult[int]{-2, 0}

	assert math.divide_euclid(-10, 7) == math.DivResult[int]{-2, 4}
	assert math.divide_floored(-10, 7) == math.DivResult[int]{-2, 4}
	assert math.divide_truncated(-10, 7) == math.DivResult[int]{-1, -3}
}

fn test_divide_both_negative() {
	assert math.divide_euclid(-10, -2) == math.DivResult[int]{5, 0}
	assert math.divide_floored(-10, -2) == math.DivResult[int]{5, 0}
	assert math.divide_truncated(-10, -2) == math.DivResult[int]{5, 0}

	assert math.divide_euclid(-10, -3) == math.DivResult[int]{4, 2}
	assert math.divide_floored(-10, -3) == math.DivResult[int]{3, -1}
	assert math.divide_truncated(-10, -3) == math.DivResult[int]{3, -1}

	assert math.divide_euclid(-10, -5) == math.DivResult[int]{2, 0}
	assert math.divide_floored(-10, -5) == math.DivResult[int]{2, 0}
	assert math.divide_truncated(-10, -5) == math.DivResult[int]{2, 0}

	assert math.divide_euclid(-10, -7) == math.DivResult[int]{2, 4}
	assert math.divide_floored(-10, -7) == math.DivResult[int]{1, -3}
	assert math.divide_truncated(-10, -7) == math.DivResult[int]{1, -3}
}

import math

fn test_min() {
	assert math.min(42, 13) == 13
	assert math.min(5, -10) == -10
	assert math.min(7.1, 7.3) == 7.1
	assert math.min(u32(32), u32(17)) == 17
}

fn test_max() {
	assert math.max(42, 13) == 42
	assert math.max(5, -10) == 5
	assert math.max(7.1, 7.3) == 7.3
	assert math.max(u32(60), u32(17)) == 60
}

fn test_abs() {
	assert math.abs(99) == 99
	assert math.abs(-10) == 10
	assert math.abs(1.2345) == 1.2345
	assert math.abs(-5.5) == 5.5
}

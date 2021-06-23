import math.mathutil as mu

fn test_min() {
	assert mu.min(42, 13) == 13
	assert mu.min(5, -10) == -10
	assert mu.min(7.1, 7.3) == 7.1
	assert mu.min(u32(32), u32(17)) == 17
}

fn test_max() {
	assert mu.max(42, 13) == 42
	assert mu.max(5, -10) == 5
	assert mu.max(7.1, 7.3) == 7.3
	assert mu.max(u32(60), u32(17)) == 60
}

fn test_abs() {
	assert mu.abs(99) == 99
	assert mu.abs(-10) == 10
	assert mu.abs(1.2345) == 1.2345
	assert mu.abs(-5.5) == 5.5
}

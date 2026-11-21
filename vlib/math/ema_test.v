module math

fn test_ema() {
	v := ema([1.0, 2.0, 3.0], 0.5)
	assert v.len == 3
	assert v[0] == 1.0
	assert v[1] == 1.5
	assert v[2] == 2.25
	assert ema([], 0.5).len == 0
	assert ema([4.0], 0.5) == [4.0]
}

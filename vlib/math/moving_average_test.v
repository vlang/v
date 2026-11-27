module math

fn test_moving_average() {
	v := moving_average([1.0, 2.0, 3.0], 2)
	assert v.len == 3
}

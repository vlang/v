module math

fn test_dema() {
	v := dema([1.0, 2.0, 3.0], 0.5)
	assert v.len == 3
}

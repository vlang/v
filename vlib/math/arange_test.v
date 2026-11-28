module math

fn test_arange() {
	v := arange(0, 5, 1)
	assert v == [0.0, 1.0, 2.0, 3.0, 4.0]
	assert arange(5, 0, -2) == [5.0, 3.0, 1.0]
	assert arange(0, 1, 0) == []
}

fn test_arange_floating_point_stagnation() {
	// At 1e20 the spacing between adjacent f64 values is much larger than the
	// step, so v + step rounds back to v. The call must terminate.
	v := arange(1e20, 1e20 + 1e5, 1.0)
	assert v.len == 1
	assert v[0] == 1e20
}

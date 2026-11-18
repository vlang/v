module math

fn test_geomspace() {
	v := geomspace(1, 100, 3)
	assert v.len == 3
	assert v[0] == 1
	assert v[2] == 100
}

fn test_geomspace_huge_ratio_stays_finite() {
	v := geomspace(1e-300, 1e300, 3)
	assert v.len == 3
	assert v[0] == 1e-300
	assert v[1] > 0.999999999 && v[1] < 1.000000001
	assert v[2] == 1e300
}

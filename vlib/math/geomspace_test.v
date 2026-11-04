module math

fn test_geomspace() {
	v := geomspace(1, 100, 3)
	assert v.len == 3
	assert v[0] == 1
	assert v[2] == 100
}

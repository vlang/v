module math

fn test_logspace() {
	v := logspace(0, 2, 3)
	assert v.len == 3
	assert v[0] == 1
	assert v[2] == 100
}

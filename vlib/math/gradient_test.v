module math

fn test_gradient() {
	assert gradient([1.0]) == []
	assert gradient([0.0, 1.0]) == [1.0, 1.0]
	v := gradient([0.0, 1.0, 4.0, 9.0])
	assert v.len == 4
	assert v[0] == 1.0
	assert v[1] == 2.0
	assert v[2] == 4.0
	assert v[3] == 5.0
}

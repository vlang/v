module math

fn test_linspace() {
	v := linspace(0, 10, 5)
	assert v.len == 5
	assert v[0] == 0
	assert v[4] == 10
	assert v[2] == 5
	assert linspace(3, 3, 4) == [3.0, 3.0, 3.0, 3.0]
	assert linspace(0, 1, 1) == [0.0]
	assert linspace(0, 1, 0) == []
}

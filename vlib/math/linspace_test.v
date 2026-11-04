module math

fn test_linspace() {
	v := linspace(0, 10, 5)
	assert v.len == 5
	assert v[0] == 0
	assert v[4] == 10
}

module math

fn test_linspace() {
	v := linspace(0, 10, 5)
	assert v.len == 5
	assert v[0] == 0
	assert v[4] == 10
}

fn test_linspace_huge_span_stays_finite() {
	v := linspace(-1e308, 1e308, 3)
	assert v.len == 3
	assert v[0] == -1e308
	assert v[1] == 0.0
	assert v[2] == 1e308
}

module math

fn test_arange() {
	v := arange(0, 5, 1)
	assert v == [0.0, 1.0, 2.0, 3.0, 4.0]
}

module math

fn test_wma() {
	v := wma([1.0, 2.0, 3.0], [1.0, 1.0])
	assert v.len == 2
}

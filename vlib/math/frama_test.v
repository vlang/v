module math

fn test_frama() {
	v := frama([1.0, 2.0, 3.0], 2)
	assert v.len == 3
}

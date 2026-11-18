module math

fn test_frama() {
	v := frama([1.0, 2.0, 3.0], 2)
	assert v.len == 3
}

fn test_frama_long_window_no_oob() {
	vals := [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]
	v := frama(vals, 2)
	assert v.len == vals.len
	for x in v {
		assert x == x
	}
}

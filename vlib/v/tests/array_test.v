fn test_for_in_array_named_array() {
	mut array := [1]
	for elem in array {
		assert elem == 1
	}
	for mut elem in array {
		assert *elem == 1
		elem = 2
		assert *elem == 2
	}
}

fn test_for_in_shared_array_named_array() {
	shared array := &[1]
	rlock array {
		for elem in array {
			assert elem == 1
		}
	}
}

fn test_fixed_array_to_dynamic_array() {
	y := [1, 2, 3]!
	mut x := y[..]
	x << 4
	assert x.len == 4
}

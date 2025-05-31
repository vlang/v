fn test_main() {
	encode([]int{len: 5, init: 5})
}

fn encode[U](val U) {
	new_val := val.clone()
	assert new_val == [5, 5, 5, 5, 5]
}

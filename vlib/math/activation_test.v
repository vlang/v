module math

fn test_activation() {
	assert relu(-1) == 0
	assert relu(2) == 2
	s := softmax_slice([1.0, 2.0])
	assert s.len == 2
}

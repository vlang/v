module math

fn test_activation() {
	assert abs(sigmoid(0) - 0.5) < 0.000001
	assert sigmoid(20) > 0.999999
	assert sigmoid(-20) < 0.000001
	assert relu(-3) == 0
	assert relu(3) == 3
	assert softmax_slice([]) == []
	s := softmax_slice([0.0, 0.0])
	assert s.len == 2
	assert abs(s[0] - 0.5) < 0.000001
	assert abs(s[0] + s[1] - 1.0) < 0.000001
}

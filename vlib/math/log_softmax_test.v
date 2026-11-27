module math

fn test_log_softmax() {
	v := log_softmax([1.0, 2.0])
	assert v.len == 2
}

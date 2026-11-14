module math

fn test_relu6() {
	assert relu6(-1) == 0
	assert relu6(3) == 3
	assert relu6(10) == 6
}

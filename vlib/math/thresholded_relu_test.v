module math

fn test_thresholded_relu() {
	assert thresholded_relu(0.5, 1.0) == 0
	assert thresholded_relu(2.0, 1.0) == 2.0
}

module math

fn test_rms() {
	assert math.abs(rms([3.0, 4.0]) - 5.0 / math.sqrt(2.0)) < 1e-9
}

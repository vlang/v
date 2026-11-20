module math

fn test_log_sigmoid() {
	assert log_sigmoid(0) == -math.log(2.0)
}

fn test_log_sigmoid_large_stays_finite() {
	assert log_sigmoid(-1000) == -1000.0
	assert log_sigmoid(1000) == 0.0
}

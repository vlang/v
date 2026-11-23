module math

fn test_interp() {
	assert interp(1.5, [0.0, 2.0], [0.0, 2.0]) == 1.5
	assert interp(-1.0, [0.0, 2.0], [10.0, 20.0]) == 10.0
	assert interp(9.0, [0.0, 2.0], [10.0, 20.0]) == 20.0
	assert interp(1.0, [0.0, 0.0], [3.0, 4.0]) == 4.0
	assert interp(0.5, [], []) == 0
	assert interp(0.5, [0.0], [1.0, 2.0]) == 0
}

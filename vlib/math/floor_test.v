import math

fn test_round_to_even() {
	assert math.round_to_even(0.123) == 0.0
	assert math.round_to_even(123.12345) == 123.00
}

fn test_rount_to_even_edge() {
	vrounds_ := [f64(-0.0), 0.0, -0.5, 0.5, -1.0, 1.0, 0.12345, 1.2345, 12.345, 123.45, 1234.5,
		12345.0, -math.pi, math.pi, math.inf(-1), math.inf(1),
		math.nan()]
	rounds_ := [f64(0.0), 0.0, -0.0, 0.0, -1.0, 1.0, 0.0, 1.0, 12.0, 123.0, 1234.0, 12345, -3.0,
		3.0, math.inf(-1), math.inf(1), math.nan()]
	for i, v in vrounds_ {
		assert math.alike(math.round_to_even(v), rounds_[i])
	}
}

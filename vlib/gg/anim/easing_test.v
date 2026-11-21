module anim

fn test_ease_endpoints() {
	assert ease(0, .linear) == 0
	assert ease(1, .linear) == 1
	assert ease(0.5, .linear) == 0.5
	assert ease(-1, .linear) == 0
	assert ease(2, .linear) == 1
	assert ease(0, .ease_in_elastic) == 0
	assert ease(1, .ease_out_elastic) == 1
	assert ease(0, .ease_in_bounce) == 0
	assert ease(1, .ease_out_bounce) == 1
	assert ease(1, .ease_in_out_back) == 1
}

fn test_ease_curves() {
	assert ease(0.5, .ease_in_quad) - 0.25 < 0.0001
	assert ease(0.5, .ease_out_quad) - 0.75 < 0.0001
	assert ease(0.5, .ease_in_out_cubic) - 0.5 < 0.0001
	assert ease(0.5, .ease_in_sine) - 0.292893 < 0.0001
	assert ease(0.25, .ease_out_bounce) > ease(0.25, .linear)
}

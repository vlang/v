module sim

fn test_add() {
	v := vector(
		x: -0.016957230930171364
		y: -0.02937078552673521
		z: 0.002311063475327252
	)
	result := v + v
	expected := vector(
		x: -0.03391446186034273
		y: -0.05874157105347042
		z: 0.004622126950654504
	)
	assert result == expected
}

fn test_dot() {
	v := vector(
		x: -0.016957230930171364
		y: -0.02937078552673521
		z: 0.002311063475327252
	)
	result := v * v
	expected := 0.0011555317376636305
	assert result == expected
}

fn test_scale() {
	v := vector(
		x: -0.016957230930171364
		y: -0.02937078552673521
		z: 0.002311063475327252
	)
	result := v.scale(2.0)
	expected := vector(
		x: -0.03391446186034273
		y: -0.05874157105347042
		z: 0.004622126950654504
	)
	assert result == expected
}

fn test_norm_squared() {
	v := vector(
		x: -0.016957230930171364
		y: -0.02937078552673521
		z: 0.002311063475327252
	)
	result := v.norm_squared()
	expected := 0.0011555317376636305
	assert result == expected
}

fn test_norm() {
	v := vector(
		x: -0.016957230930171364
		y: -0.02937078552673521
		z: 0.002311063475327252
	)
	result := v.norm()
	expected := 0.033993113091678295
	assert result == expected
}

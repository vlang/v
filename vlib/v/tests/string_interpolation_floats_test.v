fn test_f32_widths_and_precision() {
	x := f32(200.43)
	assert '|${x:10.4f}|' == '|  200.4300|'
	assert '|${x:-10.4f}|' == '|200.4300  |'
	assert '|${x:10.0f}|' == '|       200|'
	assert '|${x:-10.0f}|' == '|200       |'

	assert '|${x:0.4f}|' == '|200.4300|'
	assert '|${x:.3f}|' == '|200.430|'
	assert '|${x:.0f}|' == '|200|'

	y := f32(200.90)
	assert '|${y:.0f}|' == '|201|'
}

fn test_f64_widths_and_precision() {
	x := f64(200.43)
	assert '|${x:10.4f}|' == '|  200.4300|'
	assert '|${x:-10.4f}|' == '|200.4300  |'
	assert '|${x:10.0f}|' == '|       200|'
	assert '|${x:-10.0f}|' == '|200       |'

	assert '|${x:0.4f}|' == '|200.4300|'
	assert '|${x:.3f}|' == '|200.430|'
	assert '|${x:.0f}|' == '|200|'

	y := f64(200.90)
	assert '|${y:.0f}|' == '|201|'
}

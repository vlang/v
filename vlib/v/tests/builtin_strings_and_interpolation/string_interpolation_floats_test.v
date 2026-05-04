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

fn test_f64_widths_and_large_precision_for_small_negative_value() {
	x := -0.68849533748148505907238359213806688785552978515625
	assert '|${x:54.40f}|' == '|           -0.6884953374814851000000000000000000000000|'
}

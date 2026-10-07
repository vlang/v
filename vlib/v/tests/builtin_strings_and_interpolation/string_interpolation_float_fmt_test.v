fn test_string_interpolation_float_fmt() {
	mut a := 76.295
	eprintln('${a:8.2}')
	assert '${a:8.2}' == '    76.3'
	eprintln('${a:8.2f}')
	assert '${a:8.2f}' == '   76.30'

	a = 76.296
	eprintln('${a:8.2}')
	assert '${a:8.2}' == '    76.3'
	eprintln('${a:8.2f}')
	assert '${a:8.2f}' == '   76.30'
}

fn test_string_interpolation_float_precision_without_type_letter() {
	x := 3.14159
	assert '${x:.1} ${x:.2} ${x:.3} ${x:.4}' == '3.1 3.14 3.142 3.1416'
	assert '${x:.2f}' == '3.14'
	y := 2.5
	assert '${y:.0} ${y:.1} ${y:.3}' == '3 2.5 2.5'
	z := 1234.5678
	assert '[${z:.2}] [${z:8.2}] [${z:-9.1}]' == '[1234.57] [ 1234.57] [1234.6   ]'
	assert '${f32(x):.2} ${f32(0.1):.9}' == '3.14 0.100000001'
	// trailing zeros (and a bare dot) are trimmed
	hundred := 100.0
	assert '${hundred:.1} ${hundred:.0} ${503.6:.2}' == '100 100 503.6'
	zero := 0.0
	neg := -0.004
	assert '${zero:.2} ${neg:.2} ${-2.5:.3}' == '0 -0 -2.5'
	// outside [1e-5, 999999) the value is printed in exponent form with precision-1 decimals
	big := -123456789.5
	tiny := 1e-7
	huge := 1e20
	assert '${big:.3} ${tiny:.4} ${huge:.2}' == '-1.23e+08 1e-07 1e+20'
}

fn test_string_interpolation_float_precision_preserves_negative_zero() {
	negative := -0.0
	positive := 0.0
	assert '${negative:.0} ${negative:.1} ${negative:.3}' == '-0 -0 -0'
	assert '${positive:.0} ${positive:.1} ${positive:.3}' == '0 0 0'
	assert '[${negative:5.2}] [${negative:-5.2}]' == '[   -0] [-0   ]'
	negative32 := f32(negative)
	positive32 := f32(positive)
	assert '${negative32:.0} ${negative32:.3}' == '-0 -0'
	assert '${positive32:.0} ${positive32:.3}' == '0 0'
	assert '[${negative32:5.2}] [${negative32:-5.2}]' == '[   -0] [-0   ]'
}

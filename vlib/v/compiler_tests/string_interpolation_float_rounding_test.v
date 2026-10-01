fn test_fixed_float_interpolation_rounds_half_away_from_zero_at_any_precision() {
	assert '${6.25:.1f}' == '6.3'
	assert '${-6.25:.1f}' == '-6.3'
	assert '${f32(-0.526):.3f}' == '-0.526'
	assert '${f64(-1.234):.3f}' == '-1.234'
}

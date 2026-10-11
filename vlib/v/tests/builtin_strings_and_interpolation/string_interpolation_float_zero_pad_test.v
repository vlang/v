import math

fn test_string_interpolation_float_zero_pad() {
	x := 3.14159
	n := -3.14159
	assert '${x:08.2f}' == '00003.14'
	assert '${n:08.2f}' == '-0003.14'
	assert '${x:010.3f}' == '000003.142'
	assert '${x:08.0f}' == '00000003'
	assert '${n:08.0f}' == '-0000003'
	assert '${-0.5:07.1f}' == '-0000.5'
	zero := 0.0
	assert '${zero:06.2f}' == '000.00'
	// a width smaller than the number is no padding
	big := 123456.789
	assert '${big:04.2f}' == '123456.79'
	// without the `0` flag the padding stays spaces
	assert '${x:8.2f}' == '    3.14'
}

fn test_string_interpolation_f32_zero_pad() {
	y := f32(3.14159)
	m := f32(-2.5)
	assert '${y:08.2f}' == '00003.14'
	assert '${m:08.2f}' == '-0002.50'
}

fn test_string_interpolation_float_zero_pad_left_align_and_non_finite() {
	x := 3.14159
	// `-` overrides `0`: the padding goes on the right, as spaces
	assert '${x:-08.2f}' == '3.14    '
	// inf and nan are space-padded, as in C
	assert '${math.inf(1):08.2f}' == '    +inf'
	assert '${math.inf(-1):08.2f}' == '    -inf'
	assert '${math.nan():08.2f}' == '     nan'
}

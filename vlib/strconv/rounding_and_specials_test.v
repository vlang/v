import strconv

// Regression tests for three stdlib defects fixed together. Each assertion
// below fails on master; the comment on each names the defect.

// fxx_to_str_l_parse and fxx_to_str_l_parse_with_dot must pass through every
// special value, not just the signed ones. The guard used to be
// `s[0] == `n` || s[1] == `i``, which catches nan, +inf and -inf but not `inf`
// because its `i` sits at index 0, so it fell into the digit parser and produced
// 'Float conversion error!!'.
fn test_fxx_to_str_l_passes_through_the_unsigned_inf() {
	for input in ['inf', '+inf', '-inf', 'nan'] {
		got := strconv.fxx_to_str_l_parse(input)
		got_dot := strconv.fxx_to_str_l_parse_with_dot(input)
		assert got == input, 'parse(${input}) = ${got}'
		assert got_dot == input, 'parse_with_dot(${input}) = ${got_dot}'
	}
}

// f64_to_str_lnd1 adds a rounding delta before truncating, so the delta must
// move away from zero. Added unconditionally it moved negative values toward
// zero and the truncation then produced the wrong magnitude.
fn test_f64_to_str_lnd1_rounds_negatives_by_magnitude() {
	assert strconv.f64_to_str_lnd1(-12.345, 0) == '-12'
	assert strconv.f64_to_str_lnd1(-1.5, 0) == '-2'
	assert strconv.f64_to_str_lnd1(-0.5, 0) == '-1'
	assert strconv.f64_to_str_lnd1(-1.4, 0) == '-1'
	assert strconv.f64_to_str_lnd1(-0.4, 0) == '-0'
	assert strconv.f64_to_str_lnd1(-123.456, 2) == '-123.46'
	// positives and zero are unchanged
	assert strconv.f64_to_str_lnd1(12.345, 0) == '12'
	assert strconv.f64_to_str_lnd1(1.5, 0) == '2'
	assert strconv.f64_to_str_lnd1(0.0, 0) == '0'
}

// The rounding must not disturb the trailing-'.0' behaviour of the _with_dot
// family, which shares this code path.
fn test_fxx_to_str_l_with_dot_still_appends_dot() {
	assert strconv.f64_to_str_l_with_dot(-12.5) == '-12.5'
	assert strconv.f64_to_str_l_with_dot(34.7) == '34.7'
	assert strconv.f64_to_str_l_with_dot(0.0) == '0.0'
}

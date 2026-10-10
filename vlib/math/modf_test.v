module math

// modf splits f into an integer part and a fractional part that sum back to f, with the
// integer part truncated towards zero.
fn test_modf_splits_finite_values() {
	i, f := modf(1.5)
	assert i == 1.0
	assert f == 0.5
	zi, zf := modf(0.5)
	assert zi == 0.0
	assert zf == 0.5
	wi, wf := modf(3.0)
	assert wi == 3.0
	assert wf == 0.0
	qi, qf := modf(2.75)
	assert qi == 2.0
	assert qf == 0.75
	bi, bf := modf(123.456)
	assert bi == 123.0
	assert bf == 0.45600000000000307
	// A whole number has a zero fraction whatever its magnitude.
	hi, hf := modf(1024.0)
	assert hi == 1024.0
	assert hf == 0.0
	li, lf := modf(1.0e17)
	assert li == 1.0e17
	assert lf == 0.0
}

// Both returned parts carry the sign of the input, so the integer part of -1.5 is -1.0
// (not -2.0) and the fraction of -0.75 is -0.75.
fn test_modf_parts_share_the_sign_of_the_input() {
	i, f := modf(-1.5)
	assert i == -1.0
	assert f == -0.5
	assert i + f == -1.5
	ni, nf := modf(-3.75)
	assert ni == -3.0
	assert nf == -0.75
	assert ni + nf == -3.75
	// The integer part of a negative fraction is negative zero, not positive zero.
	zi, _ := modf(-0.75)
	assert zi == 0.0
	assert signbit(zi)
	pi, _ := modf(0.75)
	assert !signbit(pi)
}

fn test_modf_integer_plus_fraction_reconstructs_the_input() {
	for x in [f64(0.0), 1.0, -1.0, 0.5, -0.5, 1.5, -1.5, 123.456, -123.456, 1e10, -1e10, 0.1, -0.1,
		3.141592653589793] {
		i, f := modf(x)
		assert i == trunc(x)
		assert i + f == x
		assert abs(f) < 1.0
	}
}

fn test_modf_special_cases() {
	// modf(±inf) = ±inf, nan
	i, f := modf(inf(1))
	assert i == inf(1)
	assert is_nan(f)
	ni, nf := modf(inf(-1))
	assert ni == inf(-1)
	assert is_nan(nf)
	// modf(nan) = nan, nan
	n_i, n_f := modf(nan())
	assert is_nan(n_i)
	assert is_nan(n_f)
	// modf(±0) = ±0, ±0
	zi, zf := modf(0.0)
	assert zi == 0.0
	assert zf == 0.0
	assert !signbit(zi)
	assert !signbit(zf)
	mzi, mzf := modf(-0.0)
	assert mzi == 0.0
	assert mzf == 0.0
	// NOTE: the doc comment says both parts carry the sign of f, but for f = -0.0 the
	// integer part comes back as +0.0 while the fraction keeps the sign. Current
	// behaviour is asserted here rather than changed.
	assert !signbit(mzi)
	assert signbit(mzf)
	// Any other negative value, however small, does give a negative integer part.
	tiny_i, tiny_f := modf(-1e-20)
	assert tiny_i == 0.0
	assert signbit(tiny_i)
	assert tiny_f == -1e-20
	assert signbit(tiny_f)
}

// Every value at or above 2**52 is already an integer, so the fraction must be zero and
// the returned integer part must be the input itself.
fn test_modf_large_values_are_already_integers() {
	maxpowtwo := 4.503599627370496e+15 // 2**52
	ti, tf := modf(maxpowtwo)
	assert ti == maxpowtwo
	assert tf == 0.0
	nti, ntf := modf(-maxpowtwo)
	assert nti == -maxpowtwo
	assert ntf == 0.0
	pi, pf := modf(maxpowtwo - 1.0)
	assert pi == maxpowtwo - 1.0
	assert pf == 0.0
	// Above the threshold the input is passed through untouched.
	for x in [f64(1.0e16), 1.0e17, 1.0e100, max_f64, -1.0e17] {
		i, f := modf(x)
		assert i == x
		assert f == 0.0
		assert !signbit(f)
	}
}

// modf_maxpowtwo is the constant the implementation shifts the fraction off with; the
// split must stop producing a fraction at exactly that magnitude.
fn test_modf_boundary_at_maxpowtwo() {
	just_below := 4.503599627370495e+15
	i, f := modf(just_below)
	assert i == trunc(just_below)
	assert i + f == just_below
	assert abs(f) < 1.0
	at := 4.503599627370496e+15
	bi, bf := modf(at)
	assert bi == at
	assert bf == 0.0
}

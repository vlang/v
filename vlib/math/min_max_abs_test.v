import math

fn test_min() {
	assert math.min(42, 13) == 13
	assert math.min(5, -10) == -10
	assert math.min(7.1, 7.3) == 7.1
	assert math.min(u32(32), u32(17)) == 17
}

fn test_max() {
	assert math.max(42, 13) == 42
	assert math.max(5, -10) == 5
	assert math.max(7.1, 7.3) == 7.3
	assert math.max(u32(60), u32(17)) == 60
}

fn test_abs() {
	assert math.abs(99) == 99
	assert math.abs(-10) == 10
	assert math.abs(1.2345) == 1.2345
	assert math.abs(-5.5) == 5.5
}

fn test_max_min_int_has_type_of_int() {
	assert math.max(int(100), min_int) == 100
	assert math.min(int(100), max_int) == 100
}

// NaN must propagate regardless of which argument holds it. Go's `min`/`max`
// builtins do the same, so `min(NaN, 1)` and `min(1, NaN)` both return NaN.
fn test_min_max_nan() {
	assert math.is_nan(math.min(math.nan(), 1.0))
	assert math.is_nan(math.min(1.0, math.nan()))
	assert math.is_nan(math.max(math.nan(), 1.0))
	assert math.is_nan(math.max(1.0, math.nan()))
	assert math.is_nan(math.min(math.nan(), math.nan()))
	assert math.is_nan(math.max(math.nan(), math.nan()))
}

// -0.0 is not less than 0.0, so an `a < 0` test leaves it untouched. Clearing
// the sign bit returns +0.0, as Go's math.Abs does.
fn test_abs_negative_zero() {
	assert math.f64_bits(math.abs(-0.0)) == math.f64_bits(0.0)
	assert math.f64_bits(math.abs(0.0)) == math.f64_bits(0.0)
}

// The non-float branch must keep returning a value of the argument type and
// must not change the result for types where NaN has no meaning.
fn test_min_max_abs_non_float_types() {
	assert math.min('a', 'b') == 'a'
	assert math.max('a', 'b') == 'b'
	assert math.abs(99) == 99
	assert math.abs(-10) == 10
}

// Known remaining divergence from Go, recorded rather than pinned so it is not
// mistaken for a decision. `+0.0` and `-0.0` compare equal, so the tie falls
// through to `return a` here, while Go's builtins follow IEEE 754
// minNum/maxNum and always pick -0.0 / +0.0 regardless of order. Out of scope
// for https://github.com/vlang/v/issues/29833, which is about NaN.
fn test_min_max_zero_tie_breaks_diverge_from_go() {
	nz := -0.0
	assert math.f64_bits(math.min(0.0, nz)) == math.f64_bits(0.0)
	assert math.f64_bits(math.min(nz, 0.0)) == math.f64_bits(nz)
	assert math.f64_bits(math.max(0.0, nz)) == math.f64_bits(0.0)
	assert math.f64_bits(math.max(nz, 0.0)) == math.f64_bits(nz)
}

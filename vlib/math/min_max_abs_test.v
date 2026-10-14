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
	nan32 := f32(math.nan())
	assert math.is_nan(math.min(nan32, f32(1)))
	assert math.is_nan(math.min(f32(1), nan32))
	assert math.is_nan(math.max(nan32, f32(1)))
	assert math.is_nan(math.max(f32(1), nan32))
	assert math.is_nan(math.min(nan32, nan32))
	assert math.is_nan(math.max(nan32, nan32))
}

type MinMaxFloat64 = f64

type MinMaxFloat32 = f32

fn test_min_max_float_alias_nan() {
	nan64 := MinMaxFloat64(math.nan())
	nan32 := MinMaxFloat32(math.nan())
	assert math.is_nan(f64(math.min(nan64, MinMaxFloat64(1))))
	assert math.is_nan(f64(math.min(MinMaxFloat64(1), nan64)))
	assert math.is_nan(f64(math.max(nan64, MinMaxFloat64(1))))
	assert math.is_nan(f64(math.max(MinMaxFloat64(1), nan64)))
	assert math.is_nan(f32(math.min(nan32, MinMaxFloat32(1))))
	assert math.is_nan(f32(math.min(MinMaxFloat32(1), nan32)))
	assert math.is_nan(f32(math.max(nan32, MinMaxFloat32(1))))
	assert math.is_nan(f32(math.max(MinMaxFloat32(1), nan32)))
}

// The non-float branch must keep returning a value of the argument type and
// must not change the result for types where NaN has no meaning.
fn test_min_max_abs_non_float_types() {
	assert math.min('a', 'b') == 'a'
	assert math.max('a', 'b') == 'b'
	assert math.abs(99) == 99
	assert math.abs(-10) == 10
}

// Equal non-NaN arguments retain the existing choice of the second argument.
fn test_min_max_preserve_signed_zero_tie_breaks() {
	nz := -0.0
	assert math.f64_bits(math.min(0.0, nz)) == math.f64_bits(nz)
	assert math.f64_bits(math.min(nz, 0.0)) == math.f64_bits(0.0)
	assert math.f64_bits(math.max(0.0, nz)) == math.f64_bits(nz)
	assert math.f64_bits(math.max(nz, 0.0)) == math.f64_bits(0.0)
	nz32 := f32(nz)
	assert math.f32_bits(math.min(f32(0), nz32)) == math.f32_bits(nz32)
	assert math.f32_bits(math.min(nz32, f32(0))) == math.f32_bits(f32(0))
	assert math.f32_bits(math.max(f32(0), nz32)) == math.f32_bits(nz32)
	assert math.f32_bits(math.max(nz32, f32(0))) == math.f32_bits(f32(0))
}

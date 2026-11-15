module cmp

import math

fn test_compare() {
	assert compare(1, 2) == -1
	assert compare(2, 2) == 0
	assert compare(3, 2) == 1
	assert compare('a', 'b') == -1
	assert compare('x', 'x') == 0
}

fn test_less() {
	assert less(1, 2)
	assert !less(2, 2)
	assert !less(3, 2)
}

fn test_min_max() {
	assert min(3, 5) == 3
	assert max(3, 5) == 5
	assert min('a', 'b') == 'a'
	assert max('a', 'b') == 'b'
}

fn test_clamp() {
	assert clamp(5, 1, 10) == 5
	assert clamp(0, 1, 10) == 1
	assert clamp(20, 1, 10) == 10
}

fn test_coalesce() {
	assert coalesce(0, 0, 5) == 5
	assert coalesce(0, 0, 0) == 0
	assert coalesce('', '', 'x') == 'x'
	assert coalesce(false, true) == true
}

fn test_integer_boundaries() {
	assert compare(min_int, max_int) == -1
	assert compare(max_int, min_int) == 1
	assert compare(u64(0), u64(0xffff_ffff_ffff_ffff)) == -1
	assert min(u64(0xffff_ffff_ffff_ffff), u64(0)) == 0
	assert max(min_int, max_int) == max_int
}

fn test_compare_and_less_float_ordering() {
	nan := math.nan()
	assert compare(nan, 1.0) == -1
	assert compare(1.0, nan) == 1
	assert compare(nan, nan) == 0
	assert less(nan, 1.0)
	assert !less(1.0, nan)
	assert !less(nan, nan)
	assert compare(-0.0, 0.0) == 0
	assert !less(-0.0, 0.0)
	assert !less(0.0, -0.0)
	assert compare(math.inf(-1), math.inf(1)) == -1
	assert compare(math.inf(1), math.inf(1)) == 0
}

type CmpFloat32 = f32

type CmpFloat64 = f64

fn test_float_aliases() {
	nan32 := CmpFloat32(math.nan())
	nan64 := CmpFloat64(math.nan())
	assert compare(nan32, CmpFloat32(1)) == -1
	assert compare(CmpFloat32(1), nan32) == 1
	assert compare(nan64, nan64) == 0
	assert less(nan64, CmpFloat64(1))
	assert !less(CmpFloat64(1), nan64)
	assert math.is_nan(f32(min(nan32, CmpFloat32(1))))
	assert math.is_nan(f32(max(nan32, CmpFloat32(1))))
	assert math.is_nan(f64(min(nan64, CmpFloat64(1))))
	assert math.is_nan(f64(max(nan64, CmpFloat64(1))))
}

fn test_min_max_nan() {
	nan := math.nan()
	assert math.is_nan(min(nan, 1.0))
	assert math.is_nan(min(1.0, nan))
	assert math.is_nan(max(nan, 1.0))
	assert math.is_nan(max(1.0, nan))
	assert math.is_nan(min(nan, nan))
	assert math.is_nan(max(nan, nan))
	nan32 := f32(nan)
	assert math.is_nan(min(nan32, f32(1)))
	assert math.is_nan(max(nan32, f32(1)))
}

fn test_min_max_preserve_signed_zero_ties() {
	assert math.f64_bits(min(0.0, -0.0)) == math.f64_bits(-0.0)
	assert math.f64_bits(min(-0.0, 0.0)) == math.f64_bits(0.0)
	assert math.f64_bits(max(0.0, -0.0)) == math.f64_bits(-0.0)
	assert math.f64_bits(max(-0.0, 0.0)) == math.f64_bits(0.0)
}

fn test_clamp_boundaries_and_nan() {
	assert clamp(1, 1, 10) == 1
	assert clamp(10, 1, 10) == 10
	assert clamp(0, 3, 3) == 3
	assert clamp(5, 3, 3) == 3
	assert clamp('z', 'a', 'm') == 'm'
	assert clamp(math.inf(-1), -1.0, 1.0) == -1.0
	assert clamp(math.inf(1), -1.0, 1.0) == 1.0
	assert math.is_nan(clamp(math.nan(), -1.0, 1.0))
}

struct CmpValue {
	rank int
}

fn (a CmpValue) < (b CmpValue) bool {
	return a.rank < b.rank
}

fn (a CmpValue) == (b CmpValue) bool {
	return a.rank == b.rank
}

fn test_ordered_struct() {
	low := CmpValue{ rank: 1 }
	high := CmpValue{ rank: 3 }
	assert compare(low, high) == -1
	assert compare(high, low) == 1
	assert compare(low, low) == 0
	assert less(low, high)
	assert min(low, high) == low
	assert max(low, high) == high
	assert clamp(CmpValue{}, low, high) == low
}

struct CmpDefault {
	value int = 7
}

fn test_coalesce_empty_and_generic_defaults() {
	assert coalesce[int]() == 0
	assert coalesce[string]() == ''
	assert coalesce[bool]() == false
	assert coalesce[CmpValue]() == CmpValue{}
	assert coalesce(CmpValue{}, CmpValue{ rank: 2 }, CmpValue{ rank: 3 }).rank == 2
	assert coalesce[CmpDefault]().value == 7
	assert coalesce(CmpDefault{}, CmpDefault{ value: 9 }).value == 9
}

fn test_coalesce_all_zero_returns_default() {
	assert math.f64_bits(coalesce(0.0, -0.0)) == math.f64_bits(0.0)
	assert math.f32_bits(coalesce(f32(-0.0))) == math.f32_bits(f32(0.0))
}

fn test_coalesce_nan_is_nonzero() {
	assert math.is_nan(coalesce(0.0, math.nan(), 1.0))
}

// simd provides fixed-size SIMD vector types such as `F32x4`, `I8x16` and
// `U64x8`, lane masks such as `Mask32x4`, and element-wise operations on them.
// The types and methods live in vectors_generated.v, which gen.vsh writes.
module simd

import math

// broadcast_f32x4 copies value into all four lanes. It is the same as splat_f32x4.
pub fn broadcast_f32x4(value f32) F32x4 {
	return splat_f32x4(value)
}

@[noreturn]
fn lane_panic(i int, n int) {
	panic('simd: lane index ${i} out of range for ${n} lanes')
}

@[noreturn]
fn range_panic(name string, offset int, len int) {
	panic('${name}: offset ${offset} out of range for length ${len}')
}

fn abs_f32(x f32) f32 {
	return math.f32_from_bits(math.f32_bits(x) & 0x7fff_ffff)
}

fn abs_f64(x f64) f64 {
	return math.f64_from_bits(math.f64_bits(x) & 0x7fff_ffff_ffff_ffff)
}

// The division helpers wrap min / -1 to min instead of hitting C undefined
// behavior. Division by zero panics like scalar V division.
fn div_i8(x i8, y i8) i8 {
	return if y == -1 { i8(u32(0) - u32(x)) } else { x / y }
}

fn div_i16(x i16, y i16) i16 {
	return if y == -1 { i16(u32(0) - u32(x)) } else { x / y }
}

fn div_i32(x i32, y i32) i32 {
	return if y == -1 { i32(u32(0) - u32(x)) } else { x / y }
}

fn div_i64(x i64, y i64) i64 {
	return if y == -1 { i64(u64(0) - u64(x)) } else { x / y }
}

// The float to integer conversions truncate toward zero, map NaN to 0 and
// saturate out-of-range values, so they never reach a C undefined cast.
fn f32_to_i32(x f32) i32 {
	if x != x {
		return 0
	}
	if x >= 2147483648.0 {
		return max_i32
	}
	if x < -2147483648.0 {
		return min_i32
	}
	return i32(x)
}

fn f32_to_u32(x f32) u32 {
	if !(x > -1.0) {
		return 0
	}
	if x >= 4294967296.0 {
		return max_u32
	}
	return u32(x)
}

fn f64_to_i64(x f64) i64 {
	if x != x {
		return 0
	}
	if x >= 9223372036854775808.0 {
		return max_i64
	}
	if x < -9223372036854775808.0 {
		return min_i64
	}
	return i64(x)
}

fn f64_to_u64(x f64) u64 {
	if !(x > -1.0) {
		return 0
	}
	if x >= 18446744073709551616.0 {
		return max_u64
	}
	return u64(x)
}

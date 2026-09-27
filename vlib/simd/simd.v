module simd

import math

// F32x4 holds four f32 values for lane-wise arithmetic.
pub struct F32x4 {
	values [4]f32
}

// f32x4 creates a vector from four values in lane order.
pub fn f32x4(a f32, b f32, c f32, d f32) F32x4 {
	return F32x4{[a, b, c, d]!}
}

// broadcast_f32x4 copies value into all four lanes.
pub fn broadcast_f32x4(value f32) F32x4 {
	return f32x4(value, value, value, value)
}

// load_f32x4 loads four values from the start of src.
pub fn load_f32x4(src []f32) !F32x4 {
	if src.len < 4 {
		return error('simd.load_f32x4 needs at least 4 values')
	}
	return f32x4(src[0], src[1], src[2], src[3])
}

// load_f32x4_part loads up to four values and zero-fills the remaining lanes.
pub fn load_f32x4_part(src []f32) !F32x4 {
	if src.len > 4 {
		return error('simd.load_f32x4_part accepts at most 4 values')
	}
	mut values := [4]f32{}
	for i, value in src {
		values[i] = value
	}
	return F32x4{values}
}

// to_array returns the four lanes in order.
pub fn (v F32x4) to_array() [4]f32 {
	return v.values
}

// store writes all four lanes to the start of dst.
pub fn (v F32x4) store(mut dst []f32) ! {
	if dst.len < 4 {
		return error('simd.F32x4.store needs at least 4 values')
	}
	for i in 0 .. 4 {
		dst[i] = v.values[i]
	}
}

// store_part writes one lane for each element of dst, up to four elements.
pub fn (v F32x4) store_part(mut dst []f32) ! {
	if dst.len > 4 {
		return error('simd.F32x4.store_part accepts at most 4 values')
	}
	for i in 0 .. dst.len {
		dst[i] = v.values[i]
	}
}

// + adds corresponding lanes.
pub fn (v F32x4) + (other F32x4) F32x4 {
	$if @BACKEND == 'c' {
		return add_native(v, other)
	} $else {
		return f32x4(v.values[0] + other.values[0], v.values[1] + other.values[1],
			v.values[2] + other.values[2], v.values[3] + other.values[3])
	}
}

// - subtracts corresponding lanes.
pub fn (v F32x4) - (other F32x4) F32x4 {
	$if @BACKEND == 'c' {
		return sub_native(v, other)
	} $else {
		return f32x4(v.values[0] - other.values[0], v.values[1] - other.values[1],
			v.values[2] - other.values[2], v.values[3] - other.values[3])
	}
}

// * multiplies corresponding lanes.
pub fn (v F32x4) * (other F32x4) F32x4 {
	$if @BACKEND == 'c' {
		return mul_native(v, other)
	} $else {
		return f32x4(v.values[0] * other.values[0], v.values[1] * other.values[1],
			v.values[2] * other.values[2], v.values[3] * other.values[3])
	}
}

// / divides corresponding lanes.
pub fn (v F32x4) / (other F32x4) F32x4 {
	$if @BACKEND == 'c' {
		return div_native(v, other)
	} $else {
		return f32x4(v.values[0] / other.values[0], v.values[1] / other.values[1],
			v.values[2] / other.values[2], v.values[3] / other.values[3])
	}
}

// sqrt returns the square root of each lane.
pub fn (v F32x4) sqrt() F32x4 {
	$if @BACKEND == 'c' {
		return sqrt_native(v)
	} $else {
		return f32x4(f32(math.sqrt(v.values[0])), f32(math.sqrt(v.values[1])),
			f32(math.sqrt(v.values[2])), f32(math.sqrt(v.values[3])))
	}
}

// mul_add returns v * multiplier + addend for each lane.
// It does not promise fused rounding.
pub fn (v F32x4) mul_add(multiplier F32x4, addend F32x4) F32x4 {
	return v * multiplier + addend
}

// sum adds the four lanes in lane order.
pub fn (v F32x4) sum() f32 {
	return v.values[0] + v.values[1] + v.values[2] + v.values[3]
}

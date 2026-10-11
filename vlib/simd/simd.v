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
	return add_native(v, other)
}

// - subtracts corresponding lanes.
pub fn (v F32x4) - (other F32x4) F32x4 {
	return sub_native(v, other)
}

// * multiplies corresponding lanes.
pub fn (v F32x4) * (other F32x4) F32x4 {
	return mul_native(v, other)
}

// / divides corresponding lanes.
pub fn (v F32x4) / (other F32x4) F32x4 {
	return div_native(v, other)
}

// sqrt returns the square root of each lane.
pub fn (v F32x4) sqrt() F32x4 {
	return sqrt_native(v)
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

// The four-lane helpers come in two flavours by backend, which is why the
// operators above name them instead of inlining an implementation. `simd.c.v`
// holds the versions that call the C header, and is only compiled by the C
// backend; the bodies below serve every other backend. V's ARM64 backend also
// replaces a call to one of these with a single NEON instruction — that
// substitution is invisible from here, and the body is what runs wherever it
// does not apply.
$if @BACKEND != 'c' {
	// Each helper is the one operation a backend lowers to a single vector
	// instruction, so they are marked `noinline`: an inlined copy is four scalar
	// lane operations that no backend can recognize any more.
	@[noinline]
	fn add_native(a F32x4, b F32x4) F32x4 {
		return f32x4(a.values[0] + b.values[0], a.values[1] + b.values[1],
			a.values[2] + b.values[2], a.values[3] + b.values[3])
	}

	@[noinline]
	fn sub_native(a F32x4, b F32x4) F32x4 {
		return f32x4(a.values[0] - b.values[0], a.values[1] - b.values[1],
			a.values[2] - b.values[2], a.values[3] - b.values[3])
	}

	@[noinline]
	fn mul_native(a F32x4, b F32x4) F32x4 {
		return f32x4(a.values[0] * b.values[0], a.values[1] * b.values[1],
			a.values[2] * b.values[2], a.values[3] * b.values[3])
	}

	@[noinline]
	fn div_native(a F32x4, b F32x4) F32x4 {
		return f32x4(a.values[0] / b.values[0], a.values[1] / b.values[1],
			a.values[2] / b.values[2], a.values[3] / b.values[3])
	}

	@[noinline]
	fn sqrt_native(a F32x4) F32x4 {
		return f32x4(f32(math.sqrt(a.values[0])), f32(math.sqrt(a.values[1])),
			f32(math.sqrt(a.values[2])), f32(math.sqrt(a.values[3])))
	}
}

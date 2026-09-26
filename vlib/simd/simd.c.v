module simd

#flag -I @VEXEROOT/vlib/simd
#include "simd.h"

fn C.v_simd_add_f32x4(const_a &f32, const_b &f32, out &f32)
fn C.v_simd_sub_f32x4(const_a &f32, const_b &f32, out &f32)
fn C.v_simd_mul_f32x4(const_a &f32, const_b &f32, out &f32)
fn C.v_simd_div_f32x4(const_a &f32, const_b &f32, out &f32)
fn C.v_simd_sqrt_f32x4(const_a &f32, out &f32)

fn add_native(a F32x4, b F32x4) F32x4 {
	mut result := F32x4{}
	C.v_simd_add_f32x4(&a.values[0], &b.values[0], &result.values[0])
	return result
}

fn sub_native(a F32x4, b F32x4) F32x4 {
	mut result := F32x4{}
	C.v_simd_sub_f32x4(&a.values[0], &b.values[0], &result.values[0])
	return result
}

fn mul_native(a F32x4, b F32x4) F32x4 {
	mut result := F32x4{}
	C.v_simd_mul_f32x4(&a.values[0], &b.values[0], &result.values[0])
	return result
}

fn div_native(a F32x4, b F32x4) F32x4 {
	mut result := F32x4{}
	C.v_simd_div_f32x4(&a.values[0], &b.values[0], &result.values[0])
	return result
}

fn sqrt_native(a F32x4) F32x4 {
	mut result := F32x4{}
	C.v_simd_sqrt_f32x4(&a.values[0], &result.values[0])
	return result
}

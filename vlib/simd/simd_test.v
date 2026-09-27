module main

import simd

fn test_f32x4_arithmetic() {
	a := simd.f32x4(1, 4, 9, 16)
	b := simd.broadcast_f32x4(2)
	assert (a + b).to_array() == [f32(3), 6, 11, 18]!
	assert (a - b).to_array() == [f32(-1), 2, 7, 14]!
	assert (a * b).to_array() == [f32(2), 8, 18, 32]!
	assert (a / b).to_array() == [f32(0.5), 2, 4.5, 8]!
	assert a.sqrt().to_array() == [f32(1), 2, 3, 4]!
	assert a.mul_add(b, simd.broadcast_f32x4(1)).to_array() == [f32(3), 9, 19, 33]!
	assert a.sum() == 30
}

fn test_f32x4_load_store_and_tail() {
	src := [f32(1), 2, 3, 4, 5]
	v := simd.load_f32x4(src) or { panic(err) }
	mut dst := [f32(0), 0, 0, 0, 0]
	v.store(mut dst) or { panic(err) }
	assert dst == [f32(1), 2, 3, 4, 0]
	part := simd.load_f32x4_part(src[3..]) or { panic(err) }
	assert part.to_array() == [f32(4), 5, 0, 0]!
	mut tail := [f32(0), 0]
	part.store_part(mut tail) or { panic(err) }
	assert tail == [f32(4), 5]
	assert (simd.load_f32x4_part([]f32{}) or { panic(err) }).to_array() == [4]f32{}
}

fn test_f32x4_rejects_invalid_lengths() {
	mut failed := false
	simd.load_f32x4([f32(1), 2, 3]) or { failed = true }
	assert failed
	failed = false
	simd.load_f32x4_part([f32(1), 2, 3, 4, 5]) or { failed = true }
	assert failed
	mut short := [f32(0), 0, 0]
	failed = false
	simd.f32x4(1, 2, 3, 4).store(mut short) or { failed = true }
	assert failed
	mut long := [f32(0), 0, 0, 0, 0]
	failed = false
	simd.f32x4(1, 2, 3, 4).store_part(mut long) or { failed = true }
	assert failed
}

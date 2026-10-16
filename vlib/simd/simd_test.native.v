module main

// This file is selected only by the native backend (`-b native` / `-b arm64`).
// The C backend exercises the same API in `simd_test.v`; what is added here is
// the per-lane consistency check, which is the assertion that catches a vector
// load filling the wrong registers.
//
// Running it needs a native-backend host, and also a test runner that can be
// built by that backend: `v test` itself is compiled to the active backend, and
// the runner uses closures that capture variables, which the native backend
// does not support yet. Until that lands this file is checked by reading it and
// by running the same assertions under the C backend, not by CI.

import simd

fn test_native_f32x4_arithmetic() {
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

fn test_native_f32x4_matches_the_scalar_lanes() {
	// Each lane is computed from its own value, so a wrong lane order — the
	// failure mode of a load that fills the wrong registers — shows up here.
	a := simd.f32x4(0.5, 1.5, 2.5, 3.5)
	b := simd.f32x4(4, 3, 2, 1)
	added := (a + b).to_array()
	for i in 0 .. 4 {
		assert added[i] == a.to_array()[i] + b.to_array()[i]
	}
	multiplied := (a * b).to_array()
	for i in 0 .. 4 {
		assert multiplied[i] == a.to_array()[i] * b.to_array()[i]
	}
}

fn test_native_f32x4_partial_load_and_store() {
	src := [f32(1), 2, 3, 4, 5]
	v := simd.load_f32x4(src) or { panic(err) }
	assert v.to_array() == [f32(1), 2, 3, 4]!
	part := simd.load_f32x4_part(src[3..]) or { panic(err) }
	assert part.to_array() == [f32(4), 5, 0, 0]!
	mut tail := [f32(0), 0]
	part.store_part(mut tail) or { panic(err) }
	assert tail == [f32(4), 5]!
}

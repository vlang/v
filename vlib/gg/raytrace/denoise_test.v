module raytrace

import math

fn test_temporal_and_bilateral() {
	mut accum := [f32(1), 1, 1, 1, 1, 1, 1, 1]
	current := [f32(0), 0, 0, 0, 0, 0, 0, 0]
	temporal_accumulate(mut accum, current, 0.5)
	assert int(accum[0] * 1000) == 500
	temporal_accumulate(mut accum, current, 0.5)
	assert int(accum[0] * 1000) == 250

	// 4x2 image: one hot pixel among dark ones. The threshold keeps the edge.
	src := [f32(0), 0, 0, 1, 10, 10, 10, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,
		0, 0, 1, 0, 0, 0, 1]
	mut dst := []f32{len: src.len, init: 0}
	bilateral_denoise(mut dst, src, 4, 2, 1, 2)
	assert dst[4] == 10
	assert dst[3] == 1
}

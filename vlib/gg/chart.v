// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// chart_normalize maps values from [min, max] to [0, 1] and clamps the result.
// When max <= min every output is zero.
pub fn chart_normalize(values []f32, min f32, max f32) []f32 {
	mut out := []f32{len: values.len}
	for i, v in values {
		out[i] = if max > min { math.clip((v - min) / (max - min), 0, 1) } else { f32(0) }
	}
	return out
}

// chart_min_max returns a usable range for values. Equal bounds are widened by
// one and an empty slice returns (0, 1).
pub fn chart_min_max(values []f32) (f32, f32) {
	if values.len == 0 {
		return 0, 1
	}
	mut lo := values[0]
	mut hi := values[0]
	for v in values {
		if v < lo {
			lo = v
		}
		if v > hi {
			hi = v
		}
	}
	if lo == hi {
		hi = lo + 1
	}
	return lo, hi
}

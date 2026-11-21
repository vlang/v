module hdr

import math

fn test_color_space_roundtrip() {
	original := from_rgb(0.5, 0.25, 0.75)
	for from in [ColorSpace.srgb, .p3, .rec2020] {
		for to in [ColorSpace.srgb, .p3, .rec2020] {
			converted := convert_color_space(original, from, to)
			back := convert_color_space(converted, to, from)
			assert math.abs(back.r - original.r) < 0.1
			assert math.abs(back.g - original.g) < 0.1
			assert math.abs(back.b - original.b) < 0.1
		}
	}
}

fn test_lut() {
	mut lut := identity_lut(4)!
	assert lut.data.len == 4 * 4 * 4 * 3
	c := lut.apply(from_rgb(0.5, 0.25, 0.75))
	assert math.abs(c.r - 0.5) < 0.1
	assert math.abs(c.g - 0.25) < 0.1
	// A constant LUT maps everything to its constant.
	for i in 0 .. lut.data.len {
		lut.data[i] = 0.5
	}
	flat := lut.apply(white())
	assert math.abs(flat.r - 0.5) < 0.001
}

fn test_auto_exposure() {
	mut auto := AutoExposure{}
	auto.update(1, 0.1)
	assert auto.value > 0 && auto.value < 1
	for _ in 0 .. 100 {
		auto.update(0.18, 0.1)
	}
	assert math.abs(auto.value - 1) < 0.1
}

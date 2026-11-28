module anim

import gg

fn test_hsl_endpoints() {
	hsl_white := color_to_hsl(gg.rgb(255, 255, 255))
	assert hsl_white.s == 0
	assert hsl_white.l == 1
	hsl_black := color_to_hsl(gg.rgb(0, 0, 0))
	assert hsl_black.l == 0
	assert hsl_to_color(HSL{0, 0, 1}) == gg.rgb(255, 255, 255)
	assert hsl_to_color(HSL{0, 0, 0}) == gg.rgb(0, 0, 0)
}

fn test_hsl_roundtrip() {
	colors := [gg.rgb(255, 0, 0), gg.rgb(0, 255, 0), gg.rgb(0, 0, 255), gg.rgb(128, 64, 200)]
	for c in colors {
		back := hsl_to_color(color_to_hsl(c))
		assert (i16(back.r) - i16(c.r)) < 2 && (i16(c.r) - i16(back.r)) < 2, 'HSL ${c.r},${c.g},${c.b} -> ${back.r},${back.g},${back.b}'
	}
}

fn test_hsv_roundtrip() {
	colors := [gg.rgb(255, 0, 0), gg.rgb(0, 255, 128), gg.rgb(32, 16, 240)]
	for c in colors {
		back := hsv_to_color(color_to_hsv(c))
		assert (i16(back.g) - i16(c.g)) < 2 && (i16(c.g) - i16(back.g)) < 2, 'HSV ${c.r},${c.g},${c.b} -> ${back.r},${back.g},${back.b}'
	}
}

fn test_palette() {
	assert palette('missing').len == 0
	assert palette('sunset').len == 4
	stops := palette('grayscale')
	assert palette_gradient([], 0.5) == gg.Color{}
	assert palette_gradient([gg.rgb(1, 2, 3)], 0.7) == gg.rgb(1, 2, 3)
	assert palette_gradient(stops, 0) == stops[0]
	assert palette_gradient(stops, 1) == stops[stops.len - 1]
	mid := palette_gradient(stops, 0.5)
	assert mid.r == 128 || mid.r == 127
}

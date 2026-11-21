// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

fn post_clamp_u8(v f32) u8 {
	if v <= 0 {
		return 0
	}
	if v >= 255 {
		return 255
	}
	return u8(v)
}

fn post_luma(r f32, g f32, b f32) f32 {
	return 0.2126 * r + 0.7152 * g + 0.0722 * b
}

// post_grayscale returns a luminance-correct grayscale copy of RGBA pixels.
pub fn post_grayscale(pixels []u8, width int, height int) []u8 {
	mut out := []u8{len: pixels.len}
	for i in 0 .. width * height {
		at := i * 4
		if at + 3 >= pixels.len {
			break
		}
		l := post_luma(f32(pixels[at]), f32(pixels[at + 1]), f32(pixels[at + 2]))
		y := post_clamp_u8(l)
		out[at] = y
		out[at + 1] = y
		out[at + 2] = y
		out[at + 3] = pixels[at + 3]
	}
	return out
}

// post_brightness scales RGB channels by factor while preserving alpha.
pub fn post_brightness(pixels []u8, width int, height int, factor f32) []u8 {
	mut out := []u8{len: pixels.len}
	for i in 0 .. width * height {
		at := i * 4
		if at + 3 >= pixels.len {
			break
		}
		out[at] = post_clamp_u8(f32(pixels[at]) * factor)
		out[at + 1] = post_clamp_u8(f32(pixels[at + 1]) * factor)
		out[at + 2] = post_clamp_u8(f32(pixels[at + 2]) * factor)
		out[at + 3] = pixels[at + 3]
	}
	return out
}

// post_contrast adjusts contrast around mid gray; factor 1 is a no-op.
pub fn post_contrast(pixels []u8, width int, height int, factor f32) []u8 {
	mut out := []u8{len: pixels.len}
	for i in 0 .. width * height {
		at := i * 4
		if at + 3 >= pixels.len {
			break
		}
		for c in 0 .. 3 {
			v := (f32(pixels[at + c]) - 127.5) * factor + 127.5
			out[at + c] = post_clamp_u8(v)
		}
		out[at + 3] = pixels[at + 3]
	}
	return out
}

// post_saturate scales saturation; 0 is grayscale and 1 is unchanged.
pub fn post_saturate(pixels []u8, width int, height int, amount f32) []u8 {
	mut out := []u8{len: pixels.len}
	for i in 0 .. width * height {
		at := i * 4
		if at + 3 >= pixels.len {
			break
		}
		r := f32(pixels[at])
		g := f32(pixels[at + 1])
		b := f32(pixels[at + 2])
		l := post_luma(r, g, b)
		out[at] = post_clamp_u8(l + (r - l) * amount)
		out[at + 1] = post_clamp_u8(l + (g - l) * amount)
		out[at + 2] = post_clamp_u8(l + (b - l) * amount)
		out[at + 3] = pixels[at + 3]
	}
	return out
}

// post_invert inverts RGB channels and preserves alpha.
pub fn post_invert(pixels []u8, width int, height int) []u8 {
	mut out := []u8{len: pixels.len}
	for i in 0 .. width * height {
		at := i * 4
		if at + 3 >= pixels.len {
			break
		}
		out[at] = u8(255 - pixels[at])
		out[at + 1] = u8(255 - pixels[at + 1])
		out[at + 2] = u8(255 - pixels[at + 2])
		out[at + 3] = pixels[at + 3]
	}
	return out
}

// post_vignette darkens pixels towards the image edges. strength 0 is a no-op.
pub fn post_vignette(pixels []u8, width int, height int, strength f32) []u8 {
	if width <= 0 || height <= 0 {
		return pixels.clone()
	}
	mut out := []u8{len: pixels.len}
	cx := f32(width) / 2
	cy := f32(height) / 2
	max_dist := f32(math.sqrt(f64(cx * cx + cy * cy)))
	for y in 0 .. height {
		for x in 0 .. width {
			at := (y * width + x) * 4
			if at + 3 >= pixels.len {
				continue
			}
			dx := f32(x) - cx
			dy := f32(y) - cy
			d := f32(math.sqrt(f64(dx * dx + dy * dy))) / max_dist
			factor := 1 - strength * math.clip(d * d, 0, 1)
			for c in 0 .. 3 {
				out[at + c] = post_clamp_u8(f32(pixels[at + c]) * factor)
			}
			out[at + 3] = pixels[at + 3]
		}
	}
	return out
}

// post_box_blur averages each pixel over a square neighborhood. Edge pixels
// clamp their samples to the image bounds.
pub fn post_box_blur(pixels []u8, width int, height int, radius int) []u8 {
	if width <= 0 || height <= 0 || radius <= 0 {
		return pixels.clone()
	}
	mut out := []u8{len: pixels.len}
	for y in 0 .. height {
		for x in 0 .. width {
			at := (y * width + x) * 4
			if at + 3 >= pixels.len {
				continue
			}
			mut sum_r := f32(0)
			mut sum_g := f32(0)
			mut sum_b := f32(0)
			mut count := f32(0)
			for by in y - radius .. y + radius + 1 {
				sy := if by < 0 {
					0
				} else if by >= height {
					height - 1
				} else {
					by
				}
				for bx in x - radius .. x + radius + 1 {
					sx := if bx < 0 {
						0
					} else if bx >= width {
						width - 1
					} else {
						bx
					}
					sat := (sy * width + sx) * 4
					if sat + 3 >= pixels.len {
						continue
					}
					sum_r += f32(pixels[sat])
					sum_g += f32(pixels[sat + 1])
					sum_b += f32(pixels[sat + 2])
					count++
				}
			}
			if count > 0 {
				out[at] = post_clamp_u8(sum_r / count)
				out[at + 1] = post_clamp_u8(sum_g / count)
				out[at + 2] = post_clamp_u8(sum_b / count)
				out[at + 3] = pixels[at + 3]
			}
		}
	}
	return out
}

// post_tonemap applies an ACES-style filmic curve to RGB channels. Pixels are
// treated as 0..1 and mapped back to 0..255.
pub fn post_tonemap(pixels []u8, width int, height int) []u8 {
	mut out := []u8{len: pixels.len}
	for i in 0 .. width * height {
		at := i * 4
		if at + 3 >= pixels.len {
			break
		}
		for c in 0 .. 3 {
			x := f32(pixels[at + c]) / 255
			y := x * (2.51 * x + 0.03) / (x * (2.43 * x + 0.59) + 0.14)
			out[at + c] = post_clamp_u8(math.clip(y, 0, 1) * 255)
		}
		out[at + 3] = pixels[at + 3]
	}
	return out
}

// post_bloom adds a blurred bright pass back over the image. threshold is in
// 0..1 and strength scales the added light.
pub fn post_bloom(pixels []u8, width int, height int, threshold f32, strength f32, radius int) []u8 {
	mut bright := []u8{len: pixels.len}
	for i in 0 .. width * height {
		at := i * 4
		if at + 3 >= pixels.len {
			break
		}
		l := post_luma(f32(pixels[at]), f32(pixels[at + 1]), f32(pixels[at + 2])) / 255
		if l > threshold {
			bright[at] = pixels[at]
			bright[at + 1] = pixels[at + 1]
			bright[at + 2] = pixels[at + 2]
		}
		bright[at + 3] = pixels[at + 3]
	}
	blur := post_box_blur(bright, width, height, radius)
	mut out := []u8{len: pixels.len}
	for i in 0 .. width * height {
		at := i * 4
		if at + 3 >= pixels.len {
			break
		}
		for c in 0 .. 3 {
			out[at + c] = post_clamp_u8(f32(pixels[at + c]) + f32(blur[at + c]) * strength)
		}
		out[at + 3] = pixels[at + 3]
	}
	return out
}

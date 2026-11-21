module fx

import math

// This file contains allocation-free variants of the post-processing filters.
// They read from src and write into a caller-owned dst of the same size, so a
// PassChain can ping-pong two buffers instead of allocating once per pass.

fn fill_alpha(mut dst []u8, src []u8, width int, height int) {
	for i in 0 .. width * height {
		at := i * 4 + 3
		if at < dst.len && at < src.len {
			dst[at] = src[at]
		}
	}
}

// pixel_count returns how many complete RGBA pixels both buffers can hold.
// Computing this once keeps bounds checks out of the per-pixel loops.
fn pixel_count(src_len int, dst_len int, requested int) int {
	avail := if src_len < dst_len { src_len } else { dst_len }
	if avail < 4 {
		return 0
	}
	n := avail / 4
	if requested < n {
		return requested
	}
	return n
}

pub fn post_grayscale_into(mut dst []u8, src []u8, width int, height int) {
	for i in 0 .. pixel_count(src.len, dst.len, width * height) {
		at := i * 4
		l := 0.2126 * f32(src[at]) + 0.7152 * f32(src[at + 1]) + 0.0722 * f32(src[at + 2])
		y := u8(math.clip(l, 0.0, 255.0))
		dst[at] = y
		dst[at + 1] = y
		dst[at + 2] = y
		dst[at + 3] = src[at + 3]
	}
}

pub fn post_invert_into(mut dst []u8, src []u8, width int, height int) {
	for i in 0 .. pixel_count(src.len, dst.len, width * height) {
		at := i * 4
		dst[at] = u8(255 - src[at])
		dst[at + 1] = u8(255 - src[at + 1])
		dst[at + 2] = u8(255 - src[at + 2])
		dst[at + 3] = src[at + 3]
	}
}

pub fn post_brightness_into(mut dst []u8, src []u8, width int, height int, factor f32) {
	for i in 0 .. pixel_count(src.len, dst.len, width * height) {
		at := i * 4
		dst[at] = u8(math.clip(f32(src[at]) * factor, 0.0, 255.0))
		dst[at + 1] = u8(math.clip(f32(src[at + 1]) * factor, 0.0, 255.0))
		dst[at + 2] = u8(math.clip(f32(src[at + 2]) * factor, 0.0, 255.0))
		dst[at + 3] = src[at + 3]
	}
}

pub fn post_contrast_into(mut dst []u8, src []u8, width int, height int, factor f32) {
	for i in 0 .. pixel_count(src.len, dst.len, width * height) {
		at := i * 4
		dst[at] = u8(math.clip((f32(src[at]) - 127.5) * factor + 127.5, 0.0, 255.0))
		dst[at + 1] = u8(math.clip((f32(src[at + 1]) - 127.5) * factor + 127.5, 0.0, 255.0))
		dst[at + 2] = u8(math.clip((f32(src[at + 2]) - 127.5) * factor + 127.5, 0.0, 255.0))
		dst[at + 3] = src[at + 3]
	}
}

pub fn post_saturate_into(mut dst []u8, src []u8, width int, height int, amount f32) {
	for i in 0 .. pixel_count(src.len, dst.len, width * height) {
		at := i * 4
		r := f32(src[at])
		g := f32(src[at + 1])
		b := f32(src[at + 2])
		l := 0.2126 * r + 0.7152 * g + 0.0722 * b
		dst[at] = u8(math.clip(l + (r - l) * amount, 0.0, 255.0))
		dst[at + 1] = u8(math.clip(l + (g - l) * amount, 0.0, 255.0))
		dst[at + 2] = u8(math.clip(l + (b - l) * amount, 0.0, 255.0))
		dst[at + 3] = src[at + 3]
	}
}

pub fn post_vignette_into(mut dst []u8, src []u8, width int, height int, strength f32) {
	if width <= 0 || height <= 0 {
		return
	}
	cx := f32(width) / 2
	cy := f32(height) / 2
	max_dist := f32(math.sqrt(f64(cx * cx + cy * cy)))
	for y in 0 .. height {
		for x in 0 .. width {
			at := (y * width + x) * 4
			if at + 2 >= src.len || at + 2 >= dst.len {
				continue
			}
			dx := f32(x) - cx
			dy := f32(y) - cy
			d := f32(math.sqrt(f64(dx * dx + dy * dy))) / max_dist
			factor := 1 - strength * math.clip(d * d, 0.0, 1.0)
			for c in 0 .. 3 {
				dst[at + c] = u8(math.clip(f32(src[at + c]) * factor, 0.0, 255.0))
			}
		}
	}
	fill_alpha(mut dst, src, width, height)
}

pub fn post_box_blur_into(mut dst []u8, src []u8, width int, height int, radius int) {
	for y in 0 .. height {
		for x in 0 .. width {
			at := (y * width + x) * 4
			if at + 2 >= src.len || at + 2 >= dst.len {
				continue
			}
			mut sum := [f32(0), 0, 0]
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
					if sat + 2 >= src.len {
						continue
					}
					for c in 0 .. 3 {
						sum[c] += f32(src[sat + c])
					}
					count++
				}
			}
			if count > 0 {
				for c in 0 .. 3 {
					dst[at + c] = u8(math.clip(sum[c] / count, 0.0, 255.0))
				}
			}
		}
	}
	fill_alpha(mut dst, src, width, height)
}

pub fn post_tonemap_into(mut dst []u8, src []u8, width int, height int) {
	for i in 0 .. pixel_count(src.len, dst.len, width * height) {
		at := i * 4
		mut x := f32(src[at]) / 255
		dst[at] = u8(math.clip(x * (2.51 * x + 0.03) / (x * (2.43 * x + 0.59) + 0.14), 0.0, 1.0) * 255)
		x = f32(src[at + 1]) / 255
		dst[at + 1] = u8(math.clip(x * (2.51 * x + 0.03) / (x * (2.43 * x + 0.59) + 0.14), 0.0, 1.0) * 255)
		x = f32(src[at + 2]) / 255
		dst[at + 2] = u8(math.clip(x * (2.51 * x + 0.03) / (x * (2.43 * x + 0.59) + 0.14), 0.0, 1.0) * 255)
		dst[at + 3] = src[at + 3]
	}
}

pub fn post_bloom_into(mut dst []u8, src []u8, width int, height int, threshold f32, strength f32,
	radius int) {
	count := width * height * 4
	mut bright := []u8{len: count}
	for i in 0 .. width * height {
		at := i * 4
		if at + 2 >= src.len {
			break
		}
		l := (0.2126 * f32(src[at]) + 0.7152 * f32(src[at + 1]) + 0.0722 * f32(src[at + 2])) / 255
		if l > threshold {
			for c in 0 .. 3 {
				bright[at + c] = src[at + c]
			}
		}
		bright[at + 3] = src[at + 3]
	}
	mut blur := []u8{len: count}
	post_box_blur_into(mut blur, bright, width, height, radius)
	for i in 0 .. count {
		if i < src.len && i < dst.len {
			dst[i] = u8(math.clip(f32(src[i]) + f32(blur[i]) * strength, 0.0, 255.0))
		}
	}
}

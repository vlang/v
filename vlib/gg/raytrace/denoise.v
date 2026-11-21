module raytrace

// temporal_accumulate blends a new frame into an exponential moving average.
// alpha is the new frame's weight in (0, 1]; a small alpha gives smoother
// results with slower response to camera motion.
pub fn temporal_accumulate(mut accum []f32, current []f32, alpha f32) {
	n := if accum.len < current.len { accum.len } else { current.len }
	a := if alpha < 0 {
		f32(0)
	} else if alpha > 1 {
		f32(1)
	} else {
		alpha
	}
	for i in 0 .. n {
		accum[i] = accum[i] * (1 - a) + current[i] * a
	}
}

fn luminance(pixels []f32, at int) f32 {
	return 0.2126 * pixels[at] + 0.7152 * pixels[at + 1] + 0.0722 * pixels[at + 2]
}

// bilateral_denoise smooths RGB while preserving edges in luminance.
// sigma controls the spatial radius and threshold the luminance sensitivity.
pub fn bilateral_denoise(mut dst []f32, src []f32, width int, height int, sigma int,
	threshold f32) {
	if width <= 0 || height <= 0 || sigma < 0 {
		return
	}
	radius := if sigma > 4 { 4 } else { sigma }
	for y in 0 .. height {
		for x in 0 .. width {
			at := (y * width + x) * 4
			if at + 3 >= src.len || at + 3 >= dst.len {
				continue
			}
			center := luminance(src, at)
			mut sum := [f32(0), 0, 0]
			mut weight := f32(0)
			for dy in -radius .. radius + 1 {
				sy := if y + dy < 0 {
					0
				} else if y + dy >= height {
					height - 1
				} else {
					y + dy
				}
				for dx in -radius .. radius + 1 {
					sx := if x + dx < 0 {
						0
					} else if x + dx >= width {
						width - 1
					} else {
						x + dx
					}
					sat := (sy * width + sx) * 4
					if sat + 3 >= src.len {
						continue
					}
					mut diff := luminance(src, sat) - center
					if diff < 0 {
						diff = -diff
					}
					if threshold > 0 && diff > threshold {
						continue
					}
					for c in 0 .. 3 {
						sum[c] += src[sat + c]
					}
					weight++
				}
			}
			if weight > 0 {
				for c in 0 .. 3 {
					dst[at + c] = sum[c] / weight
				}
			}
			dst[at + 3] = src[at + 3]
		}
	}
}

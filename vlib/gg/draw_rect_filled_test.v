module gg

import gg.render
import math

fn filled_rect_samples() []DrawRectParams {
	return [
		DrawRectParams{ x: 47, y: 50, w: 1, h: 200, color: red },
		DrawRectParams{ x: 50, y: 50, w: 1, h: 200, color: red },
		DrawRectParams{ x: 55, y: 50, w: 100, h: 1, color: blue },
		DrawRectParams{ x: 55, y: 53, w: 100, h: 1, color: blue },
		DrawRectParams{ x: 47, y: 53, w: 1, h: 1, color: red },
	]
}

fn filled_rect_frame(scale f32, batched bool, rects []DrawRectParams, clip ?DrawRectParams) render.Frame {
	return filled_rect_frame_sized(scale, batched, rects, clip, Size{160, 260})
}

fn filled_rect_frame_sized(scale f32, batched bool, rects []DrawRectParams, clip ?DrawRectParams,
	logical_size Size) render.Frame {
	r := render.new_context(max_vertices: 32, max_commands: 8)
	defer {
		r.destroy()
	}
	width := int(logical_size.width * scale)
	height := int(logical_size.height * scale)
	r.matrix_mode_projection()
	r.ortho(0, f32(width), f32(height), 0, -1, 1)
	ctx := Context{
		r:     r
		scale: scale
	}
	if rect := clip {
		ctx.scissor_rect(int(rect.x), int(rect.y), int(rect.w), int(rect.h))
	}
	if batched {
		r.begin_quads()
	}
	for rect in rects {
		if batched {
			ctx.draw_rect_filled_no_context(rect.x, rect.y, rect.w, rect.h, rect.color)
		} else {
			ctx.draw_rect_filled(rect.x, rect.y, rect.w, rect.h, rect.color)
		}
	}
	if batched {
		r.end()
	}
	return r.render_cpu(width, height, 0)
}

fn filled_rect_coverage_matches(a render.Frame, b render.Frame) bool {
	if a.width != b.width || a.height != b.height {
		return false
	}
	for i, pixel in a.pixels {
		if (pixel == 0) != (b.pixels[i] == 0) {
			return false
		}
	}
	return true
}

fn test_one_pixel_filled_rectangles_have_position_independent_thickness() {
	for scale in [f32(0.75), 1, 1.25, 1.5, 1.75, 2, 2.5] {
		for batched in [false, true] {
			frame := filled_rect_frame(scale, batched, filled_rect_samples()[..4], none)
			expected := int(math.round(scale))
			for x in [47, 50] {
				mut count := 0
				for px in int((x - 1) * scale) .. int((x + 2) * scale) {
					if frame.pixels[int(100 * scale) * frame.width + px] != 0 {
						count++
					}
				}
				assert count == expected, 'vertical rectangle: scale=${scale}, x=${x}, batched=${batched}, thickness=${count}'
			}
			for y in [50, 53] {
				mut count := 0
				for py in int((y - 1) * scale) .. int((y + 2) * scale) {
					if frame.pixels[py * frame.width + int(100 * scale)] != 0 {
						count++
					}
				}
				assert count == expected, 'horizontal rectangle: scale=${scale}, y=${y}, batched=${batched}, thickness=${count}'
			}
		}
	}
}

fn test_one_pixel_filled_rectangles_fit_matching_logical_scissors() {
	for scale in [f32(0.75), 1, 1.25, 1.5, 1.75, 2, 2.5] {
		for batched in [false, true] {
			for rect in filled_rect_samples() {
				unclipped := filled_rect_frame(scale, batched, [rect], none)
				clipped := filled_rect_frame(scale, batched, [rect], rect)
				matches := filled_rect_coverage_matches(clipped, unclipped)
				assert matches, 'scissor changed rectangle coverage: scale=${scale}, rect=${rect}, batched=${batched}'
			}
		}
	}
}

fn test_one_pixel_filled_rectangles_preserve_coverage_when_extents_are_reversed() {
	for scale in [f32(0.75), 1, 1.25, 1.5, 1.75, 2, 2.5] {
		for batched in [false, true] {
			for rect in filled_rect_samples() {
				reference := filled_rect_frame(scale, batched, [rect], none)
				// Reverse each axis separately, then both axes together.
				for axes in [1, 2, 3] {
					reversed := DrawRectParams{
						...rect
						x: if axes & 1 != 0 { rect.x + rect.w } else { rect.x }
						y: if axes & 2 != 0 { rect.y + rect.h } else { rect.y }
						w: if axes & 1 != 0 { -rect.w } else { rect.w }
						h: if axes & 2 != 0 { -rect.h } else { rect.h }
					}
					for clipped in [false, true] {
						frame := if clipped {
							filled_rect_frame(scale, batched, [reversed], rect)
						} else {
							filled_rect_frame(scale, batched, [reversed], none)
						}
						matches := filled_rect_coverage_matches(frame, reference)
						assert matches, 'reversing an extent changed coverage: scale=${scale}, rect=${reversed}, batched=${batched}, clipped=${clipped}'
					}
				}
			}
		}
	}
}

fn test_scissors_enclose_fractional_edges() {
	rect := DrawRectParams{ x: 40, y: 40, w: 40, h: 40, color: red }
	clip := DrawRectParams{ x: 47, y: 50, w: 2, h: 2 }
	for batched in [false, true] {
		frame := filled_rect_frame(1.5, batched, [rect], clip)
		for y in 0 .. frame.height {
			for x in 0 .. frame.width {
				covered := frame.pixels[y * frame.width + x] != 0
				assert covered == (x >= 70 && x < 74 && y >= 75 && y < 78)
			}
		}
	}
}

fn test_scissors_with_nonpositive_dimensions_remain_empty() {
	rect := DrawRectParams{ x: 40, y: 40, w: 40, h: 40, color: red }
	for scale in [f32(0.75), 1, 1.5, 2.5] {
		for batched in [false, true] {
			for size in [0, -1] {
				for horizontal in [false, true] {
					clip := DrawRectParams{
						x: 47
						y: 53
						w: if horizontal { 20 } else { size }
						h: if horizontal { size } else { 20 }
					}
					frame := filled_rect_frame(scale, batched, [rect], clip)
					assert frame.pixels.all(it == 0)
				}
			}
		}
	}
}

fn test_one_pixel_filled_rectangles_keep_full_thickness_at_framebuffer_edges() {
	edges := [
		DrawRectParams{ x: 0, y: 10, w: 1, h: 20, color: red },
		DrawRectParams{ x: 99, y: 10, w: 1, h: 20, color: red },
		DrawRectParams{ x: 10, y: 0, w: 20, h: 1, color: blue },
		DrawRectParams{ x: 10, y: 99, w: 20, h: 1, color: blue },
		DrawRectParams{ x: 99, y: 99, w: 1, h: 1, color: red },
	]
	for scale in [f32(0.75), 1, 1.25, 1.5, 1.75, 2, 2.5] {
		thickness := int(math.round(scale))
		for batched in [false, true] {
			for axes in 0 .. 4 {
				mut rects := []DrawRectParams{}
				for rect in edges {
					rects << DrawRectParams{
						...rect
						x: if axes & 1 != 0 { rect.x + rect.w } else { rect.x }
						y: if axes & 2 != 0 { rect.y + rect.h } else { rect.y }
						w: if axes & 1 != 0 { -rect.w } else { rect.w }
						h: if axes & 2 != 0 { -rect.h } else { rect.h }
					}
				}
				for clipped in [false, true] {
					frame := if clipped {
						filled_rect_frame_sized(scale, batched, rects,
							DrawRectParams{ w: 100, h: 100 }, Size{100, 100})
					} else {
						filled_rect_frame_sized(scale, batched, rects, none, Size{100, 100})
					}
					case := 'scale=${scale}, batched=${batched}, axes=${axes}, clipped=${clipped}'
					for x in 0 .. frame.width {
						covered := frame.pixels[int(20 * scale) * frame.width + x] != 0
						assert covered == (x < thickness || x >= frame.width - thickness), 'vertical border: x=${x}, ${case}'
					}
					for y in 0 .. frame.height {
						covered := frame.pixels[y * frame.width + int(20 * scale)] != 0
						assert covered == (y < thickness || y >= frame.height - thickness), 'horizontal border: y=${y}, ${case}'
					}
					for y in frame.height - thickness .. frame.height {
						for x in frame.width - thickness .. frame.width {
							assert frame.pixels[y * frame.width + x] != 0, 'bottom-right corner: x=${x}, y=${y}, ${case}'
						}
					}
				}
			}
		}
	}
}

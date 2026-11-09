module gg

import gg.render
import math

fn filled_rect_frame(scale f32, batched bool) render.Frame {
	r := render.new_context(max_vertices: 32, max_commands: 8)
	defer {
		r.destroy()
	}
	width := int(160 * scale)
	height := int(260 * scale)
	r.matrix_mode_projection()
	r.ortho(0, f32(width), f32(height), 0, -1, 1)
	ctx := Context{
		r:     r
		scale: scale
	}
	if batched {
		r.begin_quads()
		ctx.draw_rect_filled_no_context(47, 50, 1, 200, red)
		ctx.draw_rect_filled_no_context(50, 50, 1, 200, red)
		ctx.draw_rect_filled_no_context(55, 50, 100, 1, blue)
		ctx.draw_rect_filled_no_context(55, 53, 100, 1, blue)
		r.end()
	} else {
		ctx.draw_rect_filled(47, 50, 1, 200, red)
		ctx.draw_rect_filled(50, 50, 1, 200, red)
		ctx.draw_rect_filled(55, 50, 100, 1, blue)
		ctx.draw_rect_filled(55, 53, 100, 1, blue)
	}
	return r.render_cpu(width, height, 0)
}

fn test_one_pixel_filled_rectangles_have_position_independent_thickness() {
	for scale in [f32(0.75), 1, 1.25, 1.5, 1.75, 2, 2.5] {
		for batched in [false, true] {
			frame := filled_rect_frame(scale, batched)
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

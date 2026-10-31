module gg

import math
import gg.render

const geometry_clear = u32(0xffffffff)

fn geometry_context(scale f32) &Context {
	ctx := &Context{
		scale: scale
		r:     render.new_context(max_vertices: 1024, max_commands: 16)
	}
	ctx.r.defaults()
	ctx.r.matrix_mode_projection()
	ctx.r.ortho(0, 128, 128, 0, -1, 1)
	ctx.r.matrix_mode_modelview()
	return ctx
}

fn geometry_bounds(frame render.Frame) (int, int, int, int) {
	mut left, mut top := frame.width, frame.height
	mut right, mut bottom := -1, -1
	for y in 0 .. frame.height {
		for x in 0 .. frame.width {
			if frame.pixels[y * frame.width + x] != geometry_clear {
				left = math.min(left, x)
				top = math.min(top, y)
				right = math.max(right, x)
				bottom = math.max(bottom, y)
			}
		}
	}
	assert right >= left && bottom >= top, 'drawing must emit visible geometry'
	return left, top, right, bottom
}

fn test_configured_lines_record_their_nonzero_endpoints_and_scale() {
	for scale in [f32(1), 2] {
		for points in [[f32(10), 20, 30, 20], [f32(30), 20, 10, 20], [f32(10), 20, 10, 40],
			[f32(10), 40, 10, 20], [f32(10), 20, 30, 40], [f32(30), 40, 10, 20]] {
			ctx := geometry_context(scale)
			ctx.draw_line_with_config(points[0], points[1], points[2], points[3],
				color:     red
				thickness: 2
			)
			frame := ctx.r.render_cpu(128, 128, geometry_clear)
			left, top, right, bottom := geometry_bounds(frame)
			dx, dy := points[2] - points[0], points[3] - points[1]
			length := math.sqrtf(dx * dx + dy * dy)
			// The lower edge is offset perpendicular to the directed segment.
			ox, oy := -dy / length * 2, dx / length * 2
			min_x := math.min(math.min(points[0], points[2]), math.min(points[0] + ox, points[2] + ox))
			max_x := math.max(math.max(points[0], points[2]), math.max(points[0] + ox, points[2] + ox))
			min_y := math.min(math.min(points[1], points[3]), math.min(points[1] + oy, points[3] + oy))
			max_y := math.max(math.max(points[1], points[3]), math.max(points[1] + oy, points[3] + oy))
			assert math.abs(f32(left) - min_x * scale) <= 1
			assert math.abs(f32(right + 1) - max_x * scale) <= 1
			assert math.abs(f32(top) - min_y * scale) <= 1
			assert math.abs(f32(bottom + 1) - max_y * scale) <= 1
			assert frame.pixels[0] == geometry_clear
			assert ctx.r.error() == .no_error
		}
	}
}

fn test_configured_dash_clipping_and_dot_spacing_scale() {
	for scale in [f32(1), 2] {
		ctx := geometry_context(scale)
		ctx.draw_line_with_config(10, 20, 30, 20, color: red, thickness: 1, line_type: .dashed)
		frame := ctx.r.render_cpu(128, 128, geometry_clear)
		left, top, right, bottom := geometry_bounds(frame)
		assert left == int(10 * scale)
		assert top == int(20 * scale)
		assert right == int(30 * scale) - 1
		assert bottom == int(21 * scale) - 1
		for x in [11, 17, 23, 29] {
			assert frame.pixels[int(20 * scale) * 128 + int(f32(x) * scale)] != geometry_clear
		}
		for x in [14, 20, 26, 30] {
			assert frame.pixels[int(20 * scale) * 128 + int(f32(x) * scale)] == geometry_clear
		}
		dots := geometry_context(scale)
		dots.draw_line_with_config(10, 20, 20, 20, color: red, thickness: 1, line_type: .dotted)
		dotted := dots.r.render_cpu(128, 128, geometry_clear)
		for offset in 0 .. 10 {
			pixel := dotted.pixels[int(20 * scale) * 128 + int(f32(10 + offset) * scale)]
			assert (pixel != geometry_clear) == (offset % 3 == 0)
		}
	}
}

fn test_all_ellipse_variants_record_scaled_extents() {
	for scale in [f32(1), 2] {
		for kind in 0 .. 6 {
			ctx := geometry_context(scale)
			match kind {
				0 { ctx.draw_ellipse_empty(30, 30, 10, 6, red) }
				1 { ctx.draw_ellipse_thick(30, 30, 10, 6, 2, red) }
				2 { ctx.draw_ellipse_filled(30, 30, 10, 6, red) }
				3 { ctx.draw_ellipse_empty_rotate(30, 30, 10, 6, f32(math.pi / 2), red) }
				4 { ctx.draw_ellipse_thick_rotate(30, 30, 10, 6, 2, f32(math.pi / 2), red) }
				else { ctx.draw_ellipse_filled_rotate(30, 30, 10, 6, f32(math.pi / 2), red) }
			}
			frame := ctx.r.render_cpu(128, 128, geometry_clear)
			left, top, right, bottom := geometry_bounds(frame)
			extra := if kind % 3 == 1 { f32(1) } else { f32(0) }
			rx := (if kind < 3 { f32(10) } else { f32(6) }) + extra
			ry := (if kind < 3 { f32(6) } else { f32(10) }) + extra
			assert math.abs(f32(left) - (30 - rx) * scale) <= 1
			assert math.abs(f32(right) - (30 + rx) * scale) <= 1
			assert math.abs(f32(top) - (30 - ry) * scale) <= 1
			assert math.abs(f32(bottom) - (30 + ry) * scale) <= 1
			assert ctx.r.error() == .no_error
		}
	}
}

fn test_configured_diagonal_dashes_preserve_their_direction_and_gaps() {
	for scale in [f32(1), 2] {
		for reversed in [false, true] {
			x := if reversed { f32(30) } else { f32(10) }
			y := x + 10
			direction := if reversed { f32(-1) } else { f32(1) }
			unit := direction / math.sqrtf(2)
			ctx := geometry_context(scale)
			ctx.draw_line_with_config(x, y, x + direction * 20, y + direction * 20,
				color:     red
				thickness: 2
				line_type: .dashed
			)
			frame := ctx.r.render_cpu(128, 128, geometry_clear)
			for distance in [f32(3), 9, 15, 21, 27, 31] {
				// Sample the center of the two-pixel strip, away from quad edges.
				px := int(math.floorf((x + unit * distance - unit) * scale))
				py := int(math.floorf((y + unit * distance + unit) * scale))
				assert (frame.pixels[py * 128 + px] != geometry_clear) == (int(distance) % 12 == 3 && distance < 29)
			}
			assert frame.pixels[0] == geometry_clear
			assert ctx.r.error() == .no_error
		}
	}
}

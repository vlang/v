// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gx
import sokol
import sokol.sgl
import math

// draw_pixel draws one pixel on the screen.
//
// NOTE calling this function frequently is very *inefficient*,
// for drawing shapes it's recommended to draw whole primitives with
// functions like `draw_rect_empty` or `draw_triangle_empty` etc.
[inline]
pub fn (ctx &Context) draw_pixel(x f32, y f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_points()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.end()
}

// draw_pixels draws pixels from an array of points [x, y, x2, y2, etc...]
//
// NOTE calling this function frequently is very *inefficient*,
// for drawing shapes it's recommended to draw whole primitives with
// functions like `draw_rect_empty` or `draw_triangle_empty` etc.
[direct_array_access; inline]
pub fn (ctx &Context) draw_pixels(points []f32, c gx.Color) {
	if points.len % 2 != 0 {
		return
	}
	len := points.len / 2

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_points()
	for i in 0 .. len {
		x, y := points[i * 2], points[i * 2 + 1]
		sgl.v2f(x * ctx.scale, y * ctx.scale)
	}
	sgl.end()
}

// draw_line draws a line between the points `x,y` and `x2,y2` in color `c`.
pub fn (ctx &Context) draw_line(x f32, y f32, x2 f32, y2 f32, c gx.Color) {
	$if macos {
		if ctx.native_rendering {
			// Make the line more clear on hi dpi screens: draw a rectangle
			mut width := math.abs(x2 - x)
			mut height := math.abs(y2 - y)
			if width == 0 {
				width = 1
			} else if height == 0 {
				height = 1
			}
			ctx.draw_rect_filled(x, y, f32(width), f32(height), c)
			return
		}
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.end()
}

// draw_line_with_config draws a line between the points `x,y` and `x2,y2` using `PenConfig`.
pub fn (ctx &Context) draw_line_with_config(x f32, y f32, x2 f32, y2 f32, config PenConfig) {
	if config.color.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}

	if config.thickness <= 0 {
		return
	}

	nx := x * ctx.scale
	ny := y * ctx.scale
	nx2 := x2 * ctx.scale
	ny2 := y2 * ctx.scale

	dx := nx2 - nx
	dy := ny2 - ny
	length := math.sqrtf(math.powf(x2 - x, 2) + math.powf(y2 - y, 2))
	theta := f32(math.atan2(dy, dx))

	sgl.push_matrix()

	sgl.translate(nx, ny, 0)
	sgl.rotate(theta, 0, 0, 1)
	sgl.translate(-nx, -ny, 0)

	if config.line_type == .solid {
		ctx.draw_rect_filled(x, y, length, config.thickness, config.color)
	} else {
		size := if config.line_type == .dotted { config.thickness } else { config.thickness * 3 }
		space := if size == 1 { 2 } else { size }

		mut available := length
		mut start_x := x

		for i := 0; available > 0; i++ {
			if i % 2 == 0 {
				ctx.draw_rect_filled(start_x, y, size, config.thickness, config.color)
				available -= size
				start_x += size
				continue
			}

			available -= space
			start_x += space
		}
	}

	sgl.pop_matrix()
}

// draw_poly_empty draws the outline of a polygon, given an array of points, and a color.
// NOTE that the points must be given in clockwise winding order.
pub fn (ctx &Context) draw_poly_empty(points []f32, c gx.Color) {
	len := points.len / 2
	if points.len % 2 != 0 || len < 3 {
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()
	for i in 0 .. len {
		sgl.v2f(points[2 * i] * ctx.scale, points[2 * i + 1] * ctx.scale)
	}
	sgl.v2f(points[0] * ctx.scale, points[1] * ctx.scale)
	sgl.end()
}

// draw_convex_poly draws a convex polygon, given an array of points, and a color.
// NOTE that the points must be given in clockwise winding order.
// The contents of the `points` array should be `x` and `y` coordinate pairs.
pub fn (ctx &Context) draw_convex_poly(points []f32, c gx.Color) {
	len := points.len / 2
	if points.len % 2 != 0 || len < 3 {
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_triangle_strip()
	x0 := points[0] * ctx.scale
	y0 := points[1] * ctx.scale
	for i in 1 .. len {
		x := points[i * 2] * ctx.scale
		y := points[i * 2 + 1] * ctx.scale
		sgl.v2f(x, y)
		if i & 0 == 0 {
			sgl.v2f(x0, y0)
		}
	}
	sgl.end()
}

// draw_rect_empty draws the outline of a rectangle.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height and `c` is the color of the outline.
pub fn (ctx &Context) draw_rect_empty(x f32, y f32, w f32, h f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y - 1) * ctx.scale)
	sgl.end()
}

// draw_rect_filled draws a filled rectangle.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height and `c` is the color of the fill.
pub fn (ctx &Context) draw_rect_filled(x f32, y f32, w f32, h f32, c gx.Color) {
	$if macos {
		if ctx.native_rendering {
			C.darwin_draw_rect(x, ctx.height - (y + h), w, h, c)
			return
		}
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_quads()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
	sgl.end()
}

// draw_rounded_rect_empty draws the outline of a rounded rectangle with a thickness of 1 px.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height.
// `radius` is the radius of the corner-rounding in pixels.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_rounded_rect_empty(x f32, y f32, w f32, h f32, radius f32, c gx.Color) {
	if w <= 0 || h <= 0 || radius < 0 {
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	mut new_radius := radius
	if w >= h && radius > h / 2 {
		new_radius = h / 2
	} else if radius > w / 2 {
		new_radius = w / 2
	}
	r := new_radius * ctx.scale
	sx := x * ctx.scale // start point x
	sy := y * ctx.scale
	width := w * ctx.scale
	height := h * ctx.scale
	// circle center coordinates
	ltx := sx + r
	lty := sy + r
	rtx := sx + width - r
	rty := lty
	rbx := rtx
	rby := sy + height - r
	lbx := ltx
	lby := rby

	mut rad := f32(0)
	mut dx := f32(0)
	mut dy := f32(0)

	if r != 0 {
		// left top quarter
		sgl.begin_line_strip()
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			sgl.v2f(ltx - dx, lty - dy)
		}
		sgl.end()

		// right top quarter
		sgl.begin_line_strip()
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			sgl.v2f(rtx + dx, rty - dy)
		}
		sgl.end()

		// right bottom quarter
		sgl.begin_line_strip()
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			sgl.v2f(rbx + dx, rby + dy)
		}
		sgl.end()

		// left bottom quarter
		sgl.begin_line_strip()
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			sgl.v2f(lbx - dx, lby + dy)
		}
		sgl.end()
	}

	// Currently don't use 'gg.draw_line()' directly, it will repeatedly execute '*ctx.scale'.
	sgl.begin_lines()
	// top
	sgl.v2f(ltx, sy)
	sgl.v2f(rtx, sy)
	// right
	sgl.v2f(rtx + r, rty)
	sgl.v2f(rtx + r, rby)
	// bottom
	sgl.v2f(lbx, lby + r)
	sgl.v2f(rbx, rby + r)
	// left
	sgl.v2f(sx, lty)
	sgl.v2f(sx, lby)
	sgl.end()
}

// draw_rounded_rect_filled draws a filled rounded rectangle.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height .
// `radius` is the radius of the corner-rounding in pixels.
// `c` is the color of the filled.
pub fn (ctx &Context) draw_rounded_rect_filled(x f32, y f32, w f32, h f32, radius f32, c gx.Color) {
	if w <= 0 || h <= 0 || radius < 0 {
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	mut new_radius := radius
	if w >= h && radius > h / 2 {
		new_radius = h / 2
	} else if radius > w / 2 {
		new_radius = w / 2
	}
	r := new_radius * ctx.scale
	sx := x * ctx.scale // start point x
	sy := y * ctx.scale
	width := w * ctx.scale
	height := h * ctx.scale
	// circle center coordinates
	ltx := sx + r
	lty := sy + r
	rtx := sx + width - r
	rty := lty
	rbx := rtx
	rby := sy + height - r
	lbx := ltx
	lby := rby

	mut rad := f32(0)
	mut dx := f32(0)
	mut dy := f32(0)

	if r != 0 {
		// left top quarter
		sgl.begin_triangle_strip()
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			sgl.v2f(ltx - dx, lty - dy)
			sgl.v2f(ltx, lty)
		}
		sgl.end()

		// right top quarter
		sgl.begin_triangle_strip()
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			sgl.v2f(rtx + dx, rty - dy)
			sgl.v2f(rtx, rty)
		}
		sgl.end()

		// right bottom quarter
		sgl.begin_triangle_strip()
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			sgl.v2f(rbx + dx, rby + dy)
			sgl.v2f(rbx, rby)
		}
		sgl.end()

		// left bottom quarter
		sgl.begin_triangle_strip()
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			sgl.v2f(lbx - dx, lby + dy)
			sgl.v2f(lbx, lby)
		}
		sgl.end()
	}

	// Separate drawing is to prevent transparent color overlap
	// top rectangle
	sgl.begin_quads()
	sgl.v2f(ltx, sy)
	sgl.v2f(rtx, sy)
	sgl.v2f(rtx, rty)
	sgl.v2f(ltx, lty)
	sgl.end()
	// middle rectangle
	sgl.begin_quads()
	sgl.v2f(sx, lty)
	sgl.v2f(rtx + r, rty)
	sgl.v2f(rbx + r, rby)
	sgl.v2f(sx, lby)
	sgl.end()
	// bottom rectangle
	sgl.begin_quads()
	sgl.v2f(lbx, lby)
	sgl.v2f(rbx, rby)
	sgl.v2f(rbx, rby + r)
	sgl.v2f(lbx, rby + r)
	sgl.end()
}

// draw_triangle_empty draws the outline of a triangle.
// `x`,`y` defines the first point
// `x2`,`y2` defines the second point
// `x3`,`y3` defines the third point
// `c` is the color of the outline.
pub fn (ctx &Context) draw_triangle_empty(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.v2f(x3 * ctx.scale, y3 * ctx.scale)
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.end()
}

// draw_triangle_filled draws a filled triangle.
// `x`,`y` defines the first point
// `x2`,`y2` defines the second point
// `x3`,`y3` defines the third point
// `c` is the color of the outline.
pub fn (ctx &Context) draw_triangle_filled(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_triangles()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.v2f(x3 * ctx.scale, y3 * ctx.scale)
	sgl.end()
}

// draw_square_empty draws the outline of a square.
// `x`,`y` is the top-left corner of the square.
// `s` is the length of each side of the square.
// `c` is the color of the outline.
[inline]
pub fn (ctx &Context) draw_square_empty(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_rect_empty(x, y, s, s, c)
}

// draw_square_filled draws a filled square.
// `x`,`y` is the top-left corner of the square.
// `s` is the length of each side of the square.
// `c` is the fill color.
[inline]
pub fn (ctx &Context) draw_square_filled(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_rect_filled(x, y, s, s, c)
}

// The table here is derived by looking at the result of vlib/gg/testdata/tweak_circles.vv
// and then choosing the most circle-ish drawing with the minimum number of segments.
const small_circle_segments = [0, 2, 4, 6, 6, 8, 8, 13, 10, 18, 12, 12, 10, 13, 16, 15, 16]!

[direct_array_access]
fn radius_to_segments(r f32) int {
	if r < 30 {
		ir := int(math.ceil(r))
		if ir > 0 && ir < gg.small_circle_segments.len {
			return gg.small_circle_segments[ir]
		}
		return ir
	}
	return int(math.ceil(math.tau * r / 8))
}

// draw_circle_empty draws the outline of a circle.
// `x`,`y` defines the center of the circle.
// `radius` defines the radius of the circle.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_circle_empty(x f32, y f32, radius f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := radius * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	segments := radius_to_segments(radius * ctx.scale)

	sgl.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		theta = f32(math.tau) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		sgl.v2f(xx + nx, yy + ny)
	}
	sgl.end()
}

// draw_circle_filled draws a filled circle.
// `x`,`y` defines the center of the circle.
// `radius` defines the radius of the circle.
// `c` is the fill color.
pub fn (ctx &Context) draw_circle_filled(x f32, y f32, radius f32, c gx.Color) {
	$if macos {
		if ctx.native_rendering {
			C.darwin_draw_circle(x - radius + 1, ctx.height - (y + radius + 3), radius,
				c)
			return
		}
	}
	ctx.draw_polygon_filled(x, y, radius, radius_to_segments(radius * ctx.scale), 0, c)
}

// draw_polygon_filled draws a filled polygon.
// `x`,`y` defines the center of the polygon.
// `size` defines the size of the polygon.
// `edges` defines number of edges in the polygon.
// `rotation` defines rotation of the polygon.
// `c` is the fill color.
pub fn (ctx &Context) draw_polygon_filled(x f32, y f32, size f32, edges int, rotation f32, c gx.Color) {
	if edges <= 0 {
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := size * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)

	sgl.begin_triangle_strip()
	for i := 0; i < edges + 1; i++ {
		theta = f32(math.tau) * f32(i) / f32(edges)
		xx = nr * math.cosf(theta + f32(math.radians(rotation)))
		yy = nr * math.sinf(theta + f32(math.radians(rotation)))
		sgl.v2f(xx + nx, yy + ny)
		sgl.v2f(nx, ny)
	}
	sgl.end()
}

// draw_circle_with_segments draws a filled circle with a specific number of segments.
// `x`,`y` defines the center of the circle.
// `radius` defines the radius of the circle.
// `segments` affects how smooth/round the circle is.
// `c` is the fill color.
pub fn (ctx &Context) draw_circle_with_segments(x f32, y f32, radius f32, segments int, c gx.Color) {
	ctx.draw_polygon_filled(x, y, radius, segments, 0, c)
}

// draw_circle_line draws the outline of a circle with a specific number of segments.
// `x`,`y` defines the center of the circle.
// `radius` defines the radius of the circle.
// `segments` affects how smooth/round the circle is.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_circle_line(x f32, y f32, radius int, segments int, c gx.Color) {
	if segments <= 0 {
		return
	}

	$if macos {
		if ctx.native_rendering {
			C.darwin_draw_circle(x - radius + 1, ctx.height - (y + radius + 3), radius,
				c)
			return
		}
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := radius * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)

	sgl.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		theta = f32(math.tau) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		sgl.v2f(xx + nx, yy + ny)
	}
	sgl.end()
}

// draw_slice_empty draws the outline of a circle slice/pie
pub fn (ctx &Context) draw_slice_empty(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	if segments <= 0 || radius <= 0 {
		return
	}
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := radius * ctx.scale * math.sinf(start_angle)
	mut yy := radius * ctx.scale * math.cosf(start_angle)

	sgl.begin_line_strip()
	sgl.v2f(nx, ny)
	for i := 0; i < segments + 1; i++ {
		sgl.v2f(xx + nx, yy + ny)
		xx, yy = xx + yy * tan_factor, yy - xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
	}
	sgl.v2f(nx, ny)
	sgl.end()
}

// draw_slice_filled draws a filled circle slice/pie
// `x`,`y` defines the end point of the slice (center of the circle that the slice is part of).
// `radius` defines the radius ("length") of the slice.
// `start_angle` is the angle in radians at which the slice starts.
// `end_angle` is the angle in radians at which the slice ends.
// `segments` affects how smooth/round the slice is.
// `c` is the fill color.
pub fn (ctx &Context) draw_slice_filled(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	if segments <= 0 || radius < 0 {
		return
	}
	if start_angle == end_angle {
		ctx.draw_slice_empty(x, y, radius, start_angle, end_angle, 1, c)
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := radius * ctx.scale * math.sinf(start_angle)
	mut yy := radius * ctx.scale * math.cosf(start_angle)

	sgl.begin_triangle_strip()
	sgl.v2f(xx + nx, yy + ny)
	for i := 0; i < segments; i++ {
		xx, yy = xx + yy * tan_factor, yy - xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
		sgl.v2f(xx + nx, yy + ny)
		if i & 1 == 0 {
			sgl.v2f(nx, ny)
		}
	}
	sgl.end()
}

// draw_arc_line draws a line arc.
// `x`,`y` defines the end point of the arc (center of the circle that the arc is part of).
// `radius` defines the radius of the arc (length from the center point where the arc is drawn).
// `start_angle` is the angle in radians at which the arc starts.
// `end_angle` is the angle in radians at which the arc ends.
// `segments` affects how smooth/round the arc is.
// `c` is the color of the arc/outline.
pub fn (ctx Context) draw_arc_line(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	if segments <= 0 || radius < 0 {
		return
	}
	if radius == 0 {
		ctx.draw_pixel(x, y, c)
		return
	}
	if start_angle == end_angle {
		xx := x + radius * math.sinf(start_angle)
		yy := y + radius * math.cosf(start_angle)
		ctx.draw_pixel(xx, yy, c)
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := radius * ctx.scale * math.sinf(start_angle)
	mut yy := radius * ctx.scale * math.cosf(start_angle)

	sgl.begin_line_strip()
	sgl.v2f(nx + xx, ny + yy)
	for i := 0; i < segments; i++ {
		xx, yy = xx + yy * tan_factor, yy - xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
		sgl.v2f(nx + xx, ny + yy)
	}
	sgl.end()
}

// draw_arc_empty draws the outline of an arc.
// `x`,`y` defines the end point of the arc (center of the circle that the arc is part of).
// `inner_radius` defines the radius of the arc (length from the center point where the arc is drawn).
// `thickness` defines how wide the arc is drawn.
// `start_angle` is the angle in radians at which the arc starts.
// `end_angle` is the angle in radians at which the arc ends.
// `segments` affects how smooth/round the arc is.
// `c` is the color of the arc outline.
pub fn (ctx &Context) draw_arc_empty(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	outer_radius := inner_radius + thickness
	if segments <= 0 || outer_radius < 0 {
		return
	}

	if inner_radius <= 0 {
		ctx.draw_slice_empty(x, y, outer_radius, start_angle, end_angle, segments, c)
		return
	}
	if inner_radius == outer_radius {
		ctx.draw_arc_line(x, y, outer_radius, start_angle, end_angle, segments, c)
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)

	sgl.begin_line_strip()

	// outer
	mut xx := outer_radius * ctx.scale * math.sinf(start_angle)
	mut yy := outer_radius * ctx.scale * math.cosf(start_angle)
	sxx, syy := xx, yy
	sgl.v2f(nx + xx, ny + yy)
	for i := 0; i < segments; i++ {
		xx, yy = xx + yy * tan_factor, yy - xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
		sgl.v2f(nx + xx, ny + yy)
	}

	// inner
	xx = inner_radius * ctx.scale * math.sinf(end_angle)
	yy = inner_radius * ctx.scale * math.cosf(end_angle)
	sgl.v2f(nx + xx, ny + yy)
	for i := 0; i < segments; i++ {
		xx, yy = xx - yy * tan_factor, yy + xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
		sgl.v2f(nx + xx, ny + yy)
	}

	sgl.v2f(nx + sxx, ny + syy)
	sgl.end()
}

// draw_arc_filled draws a filled arc.
// `x`,`y` defines the central point of the arc (center of the circle that the arc is part of).
// `inner_radius` defines the radius of the arc (length from the center point where the arc is drawn).
// `thickness` defines how wide the arc is drawn.
// `start_angle` is the angle in radians at which the arc starts.
// `end_angle` is the angle in radians at which the arc ends.
// `segments` affects how smooth/round the arc is.
// `c` is the fill color of the arc.
pub fn (ctx &Context) draw_arc_filled(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	outer_radius := inner_radius + thickness
	if segments <= 0 || outer_radius < 0 {
		return
	}

	if inner_radius <= 0 {
		ctx.draw_slice_filled(x, y, outer_radius, start_angle, end_angle, segments, c)
		return
	}
	if inner_radius == outer_radius {
		ctx.draw_arc_line(x, y, outer_radius, start_angle, end_angle, segments, c)
		return
	}
	if start_angle == end_angle {
		ctx.draw_arc_empty(x, y, inner_radius, thickness, start_angle, end_angle, 1, c)
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut ix := ctx.scale * math.sinf(start_angle)
	mut iy := ctx.scale * math.cosf(start_angle)
	mut ox := outer_radius * ix
	mut oy := outer_radius * iy
	ix *= inner_radius
	iy *= inner_radius

	sgl.begin_triangle_strip()
	sgl.v2f(nx + ix, ny + iy)
	sgl.v2f(nx + ox, ny + oy)
	for i := 0; i < segments; i++ {
		ix, iy = ix + iy * tan_factor, iy - ix * tan_factor
		ix *= rad_factor
		iy *= rad_factor
		sgl.v2f(nx + ix, ny + iy)
		ox, oy = ox + oy * tan_factor, oy - ox * tan_factor
		ox *= rad_factor
		oy *= rad_factor
		sgl.v2f(nx + ox, ny + oy)
	}
	sgl.end()
}

// draw_ellipse_empty draws the outline of an ellipse.
// `x`,`y` defines the center of the ellipse.
// `rw` defines the *width* radius of the ellipse.
// `rh` defines the *height* radius of the ellipse.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_ellipse_empty(x f32, y f32, rw f32, rh f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()
	for i := 0; i < 360; i += 10 {
		sgl.v2f(x + math.sinf(f32(math.radians(i))) * rw, y + math.cosf(f32(math.radians(i))) * rh)
		sgl.v2f(x + math.sinf(f32(math.radians(i + 10))) * rw, y + math.cosf(f32(math.radians(i +
			10))) * rh)
	}
	sgl.end()
}

// draw_ellipse_filled draws an opaque elipse.
// `x`,`y` defines the center of the ellipse.
// `rw` defines the *width* radius of the ellipse.
// `rh` defines the *height* radius of the ellipse.
// `c` is the fill color.
pub fn (ctx &Context) draw_ellipse_filled(x f32, y f32, rw f32, rh f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_triangle_strip()
	for i := 0; i < 360; i += 10 {
		sgl.v2f(x, y)
		sgl.v2f(x + math.sinf(f32(math.radians(i))) * rw, y + math.cosf(f32(math.radians(i))) * rh)
		sgl.v2f(x + math.sinf(f32(math.radians(i + 10))) * rw, y + math.cosf(f32(math.radians(i +
			10))) * rh)
	}
	sgl.end()
}

// draw_cubic_bezier draws a cubic Bézier curve, also known as a spline, from four points.
// The four points is provided as one `points` array which contains a stream of point pairs (x and y coordinates).
// Thus a cubic Bézier could be declared as: `points := [x1, y1, control_x1, control_y1, control_x2, control_y2, x2, y2]`.
// Please see `draw_cubic_bezier_in_steps` to control the amount of steps (segments) used to draw the curve.
pub fn (ctx &Context) draw_cubic_bezier(points []f32, c gx.Color) {
	ctx.draw_cubic_bezier_in_steps(points, u32(30 * ctx.scale), c)
}

// draw_cubic_bezier_in_steps draws a cubic Bézier curve, also known as a spline, from four points.
// The smoothness of the curve can be controlled with the `steps` parameter. `steps` determines how many iterations is
// taken to draw the curve.
// The four points is provided as one `points` array which contains a stream of point pairs (x and y coordinates).
// Thus a cubic Bézier could be declared as: `points := [x1, y1, control_x1, control_y1, control_x2, control_y2, x2, y2]`.
pub fn (ctx &Context) draw_cubic_bezier_in_steps(points []f32, steps u32, c gx.Color) {
	if steps <= 0 || points.len != 8 {
		return
	}
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()

	p1_x, p1_y := points[0], points[1]
	p2_x, p2_y := points[6], points[7]

	ctrl_p1_x, ctrl_p1_y := points[2], points[3]
	ctrl_p2_x, ctrl_p2_y := points[4], points[5]

	// The constant 3 is actually points.len() - 1;

	step := f32(1) / steps
	sgl.v2f(p1_x * ctx.scale, p1_y * ctx.scale)
	for u := f32(0); u <= f32(1); u += step {
		pow_2_u := u * u
		pow_3_u := pow_2_u * u

		x := pow_3_u * (p2_x + 3 * (ctrl_p1_x - ctrl_p2_x) - p1_x) +
			3 * pow_2_u * (p1_x - 2 * ctrl_p1_x + ctrl_p2_x) + 3 * u * (ctrl_p1_x - p1_x) + p1_x

		y := pow_3_u * (p2_y + 3 * (ctrl_p1_y - ctrl_p2_y) - p1_y) +
			3 * pow_2_u * (p1_y - 2 * ctrl_p1_y + ctrl_p2_y) + 3 * u * (ctrl_p1_y - p1_y) + p1_y

		sgl.v2f(x * ctx.scale, y * ctx.scale)
	}
	sgl.v2f(p2_x * ctx.scale, p2_y * ctx.scale)

	sgl.end()
}

//---- deprecated

// Sets a pixel
[deprecated: 'use draw_pixel() instead']
pub fn (ctx &Context) set_pixel(x f32, y f32, c gx.Color) {
	ctx.draw_pixel(x, y, c)
}

[deprecated: 'use draw_pixels() instead']
pub fn (ctx &Context) set_pixels(points []f32, c gx.Color) {
	ctx.draw_pixels(points, c)
}

[deprecated: 'use draw_poly_empty() instead']
pub fn (ctx &Context) draw_empty_poly(points []f32, c gx.Color) {
	ctx.draw_poly_empty(points, c)
}

// TODO: Fix alpha
[deprecated: 'use draw_rect_filled() instead']
pub fn (ctx &Context) draw_rect(x f32, y f32, w f32, h f32, c gx.Color) {
	ctx.draw_rect_filled(x, y, w, h, c)
}

// Draws the outline of a rectangle
[deprecated: 'use draw_rect_empty() instead']
pub fn (ctx &Context) draw_empty_rect(x f32, y f32, w f32, h f32, c gx.Color) {
	ctx.draw_rect_empty(x, y, w, h, c)
}

[deprecated: 'use draw_rounded_rect_empty()']
pub fn (ctx &Context) draw_empty_rounded_rect(x f32, y f32, w f32, h f32, radius f32, c gx.Color) {
	ctx.draw_rounded_rect_empty(x, y, w, h, radius, c)
}

[deprecated: 'use draw_rounded_rect_filled()']
pub fn (ctx &Context) draw_rounded_rect(x f32, y f32, w f32, h f32, radius f32, c gx.Color) {
	ctx.draw_rounded_rect_filled(x, y, w, h, radius, c)
}

// Draws the outline of a triangle
[deprecated: 'use draw_triangle_empty() instead']
pub fn (ctx &Context) draw_empty_triangle(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	ctx.draw_triangle_empty(x, y, x2, y2, x3, y3, c)
}

// Draws a filled triangle
[deprecated: 'use draw_triangle_filled() instead']
pub fn (ctx &Context) draw_triangle(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	ctx.draw_triangle_filled(x, y, x2, y2, x3, y3, c)
}

// Draws the outline of a square
[deprecated: 'use draw_square_empty() instead']
pub fn (ctx &Context) draw_empty_square(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_square_empty(x, y, s, c)
}

// Draws a filled square
[deprecated: 'use draw_square_filled() instead']
pub fn (ctx &Context) draw_square(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_square_filled(x, y, s, c)
}

[deprecated: 'use draw_circle_filled() instead']
pub fn (ctx &Context) draw_circle(x f32, y f32, radius f32, c gx.Color) {
	ctx.draw_circle_filled(x, y, radius, c)
}

[deprecated: 'use draw_slice_empty() instead']
pub fn (ctx &Context) draw_empty_slice(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	ctx.draw_slice_empty(x, y, radius, start_angle, end_angle, segments, c)
}

[deprecated: 'use draw_slice_filled() instead']
pub fn (ctx &Context) draw_slice(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	ctx.draw_slice_filled(x, y, radius, start_angle, end_angle, segments, c)
}

[deprecated: 'use draw_arc_empty() instead']
pub fn (ctx &Context) draw_empty_arc(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	ctx.draw_arc_empty(x, y, inner_radius, thickness, start_angle, end_angle, segments,
		c)
}

[deprecated: 'use draw_arc_filled() instead']
pub fn (ctx &Context) draw_arc(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	ctx.draw_arc_filled(x, y, inner_radius, thickness, start_angle, end_angle, segments,
		c)
}

[deprecated: 'use draw_ellipse_empty() instead']
pub fn (ctx &Context) draw_empty_ellipse(x f32, y f32, rw f32, rh f32, c gx.Color) {
	ctx.draw_ellipse_empty(x, y, rw, rh, c)
}

[deprecated: 'use draw_ellipse_filled() instead']
pub fn (ctx &Context) draw_ellipse(x f32, y f32, rw f32, rh f32, c gx.Color) {
	ctx.draw_ellipse_filled(x, y, rw, rh, c)
}

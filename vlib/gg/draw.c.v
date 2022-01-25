// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module gg

import sokol
import sokol.sgl
import math

//---- pixel

[inline]
pub fn (ctx &Context) draw_pixel(x f32, y f32, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_points()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.end()
}

// Sets pixels from an array of points [x, y, x2, y2, etc...]
[direct_array_access; inline]
pub fn (ctx &Context) draw_pixels(points []f32, c Color) {
	assert points.len % 2 == 0
	len := points.len / 2

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_points()
	for i in 0 .. len {
		x, y := points[i * 2], points[i * 2 + 1]
		sgl.v2f(x * ctx.scale, y * ctx.scale)
	}
	sgl.end()
}

//---- line

// Draws a line between the points provided
pub fn (ctx &Context) draw_line(x f32, y f32, x2 f32, y2 f32, c Color) {
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
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.end()
}

// Draws a line between the points provided with the PenConfig
pub fn (ctx &Context) draw_line_with_config(x f32, y f32, x2 f32, y2 f32, config PenConfig) {
	if config.color.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
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

//---- polyline

// draw_empty_poly draws the borders of a polygon, given an array of points, and a color.
// Note that the points must be given in clockwise order.
pub fn (ctx &Context) draw_poly_empty(points []f32, c Color) {
	assert points.len % 2 == 0
	len := points.len / 2
	assert len >= 3

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
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
// Note that the points must be given in clockwise order.
pub fn (ctx &Context) draw_convex_poly(points []f32, c Color) {
	assert points.len % 2 == 0
	len := points.len / 2
	assert len >= 3

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_triangle_strip()
	x0 := points[0] * ctx.scale
	y0 := points[1] * ctx.scale
	for i in 1 .. (len / 2 + 1) {
		sgl.v2f(x0, y0)
		sgl.v2f(points[i * 4 - 2] * ctx.scale, points[i * 4 - 1] * ctx.scale)
		sgl.v2f(points[i * 4] * ctx.scale, points[i * 4 + 1] * ctx.scale)
	}

	if len % 2 == 0 {
		sgl.v2f(points[2 * len - 2] * ctx.scale, points[2 * len - 1] * ctx.scale)
	}
	sgl.end()
}

//---- rectangle

pub fn (ctx &Context) draw_rect_empty(x f32, y f32, w f32, h f32, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
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

pub fn (ctx &Context) draw_rect_filled(x f32, y f32, w f32, h f32, c Color) {
	$if macos {
		if ctx.native_rendering {
			C.darwin_draw_rect(x, ctx.height - (y + h), w, h, c)
			return
		}
	}
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_quads()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
	sgl.end()
}

// draw_rounded_rect_empty draws the outline of a rounded rectangle
pub fn (ctx &Context) draw_rounded_rect_empty(x f32, y f32, w f32, h f32, radius f32, c Color) {
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	r := radius * ctx.scale
	nx := x * ctx.scale
	ny := y * ctx.scale
	width := w * ctx.scale
	height := h * ctx.scale
	segments := 2 * math.pi * r
	segdiv := segments / 4
	rb := 0
	lb := int(rb + segdiv)
	lt := int(lb + segdiv)
	rt := int(lt + segdiv)
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	// left top
	lx := nx + r
	ly := ny + r
	theta_coeff := 2 * f32(math.pi) / segments
	for i in lt .. rt {
		theta = theta_coeff * f32(i)
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + lx, yy + ly)
	}
	// right top
	mut rx := nx + width - r
	mut ry := ny + r
	for i in rt .. int(segments) {
		theta = theta_coeff * f32(i)
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rx, yy + ry)
	}
	// right bottom
	mut rbx := rx
	mut rby := ny + height - r
	for i in rb .. lb {
		theta = theta_coeff * f32(i)
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rbx, yy + rby)
	}
	// left bottom
	mut lbx := lx
	mut lby := ny + height - r
	for i in lb .. lt {
		theta = theta_coeff * f32(i)
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + lbx, yy + lby)
	}
	sgl.v2f(lx + xx, ly)
	sgl.end()
}

// draw_rounded_rect_filled draws a filled rounded rectangle
pub fn (ctx &Context) draw_rounded_rect_filled(x f32, y f32, w f32, h f32, radius f32, c Color) {
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_triangle_strip()
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	r := radius * ctx.scale
	nx := x * ctx.scale
	ny := y * ctx.scale
	width := w * ctx.scale
	height := h * ctx.scale
	segments := 2 * math.pi * r
	segdiv := segments / 4
	rb := 0
	lb := int(rb + segdiv)
	lt := int(lb + segdiv)
	rt := int(lt + segdiv)
	// left top
	lx := nx + r
	ly := ny + r
	for i in lt .. rt {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + lx, yy + ly)
		sgl.v2f(lx, ly)
	}
	// right top
	mut rx := nx + width - r
	mut ry := ny + r
	for i in rt .. int(segments) {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rx, yy + ry)
		sgl.v2f(rx, ry)
	}
	// right bottom
	mut rbx := rx
	mut rby := ny + height - r
	for i in rb .. lb {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rbx, yy + rby)
		sgl.v2f(rbx, rby)
	}
	// left bottom
	mut lbx := lx
	mut lby := ny + height - r
	for i in lb .. lt {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + lbx, yy + lby)
		sgl.v2f(lbx, lby)
	}
	sgl.v2f(lx + xx, ly)
	sgl.v2f(lx, ly)
	sgl.end()
	sgl.begin_quads()
	sgl.v2f(lx, ly)
	sgl.v2f(rx, ry)
	sgl.v2f(rbx, rby)
	sgl.v2f(lbx, lby)
	sgl.end()
}

//---- triangle

pub fn (ctx &Context) draw_triangle_empty(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.v2f(x3 * ctx.scale, y3 * ctx.scale)
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.end()
}

pub fn (ctx &Context) draw_triangle_filled(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_triangles()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.v2f(x3 * ctx.scale, y3 * ctx.scale)
	sgl.end()
}

//---- square

[inline]
pub fn (ctx &Context) draw_square_empty(x f32, y f32, s f32, c Color) {
	ctx.draw_rect_empty(x, y, s, s, c)
}

[inline]
pub fn (ctx &Context) draw_square_filled(x f32, y f32, s f32, c Color) {
	ctx.draw_rect_filled(x, y, s, s, c)
}

//---- circle

const circle_segment_size = 8.35

// draw_circle_empty draws an empty circle
pub fn (ctx &Context) draw_circle_empty(x f32, y f32, r f32, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := r * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)

	segments := math.ceil(2 * math.pi * r / gg.circle_segment_size)

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		theta = 2.0 * f32(math.pi) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		sgl.v2f(xx + nx, yy + ny)
	}
	sgl.end()
}

// draw_circle_filled draws a filled circle
pub fn (ctx &Context) draw_circle_filled(x f32, y f32, r f32, c Color) {
	segments := int(math.ceil(2 * math.pi * r / gg.circle_segment_size))
	ctx.draw_circle_with_segments(x, y, r, segments, c)
}

// draw_circle_with_segments draws a circle with a specific number of segments (affects how smooth/round the circle is)
pub fn (ctx &Context) draw_circle_with_segments(x f32, y f32, r f32, segments int, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := r * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	sgl.begin_triangle_strip()
	for i := 0; i < segments + 1; i++ {
		theta = 2.0 * f32(math.pi) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		sgl.v2f(xx + nx, yy + ny)
		sgl.v2f(nx, ny)
	}
	sgl.end()
}

pub fn (ctx &Context) draw_circle_line(x f32, y f32, r int, segments int, c Color) {
	$if macos {
		if ctx.native_rendering {
			C.darwin_draw_circle(x - r + 1, ctx.height - (y + r + 3), r, c)
			return
		}
	}
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := r * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	sgl.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		theta = 2.0 * f32(math.pi) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		sgl.v2f(xx + nx, yy + ny)
	}
	sgl.end()
}

//---- slice

// draw_slice_empty draws the outline of a circle slice/pie
pub fn (ctx &Context) draw_slice_empty(x f32, y f32, outer_radius f32, start_angle f32, end_angle f32, segments int, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	theta := f32(end_angle / f32(segments))
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	nx := x * ctx.scale
	ny := y * ctx.scale
	mut xx := outer_radius * math.cosf(start_angle)
	mut yy := outer_radius * math.sinf(start_angle)
	sgl.begin_line_strip()
	sgl.v2f(nx, ny)
	for i := 0; i < segments + 1; i++ {
		sgl.v2f(xx + nx, yy + ny)
		tx := -yy
		ty := xx
		xx += tx * tan_factor
		yy += ty * tan_factor
		xx *= rad_factor
		yy *= rad_factor
	}
	sgl.v2f(nx, ny)
	sgl.end()
}

// draw_slice_filled draws a filled circle slice/pie
pub fn (ctx &Context) draw_slice_filled(x f32, y f32, r f32, start_angle f32, end_angle f32, segments int, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle / f32(segments))
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := r * math.cosf(start_angle)
	mut yy := r * math.sinf(start_angle)
	sgl.begin_triangle_strip()
	for i := 0; i < segments + 1; i++ {
		sgl.v2f(xx + nx, yy + ny)
		sgl.v2f(nx, ny)
		tx := -yy
		ty := xx
		xx += tx * tan_factor
		yy += ty * tan_factor
		xx *= rad_factor
		yy *= rad_factor
	}
	sgl.end()
}

//---- arc

// draw_arc_empty draws the outline of an arc
pub fn (ctx &Context) draw_arc_empty(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c Color) {
	if start_angle == end_angle || inner_radius <= 0.0 {
		return
	}

	mut a1 := start_angle
	mut a2 := end_angle

	if a2 < a1 {
		a1, a2 = a2, a1
	}

	if inner_radius <= 0.0 {
		ctx.draw_slice_empty(x, y, int(thickness), a1, a2, segments, c)
		return
	}

	outer_radius := inner_radius + thickness
	mut step_length := (a2 - a1) / f32(segments)
	mut angle := a1

	sgl.begin_line_strip()
	sgl.c4b(c.r, c.g, c.b, c.a)

	// Outer circle
	for _ in 0 .. segments {
		msa := f32(math.sin(angle))
		mca := f32(math.cos(angle))
		ms := f32(math.sin(angle + step_length))
		mc := f32(math.cos(angle + step_length))
		sgl.v2f(x + msa * outer_radius, y + mca * outer_radius)
		sgl.v2f(x + ms * outer_radius, y + mc * outer_radius)
		angle += step_length
	}

	// Inner circle
	for _ in 0 .. segments {
		msa := f32(math.sin(angle))
		mca := f32(math.cos(angle))
		msb := f32(math.sin(angle - step_length))
		mcb := f32(math.cos(angle - step_length))
		sgl.v2f(x + msa * inner_radius, y + mca * inner_radius)
		sgl.v2f(x + msb * inner_radius, y + mcb * inner_radius)

		angle -= step_length
	}

	// Closing end
	msa := f32(math.sin(angle))
	mca := f32(math.cos(angle))
	sgl.v2f(x + msa * inner_radius, y + mca * inner_radius)
	sgl.v2f(x + msa * outer_radius, y + mca * outer_radius)
	sgl.end()
}

// draw_arc_filled draws a filled arc
pub fn (ctx &Context) draw_arc_filled(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c Color) {
	if start_angle == end_angle || inner_radius <= 0.0 {
		return
	}

	mut a1 := start_angle
	mut a2 := end_angle

	if a2 < a1 {
		a1, a2 = a2, a1
	}

	if inner_radius <= 0.0 {
		ctx.draw_slice_filled(x, y, int(thickness), a1, a2, segments, c)
	}

	outer_radius := inner_radius + thickness
	mut step_length := (a2 - a1) / f32(segments)
	mut angle := a1

	sgl.begin_quads()
	sgl.c4b(c.r, c.g, c.b, c.a)
	for _ in 0 .. segments {
		msa := f32(math.sin(angle))
		mca := f32(math.cos(angle))
		sgl.v2f(x + msa * inner_radius, y + mca * inner_radius)
		sgl.v2f(x + msa * outer_radius, y + mca * outer_radius)

		ms := f32(math.sin(angle + step_length))
		mc := f32(math.cos(angle + step_length))
		sgl.v2f(x + ms * outer_radius, y + mc * outer_radius)
		sgl.v2f(x + ms * inner_radius, y + mc * inner_radius)

		angle += step_length
	}
	sgl.end()
}

//---- ellipse

// draw_ellipse_empty draws the outline of an ellipse, with a center at x,y
pub fn (ctx &Context) draw_ellipse_empty(x f32, y f32, rw f32, rh f32, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
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

// draw_ellipse_filled - draws an opaque elipse, with a center at x,y , filled with the color `c`
pub fn (ctx &Context) draw_ellipse_filled(x f32, y f32, rw f32, rh f32, c Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
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

//---- bezier

// draw_cubic_bezier draws a cubic Bézier curve, also known as a spline, from four points.
// The four points is provided as one `points` array which contains a stream of point pairs (x and y coordinates).
// Thus a cubic Bézier could be declared as: `points := [x1, y1, control_x1, control_y1, control_x2, control_y2, x2, y2]`.
// Please see `draw_cubic_bezier_in_steps` to control the amount of steps (segments) used to draw the curve.
pub fn (ctx &Context) draw_cubic_bezier(points []f32, c Color) {
	ctx.draw_cubic_bezier_in_steps(points, u32(30 * ctx.scale), c)
}

// draw_cubic_bezier_in_steps draws a cubic Bézier curve, also known as a spline, from four points.
// The smoothness of the curve can be controlled with the `steps` parameter. `steps` determines how many iterations is
// taken to draw the curve.
// The four points is provided as one `points` array which contains a stream of point pairs (x and y coordinates).
// Thus a cubic Bézier could be declared as: `points := [x1, y1, control_x1, control_y1, control_x2, control_y2, x2, y2]`.
pub fn (ctx &Context) draw_cubic_bezier_in_steps(points []f32, steps u32, c Color) {
	assert steps > 0
	assert points.len == 8

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()

	p1_x, p1_y := points[0], points[1]
	p2_x, p2_y := points[6], points[7]

	ctrl_p1_x, ctrl_p1_y := points[2], points[3]
	ctrl_p2_x, ctrl_p2_y := points[4], points[5]

	// The constant 3 is actually points.len() - 1;

	step := f32(1.0) / steps
	sgl.v2f(p1_x * ctx.scale, p1_y * ctx.scale)
	for u := f32(0.0); u <= f32(1.0); u += step {
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
pub fn (ctx &Context) set_pixel(x f32, y f32, c Color) {
	ctx.draw_pixel(x, y, c)
}

[deprecated: 'use draw_pixels() instead']
pub fn (ctx &Context) set_pixels(points []f32, c Color) {
	ctx.draw_pixels(points, c)
}

[deprecated: 'use draw_poly_empty() instead']
pub fn (ctx &Context) draw_empty_poly(points []f32, c Color) {
	ctx.draw_poly_empty(points, c)
}

// TODO: Fix alpha
[deprecated: 'use draw_rect_filled() instead']
pub fn (ctx &Context) draw_rect(x f32, y f32, w f32, h f32, c Color) {
	ctx.draw_rect_filled(x, y, w, h, c)
}

// Draws the outline of a rectangle
[deprecated: 'use draw_rect_empty() instead']
pub fn (ctx &Context) draw_empty_rect(x f32, y f32, w f32, h f32, c Color) {
	ctx.draw_rect_empty(x, y, w, h, c)
}

[deprecated: 'use draw_rounded_rect_empty()']
pub fn (ctx &Context) draw_empty_rounded_rect(x f32, y f32, w f32, h f32, radius f32, c Color) {
	ctx.draw_rounded_rect_empty(x, y, w, h, radius, c)
}

[deprecated: 'use draw_rounded_rect_filled()']
pub fn (ctx &Context) draw_rounded_rect(x f32, y f32, w f32, h f32, radius f32, c Color) {
	ctx.draw_rounded_rect_filled(x, y, w, h, radius, c)
}

// Draws the outline of a triangle
[deprecated: 'use draw_triangle_empty() instead']
pub fn (ctx &Context) draw_empty_triangle(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c Color) {
	ctx.draw_triangle_empty(x, y, x2, y2, x3, y3, c)
}

// Draws a filled triangle
[deprecated: 'use draw_triangle_filled() instead']
pub fn (ctx &Context) draw_triangle(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c Color) {
	ctx.draw_triangle_filled(x, y, x2, y2, x3, y3, c)
}

// Draws the outline of a square
[deprecated: 'use draw_square_empty() instead']
pub fn (ctx &Context) draw_empty_square(x f32, y f32, s f32, c Color) {
	ctx.draw_square_empty(x, y, s, c)
}

// Draws a filled square
[deprecated: 'use draw_square_filled() instead']
pub fn (ctx &Context) draw_square(x f32, y f32, s f32, c Color) {
	ctx.draw_square_filled(x, y, s, c)
}

[deprecated: 'use draw_circle_filled() instead']
pub fn (ctx &Context) draw_circle(x f32, y f32, r f32, c Color) {
	ctx.draw_circle_filled(x, y, r, c)
}

[deprecated: 'use draw_slice_empty() instead']
pub fn (ctx &Context) draw_empty_slice(x f32, y f32, r f32, start_angle f32, end_angle f32, segments int, c Color) {
	ctx.draw_slice_empty(x, y, r, start_angle, end_angle, segments, c)
}

[deprecated: 'use draw_slice_filled() instead']
pub fn (ctx &Context) draw_slice(x f32, y f32, r f32, start_angle f32, end_angle f32, segments int, c Color) {
	ctx.draw_slice_filled(x, y, r, start_angle, end_angle, segments, c)
}

[deprecated: 'use draw_arc_empty() instead']
pub fn (ctx &Context) draw_empty_arc(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c Color) {
	ctx.draw_arc_empty(x, y, inner_radius, thickness, start_angle, end_angle, segments,
		c)
}

[deprecated: 'use draw_arc_filled() instead']
pub fn (ctx &Context) draw_arc(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c Color) {
	ctx.draw_arc_filled(x, y, inner_radius, thickness, start_angle, end_angle, segments,
		c)
}

[deprecated: 'use draw_ellipse_empty() instead']
pub fn (ctx &Context) draw_empty_ellipse(x f32, y f32, rw f32, rh f32, c Color) {
	ctx.draw_ellipse_empty(x, y, rw, rh, c)
}

[deprecated: 'use draw_ellipse_filled() instead']
pub fn (ctx &Context) draw_ellipse(x f32, y f32, rw f32, rh f32, c Color) {
	ctx.draw_ellipse_filled(x, y, rw, rh, c)
}

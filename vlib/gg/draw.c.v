// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

@[params]
pub struct DrawPixelConfig {
pub mut:
	size f32 = 1.0
}

// draw_pixel draws one pixel on the screen.
//
// NOTE calling this function frequently is very *inefficient*,
// for drawing shapes it's recommended to draw whole primitives with
// functions like `draw_rect_empty` or `draw_triangle_empty` etc.
pub fn (ctx &Context) draw_pixel(x f32, y f32, c Color, params DrawPixelConfig) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.begin_points()
	ctx.r.c4b(c.r, c.g, c.b, c.a)
	ctx.r.point_size(params.size)
	ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	ctx.r.end()
}

// draw_pixels draws pixels from an array of points [x, y, x2, y2, etc...]
//
// NOTE calling this function frequently is very *inefficient*,
// for drawing shapes it's recommended to draw whole primitives with
// functions like `draw_rect_empty` or `draw_triangle_empty` etc.
@[direct_array_access]
pub fn (ctx &Context) draw_pixels(points []f32, c Color, params DrawPixelConfig) {
	if points.len % 2 != 0 {
		return
	}
	len := points.len / 2

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.begin_points()
	ctx.r.c4b(c.r, c.g, c.b, c.a)
	ctx.r.point_size(params.size)
	for i in 0 .. len {
		x, y := points[i * 2], points[i * 2 + 1]
		ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	}
	ctx.r.end()
}

// draw_line draws a line between the points `x,y` and `x2,y2` in color `c`.
pub fn (ctx &Context) draw_line(x f32, y f32, x2 f32, y2 f32, c Color) {
	$if macos {
		if ctx.native_rendering {
			// Make horizontal/vertical lines more clear on hi dpi screens
			// by drawing a rectangle. Diagonals use the same native surface.
			if x == x2 || y == y2 {
				// Normalize the origin, so the rect covers the segment
				// regardless of the order of the points.
				rx := if x < x2 { x } else { x2 }
				ry := if y < y2 { y } else { y2 }
				mut width := math.abs(x2 - x)
				mut height := math.abs(y2 - y)
				if width == 0 {
					width = 1
				} else if height == 0 {
					height = 1
				}
				ctx.draw_rect_filled(rx, ry, f32(width), f32(height), c)
				return
			}
			darwin_stroke_line(current_cg_context(), x, ctx.height - y, x2, ctx.height - y2,
				c)
			return
		}
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	ctx.r.begin_lines()
	ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	ctx.r.v2f(x2 * ctx.scale, y2 * ctx.scale)
	ctx.r.end()
}

// draw_line_with_config draws a line between the points `x,y` and `x2,y2` using `PenConfig`.
// Coordinates, thickness and dash spacing are in logical pixels.
pub fn (ctx &Context) draw_line_with_config(x f32, y f32, x2 f32, y2 f32, config PenConfig) {
	if config.color.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(config.color.r, config.color.g, config.color.b, config.color.a)

	if config.thickness <= 0 {
		return
	}

	nx := x * ctx.scale
	ny := y * ctx.scale
	nx2 := x2 * ctx.scale
	ny2 := y2 * ctx.scale

	dx := nx2 - nx
	dy := ny2 - ny
	// length and thickness are in device space: the matrix below already
	// holds the scaled translation+rotation, so the rects are emitted at
	// the origin without applying ctx.scale a second time.
	length := math.sqrtf(dx * dx + dy * dy)
	thickness := config.thickness * ctx.scale
	theta := f32(math.atan2(dy, dx))

	ctx.r.push_matrix()

	ctx.r.translate(nx, ny, 0)
	ctx.r.rotate(theta, 0, 0, 1)

	ctx.r.begin_quads()
	if config.line_type == .solid {
		ctx.r.v2f(0, 0)
		ctx.r.v2f(length, 0)
		ctx.r.v2f(length, thickness)
		ctx.r.v2f(0, thickness)
	} else {
		logical_size := if config.line_type == .dotted {
			config.thickness
		} else {
			config.thickness * 3
		}
		size := logical_size * ctx.scale
		space := (if logical_size == 1 { f32(2) } else { logical_size }) * ctx.scale
		mut start_x := f32(0)

		for start_x < length {
			end_x := math.min(start_x + size, length)
			ctx.r.v2f(start_x, 0)
			ctx.r.v2f(end_x, 0)
			ctx.r.v2f(end_x, thickness)
			ctx.r.v2f(start_x, thickness)
			start_x += size + space
		}
	}
	ctx.r.end()

	ctx.r.pop_matrix()
}

// draw_poly_empty draws the outline of a polygon, given an array of points, and a color.
// NOTE that the points must be given in clockwise winding order.
pub fn (ctx &Context) draw_poly_empty(points []f32, c Color) {
	len := points.len / 2
	if points.len % 2 != 0 || len < 3 {
		return
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	ctx.r.begin_line_strip()
	for i in 0 .. len {
		ctx.r.v2f(points[2 * i] * ctx.scale, points[2 * i + 1] * ctx.scale)
	}
	ctx.r.v2f(points[0] * ctx.scale, points[1] * ctx.scale)
	ctx.r.end()
}

// draw_convex_poly draws a convex polygon, given an array of points, and a color.
// NOTE that the points must be given in clockwise winding order.
// The contents of the `points` array should be `x` and `y` coordinate pairs.
pub fn (ctx &Context) draw_convex_poly(points []f32, c Color) {
	len := points.len / 2
	if points.len % 2 != 0 || len < 3 {
		return
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	ctx.r.begin_triangle_strip()
	x0 := points[0] * ctx.scale
	y0 := points[1] * ctx.scale
	for i in 1 .. len {
		x := points[i * 2] * ctx.scale
		y := points[i * 2 + 1] * ctx.scale
		ctx.r.v2f(x, y)
		// Re-emit the shared first vertex on even steps to continue the strip.
		if i % 2 == 0 {
			ctx.r.v2f(x0, y0)
		}
	}
	ctx.r.end()
}

@[inline]
fn rect_empty_screen_bounds(scale f32, x f32, y f32, w f32, h f32) (f32, f32, f32, f32) {
	// Keep the outline inside pixels so the top-left corner stays aligned and the
	// border renders consistently across different OpenGL implementations.
	toffset := f32(0.1)
	boffset := f32(-0.1)
	return toffset + x * scale, toffset + y * scale, boffset + (x + w) * scale, boffset +
		(y + h) * scale
}

// draw_rect_empty draws the outline of a rectangle.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height and `c` is the color of the outline.
// Note: it is much more efficient to draw lots of empty rectangles one after the other,
// without filled rectangles between them, than to draw a mix.
pub fn (ctx &Context) draw_rect_empty(x f32, y f32, w f32, h f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)
	tleft_x, tleft_y, bright_x, bright_y := rect_empty_screen_bounds(ctx.scale, x, y, w, h)
	ctx.r.begin_lines() // more predictable, compared to ctx.r.begin_line_strip, at the price of more vertexes send
	// top:
	ctx.r.v2f(tleft_x, tleft_y)
	ctx.r.v2f(bright_x, tleft_y)
	// left:
	ctx.r.v2f(tleft_x, tleft_y)
	ctx.r.v2f(tleft_x, bright_y)
	// right:
	ctx.r.v2f(bright_x, tleft_y)
	ctx.r.v2f(bright_x, bright_y)
	// bottom:
	ctx.r.v2f(tleft_x, bright_y)
	ctx.r.v2f(bright_x, bright_y)
	ctx.r.end()
}

// draw_rect_empty_no_context draws the outline of a rectangle, but without saving/restoring the context.
// It is intended to be used in loops, where you do manually: `ctx.r.begin_lines()` *before* the loop,
// then draw many rectangles, then call manually `ctx.r.end()` *after* the loop.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height and `c` is the color of the outline.
// Note: it is much more efficient to draw lots of empty rectangles one after the other,
// without filled rectangles between them, than to draw a mix.
pub fn (ctx &Context) draw_rect_empty_no_context(x f32, y f32, w f32, h f32, c Color) {
	tleft_x, tleft_y, bright_x, bright_y := rect_empty_screen_bounds(ctx.scale, x, y, w, h)
	// Note: the following line is deliberately commented, compare to draw_rect_empty/5;
	// ctx.r.begin_lines() // more predictable, compared to ctx.r.begin_line_strip, at the price of more vertexes send
	ctx.r.c4b(c.r, c.g, c.b, c.a)
	// top:
	ctx.r.v2f(tleft_x, tleft_y)
	ctx.r.v2f(bright_x, tleft_y)
	// left:
	ctx.r.v2f(tleft_x, tleft_y)
	ctx.r.v2f(tleft_x, bright_y)
	// right:
	ctx.r.v2f(bright_x, tleft_y)
	ctx.r.v2f(bright_x, bright_y)
	// bottom:
	ctx.r.v2f(tleft_x, bright_y)
	ctx.r.v2f(bright_x, bright_y)
	// Note: the following line is deliberately commented, compare to draw_rect_empty/5;
	// ctx.r.end()
}

// draw_rect_filled draws a filled rectangle.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height and `c` is the color of the fill.
// Note: it is much more efficient to draw lots of filled rectangles one after the other,
// without empty rectangles between them, than to draw a mix.
pub fn (ctx &Context) draw_rect_filled(x f32, y f32, w f32, h f32, c Color) {
	$if macos {
		if ctx.native_rendering {
			darwin_draw_rect(x, ctx.height - (y + h), w, h, c)
			return
		}
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	ctx.r.begin_quads()
	ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	ctx.r.v2f((x + w) * ctx.scale, y * ctx.scale)
	ctx.r.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	ctx.r.v2f(x * ctx.scale, (y + h) * ctx.scale)
	ctx.r.end()
}

// draw_rect_filled_no_context draws a filled rectangle, but without saving/restoring the context.
// It is intended to be used in loops, where you do manually: `ctx.r.begin_quads()` *before* the loop,
// then draw many rectangles, then call manually `ctx.r.end()` *after* the loop.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height and `c` is the color of the fill.
// Note: it is much more efficient to draw lots of filled rectangles one after the other,
// without empty rectangles between them, than to draw a mix.
pub fn (ctx &Context) draw_rect_filled_no_context(x f32, y f32, w f32, h f32, c Color) {
	// Note: the following line is deliberately commented, compare to draw_rect_empty/5;
	// ctx.r.begin_quads()
	ctx.r.c4b(c.r, c.g, c.b, c.a)
	ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	ctx.r.v2f((x + w) * ctx.scale, y * ctx.scale)
	ctx.r.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	ctx.r.v2f(x * ctx.scale, (y + h) * ctx.scale)
	// Note: the following line is deliberately commented, compare to draw_rect_filled/5;
	// ctx.r.end()
}

pub enum PaintStyle {
	fill
	stroke
}

@[params]
pub struct DrawRectParams {
pub:
	x          f32
	y          f32
	w          f32
	h          f32
	color      Color      = black
	style      PaintStyle = .fill
	is_rounded bool
	radius     f32
}

pub fn (ctx &Context) draw_rect(p DrawRectParams) {
	if p.is_rounded {
		if p.style == .fill {
			ctx.draw_rounded_rect_filled(p.x, p.y, p.w, p.h, p.radius, p.color)
		} else {
			ctx.draw_rounded_rect_empty(p.x, p.y, p.w, p.h, p.radius, p.color)
		}
	} else {
		if p.style == .fill {
			ctx.draw_rect_filled(p.x, p.y, p.w, p.h, p.color)
		} else {
			ctx.draw_rect_empty(p.x, p.y, p.w, p.h, p.color)
		}
	}
}

// draw_rounded_rect_empty draws the outline of a rounded rectangle with a thickness of 1 px.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height.
// `radius` is the radius of the corner-rounding in pixels.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_rounded_rect_empty(x f32, y f32, w f32, h f32, radius f32, c Color) {
	if w <= 0 || h <= 0 || radius < 0 {
		return
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	mut new_radius := radius
	if radius < 1 {
		new_radius = 0
	}
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

	align_pixel := fn (v f32) f32 {
		return math.floorf(v) + 0.5
	}
	// circle center coordinates
	ltx := align_pixel(sx + r)
	lty := align_pixel(sy + r)
	rtx := align_pixel(sx + width - r)
	rty := align_pixel(sy + r)
	rbx := align_pixel(sx + width - r)
	rby := align_pixel(sy + height - r)
	lbx := align_pixel(sx + r)
	lby := align_pixel(sy + height - r)

	mut rad := f32(0)
	mut dx := f32(0)
	mut dy := f32(0)

	if r == 0 {
		ctx.r.begin_line_strip()
		// top
		ctx.r.v2f(ltx, lty)
		ctx.r.v2f(rtx, rty)
		// right
		ctx.r.v2f(rbx, rby)
		// bottom
		ctx.r.v2f(lbx, lby)
		// left
		ctx.r.v2f(ltx, lty)
		ctx.r.end()
		return
	}

	ctx.r.begin_line_strip()
	// left top quarter
	for i in 0 .. 31 {
		rad = f32(math.radians(i * 3))
		dx = r * math.cosf(rad)
		dy = r * math.sinf(rad)
		ctx.r.v2f(ltx - dx, lty - dy)
	}
	// right top quarter
	for i in 0 .. 31 {
		rad = f32(math.radians(i * 3))
		dx = r * math.sinf(rad)
		dy = r * math.cosf(rad)
		ctx.r.v2f(rtx + dx, rty - dy)
	}
	// right bottom quarter
	for i in 0 .. 31 {
		rad = f32(math.radians(i * 3))
		dx = r * math.cosf(rad)
		dy = r * math.sinf(rad)
		ctx.r.v2f(rbx + dx, rby + dy)
	}
	// left bottom quarter
	for i in 0 .. 31 {
		rad = f32(math.radians(i * 3))
		dx = r * math.sinf(rad)
		dy = r * math.cosf(rad)
		ctx.r.v2f(lbx - dx, lby + dy)
	}
	// close
	ctx.r.v2f(ltx - r, lty)
	ctx.r.end()
}

// draw_rounded_rect_filled draws a filled rounded rectangle.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height .
// `radius` is the radius of the corner-rounding in pixels.
// `c` is the color of the filled.
// it divides the rounded rectangle into 2 shapes, the top rounded part and the bottom rounded part which are connected at both extremes.
pub fn (ctx &Context) draw_rounded_rect_filled(x f32, y f32, w f32, h f32, radius f32, c Color) {
	if w <= 0 || h <= 0 || radius < 0 {
		return
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

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

	// left x coordinate
	lx := sx + r
	// right x coordinate
	rx := sx + width - r
	// top y coordinate
	ty := sy + r
	// bottom y coordinate
	by := sy + height - r

	if r == 0 {
		// No radius means juste a rectangle
		ctx.r.begin_quads()
		ctx.r.v2f(sx, ty)
		ctx.r.v2f(rx + r, ty)
		ctx.r.v2f(rx + r, by)
		ctx.r.v2f(sx, by)
		ctx.r.end()
	} else {
		// draw the top then the bottom and link them with 2 triangle
		mut rad := f32(0)
		mut dx := f32(0)
		mut dy := f32(0)

		// top part
		// starting at -30 then multiplying it by -1 makes you ends with the angle closer to the side which is needed to link both parts
		ctx.r.begin_triangle_strip()
		for i in -30 .. 1 {
			rad = f32(math.radians(-i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			ctx.r.v2f(rx + dx, ty - dy)
			ctx.r.v2f(lx - dx, ty - dy)
		}

		// bottom part
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = r * math.cosf(rad)
			dy = r * math.sinf(rad)
			ctx.r.v2f(rx + dx, by + dy)
			ctx.r.v2f(lx - dx, by + dy)
		}
		ctx.r.end()
	}
}

// clamp_radius clamps a corner radius into [0, max_radius].
@[inline]
fn clamp_radius(r f32, max_radius f32) f32 {
	if r < 0 {
		return 0
	}
	if r > max_radius {
		return max_radius
	}
	return r
}

// clamp_slice clamps a 9-slice border size into [0, available].
@[inline]
fn clamp_slice(size int, available f32) f32 {
	if size < 0 || available <= 0 {
		return 0
	}
	v := f32(size)
	if v > available {
		return available
	}
	return v
}

// draw_rounded_rect_filled_4 draws a filled rounded rectangle with per-corner radii.
// `tl`, `tr`, `br`, `bl` are the radii for top-left, top-right, bottom-right, bottom-left.
pub fn (ctx &Context) draw_rounded_rect_filled_4(x f32, y f32, w f32, h f32, tl f32, tr f32, br f32, bl f32, c Color) {
	if w <= 0 || h <= 0 {
		return
	}
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	// radii are clamped to half of the smaller side, in logical pixels
	max_radius := if w < h { w / 2 } else { h / 2 }
	r_tl := clamp_radius(tl, max_radius)
	r_tr := clamp_radius(tr, max_radius)
	r_br := clamp_radius(br, max_radius)
	r_bl := clamp_radius(bl, max_radius)

	s := ctx.scale
	sx := x * s
	sy := y * s
	width := w * s
	height := h * s
	rtl := r_tl * s
	rtr := r_tr * s
	rbr := r_br * s
	rbl := r_bl * s

	// A rounded rectangle is convex, so one triangle strip sweeping the left and
	// right outlines covers it exactly once. Overlapping quads would double-blend
	// translucent fills, so do not split this into a center rect plus edge rects.
	segments := 30
	ctx.r.begin_triangle_strip()
	for i in 0 .. segments + 1 {
		t := f32(i) / f32(segments)
		// left outline, top to bottom: top-left arc
		a := 3.0 * math.pi / 2.0 - t * (math.pi / 2.0)
		ctx.r.v2f(sx + rtl + rtl * math.cosf(a), sy + rtl + rtl * math.sinf(a))
		// right outline, top to bottom: top-right arc
		b := -math.pi / 2.0 + t * (math.pi / 2.0)
		ctx.r.v2f(sx + width - rtr + rtr * math.cosf(b), sy + rtr + rtr * math.sinf(b))
	}
	for i in 0 .. segments + 1 {
		t := f32(i) / f32(segments)
		// left outline, top to bottom: bottom-left arc
		a := math.pi - t * (math.pi / 2.0)
		ctx.r.v2f(sx + rbl + rbl * math.cosf(a), sy + height - rbl + rbl * math.sinf(a))
		// right outline, top to bottom: bottom-right arc
		b := t * (math.pi / 2.0)
		ctx.r.v2f(sx + width - rbr + rbr * math.cosf(b), sy + height - rbr + rbr * math.sinf(b))
	}
	ctx.r.end()
}

// draw_9slice draws an image scaled to fit the target rect using 9-slice scaling.
pub fn (ctx &Context) draw_9slice(img &Image, x f32, y f32, w f32, h f32, slice_left int, slice_top int, slice_right int, slice_bottom int) {
	if img == nil || !img.simg_ok {
		return
	}
	s := ctx.scale
	ix := x * s
	iy := y * s
	iw := if w == 0 { f32(img.width) * s } else { w * s }
	ih := if h == 0 { f32(img.height) * s } else { h * s }
	sw := f32(img.width)
	sh := f32(img.height)
	if sw <= 0 || sh <= 0 || iw <= 0 || ih <= 0 {
		return
	}
	// source slices, clamped to the image and to each other
	src_l := clamp_slice(slice_left, sw)
	src_r := clamp_slice(slice_right, sw - src_l)
	src_t := clamp_slice(slice_top, sh)
	src_b := clamp_slice(slice_bottom, sh - src_t)
	// destination slices shrink proportionally when the target is smaller than
	// their combined size, so no slice ever gets negative width/height.
	mut sl := src_l * s
	mut sr := src_r * s
	mut st := src_t * s
	mut sb := src_b * s
	if sl + sr > iw && sl + sr > 0 {
		k := iw / (sl + sr)
		sl *= k
		sr *= k
	}
	if st + sb > ih && st + sb > 0 {
		k := ih / (st + sb)
		st *= k
		sb *= k
	}
	mid_w := iw - sl - sr
	mid_h := ih - st - sb

	ctx.r.enable_texture()
	ctx.r.texture(img.simg, img.ssmp)
	ctx.r.load_pipeline(ctx.pipeline.alpha)

	u_l := src_l / sw
	u_r := (sw - src_r) / sw
	v_t := src_t / sh
	v_b := (sh - src_b) / sh

	// 9 slices: top-left, top-center, top-right, mid-left, center, mid-right, bot-left, bot-center, bot-right
	// Top row
	ctx.r.begin_quads()
	ctx.r.c4b(255, 255, 255, 255)
	// top-left
	ctx.r.v3f_t2f(ix, iy, 0, 0, 0)
	ctx.r.v3f_t2f(ix + sl, iy, 0, u_l, 0)
	ctx.r.v3f_t2f(ix + sl, iy + st, 0, u_l, v_t)
	ctx.r.v3f_t2f(ix, iy + st, 0, 0, v_t)
	// top-center
	ctx.r.v3f_t2f(ix + sl, iy, 0, u_l, 0)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy, 0, u_r, 0)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + st, 0, u_r, v_t)
	ctx.r.v3f_t2f(ix + sl, iy + st, 0, u_l, v_t)
	// top-right
	ctx.r.v3f_t2f(ix + sl + mid_w, iy, 0, u_r, 0)
	ctx.r.v3f_t2f(ix + iw, iy, 0, 1, 0)
	ctx.r.v3f_t2f(ix + iw, iy + st, 0, 1, v_t)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + st, 0, u_r, v_t)
	ctx.r.end()

	// middle row
	ctx.r.begin_quads()
	// mid-left
	ctx.r.v3f_t2f(ix, iy + st, 0, 0, v_t)
	ctx.r.v3f_t2f(ix + sl, iy + st, 0, u_l, v_t)
	ctx.r.v3f_t2f(ix + sl, iy + st + mid_h, 0, u_l, v_b)
	ctx.r.v3f_t2f(ix, iy + st + mid_h, 0, 0, v_b)
	// center
	ctx.r.v3f_t2f(ix + sl, iy + st, 0, u_l, v_t)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + st, 0, u_r, v_t)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + st + mid_h, 0, u_r, v_b)
	ctx.r.v3f_t2f(ix + sl, iy + st + mid_h, 0, u_l, v_b)
	// mid-right
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + st, 0, u_r, v_t)
	ctx.r.v3f_t2f(ix + iw, iy + st, 0, 1, v_t)
	ctx.r.v3f_t2f(ix + iw, iy + st + mid_h, 0, 1, v_b)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + st + mid_h, 0, u_r, v_b)
	ctx.r.end()

	// bottom row
	ctx.r.begin_quads()
	// bot-left
	ctx.r.v3f_t2f(ix, iy + st + mid_h, 0, 0, v_b)
	ctx.r.v3f_t2f(ix + sl, iy + st + mid_h, 0, u_l, v_b)
	ctx.r.v3f_t2f(ix + sl, iy + ih, 0, u_l, 1)
	ctx.r.v3f_t2f(ix, iy + ih, 0, 0, 1)
	// bot-center
	ctx.r.v3f_t2f(ix + sl, iy + st + mid_h, 0, u_l, v_b)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + st + mid_h, 0, u_r, v_b)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + ih, 0, u_r, 1)
	ctx.r.v3f_t2f(ix + sl, iy + ih, 0, u_l, 1)
	// bot-right
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + st + mid_h, 0, u_r, v_b)
	ctx.r.v3f_t2f(ix + iw, iy + st + mid_h, 0, 1, v_b)
	ctx.r.v3f_t2f(ix + iw, iy + ih, 0, 1, 1)
	ctx.r.v3f_t2f(ix + sl + mid_w, iy + ih, 0, u_r, 1)
	ctx.r.end()

	ctx.r.disable_texture()
}

// draw_rounded_rect_border  draws a rounded rectangle with a border using the given parameters.
// when border width < 1 or the color is transparent, draws a borderless filled rounded rectangle.
// when the background is transparent, draws a hollow rounded rectangle with only a border.
// `x`,`y` is the top-left corner of the rectangle.
// `w` is the width, `h` is the height.
// `r` is the radius of the corner-rounding in pixels.
// `border_w` is the width of the border,This implementation uses the mainstream inner border scheme (extending inward).
// `border_c` is the color of the border.
// `bg` is the background or fill color of the rounded rectangle,supporting transparent colors.
pub fn (ctx &Context) draw_rounded_rect_border(x f32, y f32, w f32, h f32, r f32, border_w f32, border_c Color, bg Color) {
	if w <= 0 || h <= 0 {
		return
	}

	mut new_r := r
	if r < 1 {
		new_r = 0
	}
	if w >= h && r > h / 2 {
		new_r = h / 2
	} else if r > w / 2 {
		new_r = w / 2
	}

	mut bw := border_w
	if border_w < 1 {
		bw = 0
	}
	if w >= h && border_w > h / 2 {
		bw = h / 2
	} else if border_w > w / 2 {
		bw = w / 2
	}

	sx := x * ctx.scale // start point x
	sy := y * ctx.scale
	width := w * ctx.scale
	height := h * ctx.scale
	new_r = new_r * ctx.scale
	bw = bw * ctx.scale

	// draw background
	ctx.draw_rounded_rect_filled(x, y, w, h, r, bg)

	// draw border
	if bw == 0 || border_c.a == 0 {
		return
	}
	if border_c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(border_c.r, border_c.g, border_c.b, border_c.a)

	// circle center coordinates
	ltx := sx + new_r
	lty := sy + new_r
	rtx := sx + width - new_r
	rty := sy + new_r
	rbx := sx + width - new_r
	rby := sy + height - new_r
	lbx := sx + new_r
	lby := sy + height - new_r

	offset := math.abs(bw - new_r)
	mut rad := f32(0)
	mut dx := f32(0)
	mut dy := f32(0)

	ctx.r.begin_triangle_strip()

	if new_r == 0 {
		// top
		ctx.r.v2f(ltx, lty)
		ctx.r.v2f(ltx + offset, lty + offset)
		ctx.r.v2f(rtx, rty)
		ctx.r.v2f(rtx - offset, rty + offset)
		// right
		ctx.r.v2f(rbx, rby)
		ctx.r.v2f(rbx - offset, rby - offset)
		// bottom
		ctx.r.v2f(lbx, lby)
		ctx.r.v2f(lbx + offset, lby - offset)
		// left
		ctx.r.v2f(ltx, lty)
		ctx.r.v2f(ltx + offset, lty + offset)
		ctx.r.end()
		return
	}

	if bw < new_r {
		// left top
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = new_r * math.cosf(rad)
			dy = new_r * math.sinf(rad)
			ctx.r.v2f(ltx - dx, lty - dy)
			dx = offset * math.cosf(rad)
			dy = offset * math.sinf(rad)
			ctx.r.v2f(ltx - dx, lty - dy)
		}

		// right top
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = new_r * math.sinf(rad)
			dy = new_r * math.cosf(rad)
			ctx.r.v2f(rtx + dx, rty - dy)
			dx = offset * math.sinf(rad)
			dy = offset * math.cosf(rad)
			ctx.r.v2f(rtx + dx, rty - dy)
		}
		// right bottom
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = new_r * math.cosf(rad)
			dy = new_r * math.sinf(rad)
			ctx.r.v2f(rbx + dx, rby + dy)
			dx = offset * math.cosf(rad)
			dy = offset * math.sinf(rad)
			ctx.r.v2f(rbx + dx, rby + dy)
		}
		// left bottom
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = new_r * math.sinf(rad)
			dy = new_r * math.cosf(rad)
			ctx.r.v2f(lbx - dx, lby + dy)
			dx = offset * math.sinf(rad)
			dy = offset * math.cosf(rad)
			ctx.r.v2f(lbx - dx, lby + dy)
		}
		// close
		ctx.r.v2f(ltx - new_r, lty)
		ctx.r.v2f(ltx - offset, lty)
	} else {
		// left top
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = new_r * math.cosf(rad)
			dy = new_r * math.sinf(rad)
			ctx.r.v2f(ltx - dx, lty - dy)
			dx = offset * math.cosf(rad)
			dy = offset * math.sinf(rad)
			ctx.r.v2f(ltx + offset, lty + offset)
		}

		// right top
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = new_r * math.sinf(rad)
			dy = new_r * math.cosf(rad)
			ctx.r.v2f(rtx + dx, rty - dy)
			dx = offset * math.sinf(rad)
			dy = offset * math.cosf(rad)
			ctx.r.v2f(rtx - offset, rty + offset)
		}
		// right bottom
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = new_r * math.cosf(rad)
			dy = new_r * math.sinf(rad)
			ctx.r.v2f(rbx + dx, rby + dy)
			dx = offset * math.cosf(rad)
			dy = offset * math.sinf(rad)
			ctx.r.v2f(rbx - offset, rby - offset)
		}
		// left bottom
		for i in 0 .. 31 {
			rad = f32(math.radians(i * 3))
			dx = new_r * math.sinf(rad)
			dy = new_r * math.cosf(rad)
			ctx.r.v2f(lbx - dx, lby + dy)
			dx = offset * math.sinf(rad)
			dy = offset * math.cosf(rad)
			ctx.r.v2f(lbx + offset, lby - offset)
		}
		// close
		ctx.r.v2f(ltx - new_r, lty)
		ctx.r.v2f(ltx + offset, lty + offset)
	}

	ctx.r.end()
}

// draw_triangle_empty draws the outline of a triangle.
// `x`,`y` defines the first point
// `x2`,`y2` defines the second point
// `x3`,`y3` defines the third point
// `c` is the color of the outline.
pub fn (ctx &Context) draw_triangle_empty(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	ctx.r.begin_line_strip()
	ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	ctx.r.v2f(x2 * ctx.scale, y2 * ctx.scale)
	ctx.r.v2f(x3 * ctx.scale, y3 * ctx.scale)
	ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	ctx.r.end()
}

// draw_triangle_filled draws a filled triangle.
// `x`,`y` defines the first point
// `x2`,`y2` defines the second point
// `x3`,`y3` defines the third point
// `c` is the color of the outline.
pub fn (ctx &Context) draw_triangle_filled(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	ctx.r.begin_triangles()
	ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	ctx.r.v2f(x2 * ctx.scale, y2 * ctx.scale)
	ctx.r.v2f(x3 * ctx.scale, y3 * ctx.scale)
	ctx.r.end()
}

// draw_square_empty draws the outline of a square.
// `x`,`y` is the top-left corner of the square.
// `s` is the length of each side of the square.
// `c` is the color of the outline.
@[inline]
pub fn (ctx &Context) draw_square_empty(x f32, y f32, s f32, c Color) {
	ctx.draw_rect_empty(x, y, s, s, c)
}

// draw_square_filled draws a filled square.
// `x`,`y` is the top-left corner of the square.
// `s` is the length of each side of the square.
// `c` is the fill color.
@[inline]
pub fn (ctx &Context) draw_square_filled(x f32, y f32, s f32, c Color) {
	ctx.draw_rect_filled(x, y, s, s, c)
}

// The table here is derived by looking at the result of vlib/gg/testdata/tweak_circles.vv
// and then choosing the most circle-ish drawing with the minimum number of segments.
const small_circle_segments = [0, 2, 4, 6, 6, 8, 8, 10, 13, 18, 12, 12, 10, 13, 16, 15, 16]!

@[direct_array_access]
fn radius_to_segments(r f32) int {
	if r < 30 {
		ir := int(math.ceil(r))
		if ir > 0 && ir < small_circle_segments.len {
			return small_circle_segments[ir]
		}
		return ir
	}
	return int(math.ceil(math.tau * r / 8))
}

// draw_circle_empty draws the outline of a circle.
// `x`,`y` defines the center of the circle.
// `radius` defines the radius of the circle.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_circle_empty(x f32, y f32, radius f32, c Color) {
	$if macos {
		if ctx.native_rendering {
			darwin_draw_circle_empty(x - radius + 1, ctx.height - (y + radius + 3), radius, c)
			return
		}
	}
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := radius * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	segments := radius_to_segments(radius * ctx.scale)
	if segments <= 0 {
		// Nothing to emit: avoids a division by zero in the loop below.
		return
	}

	ctx.r.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		theta = f32(math.tau) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		ctx.r.v2f(xx + nx, yy + ny)
	}
	ctx.r.end()
}

// draw_circle_filled draws a filled circle.
// `x`,`y` defines the center of the circle.
// `radius` defines the radius of the circle.
// `c` is the fill color.
pub fn (ctx &Context) draw_circle_filled(x f32, y f32, radius f32, c Color) {
	$if macos {
		if ctx.native_rendering {
			darwin_draw_circle(x - radius + 1, ctx.height - (y + radius + 3), radius, c)
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
pub fn (ctx &Context) draw_polygon_filled(x f32, y f32, size f32, edges int, rotation f32, c Color) {
	if edges <= 0 {
		return
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := size * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)

	ctx.r.begin_triangle_strip()
	for i := 0; i < edges + 1; i++ {
		theta = f32(math.tau) * f32(i) / f32(edges)
		xx = nr * math.cosf(theta + f32(math.radians(rotation)))
		yy = nr * math.sinf(theta + f32(math.radians(rotation)))
		ctx.r.v2f(xx + nx, yy + ny)
		ctx.r.v2f(nx, ny)
	}
	ctx.r.end()
}

// draw_circle_with_segments draws a filled circle with a specific number of segments.
// `x`,`y` defines the center of the circle.
// `radius` defines the radius of the circle.
// `segments` affects how smooth/round the circle is.
// `c` is the fill color.
pub fn (ctx &Context) draw_circle_with_segments(x f32, y f32, radius f32, segments int, c Color) {
	ctx.draw_polygon_filled(x, y, radius, segments, 0, c)
}

// draw_circle_line draws the outline of a circle with a specific number of segments.
// `x`,`y` defines the center of the circle.
// `radius` defines the radius of the circle.
// `segments` affects how smooth/round the circle is.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_circle_line(x f32, y f32, radius int, segments int, c Color) {
	if segments <= 0 {
		return
	}

	$if macos {
		if ctx.native_rendering {
			darwin_draw_circle(x - radius + 1, ctx.height - (y + radius + 3), radius, c)
			return
		}
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := radius * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)

	ctx.r.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		theta = f32(math.tau) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		ctx.r.v2f(xx + nx, yy + ny)
	}
	ctx.r.end()
}

// draw_slice_empty draws the outline of a circle slice/pie
pub fn (ctx &Context) draw_slice_empty(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int,
	c Color) {
	if segments <= 0 || radius <= 0 {
		return
	}
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := radius * ctx.scale * math.sinf(start_angle)
	mut yy := radius * ctx.scale * math.cosf(start_angle)

	ctx.r.begin_line_strip()
	ctx.r.v2f(nx, ny)
	for i := 0; i < segments + 1; i++ {
		ctx.r.v2f(xx + nx, yy + ny)
		xx, yy = xx + yy * tan_factor, yy - xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
	}
	ctx.r.v2f(nx, ny)
	ctx.r.end()
}

// draw_slice_filled draws a filled circle slice/pie
// `x`,`y` defines the end point of the slice (center of the circle that the slice is part of).
// `radius` defines the radius ("length") of the slice.
// `start_angle` is the angle in radians at which the slice starts.
// `end_angle` is the angle in radians at which the slice ends.
// `segments` affects how smooth/round the slice is.
// `c` is the fill color.
pub fn (ctx &Context) draw_slice_filled(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int,
	c Color) {
	if segments <= 0 || radius < 0 {
		return
	}
	if start_angle == end_angle {
		ctx.draw_slice_empty(x, y, radius, start_angle, end_angle, 1, c)
		return
	}

	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := radius * ctx.scale * math.sinf(start_angle)
	mut yy := radius * ctx.scale * math.cosf(start_angle)

	ctx.r.begin_triangle_strip()
	ctx.r.v2f(xx + nx, yy + ny)
	for i := 0; i < segments; i++ {
		xx, yy = xx + yy * tan_factor, yy - xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
		ctx.r.v2f(xx + nx, yy + ny)
		ctx.r.v2f(nx, ny)
	}
	ctx.r.end()
}

// draw_arc_line draws a line arc.
// `x`,`y` defines the end point of the arc (center of the circle that the arc is part of).
// `radius` defines the radius of the arc (length from the center point where the arc is drawn).
// `start_angle` is the angle in radians at which the arc starts.
// `end_angle` is the angle in radians at which the arc ends.
// `segments` affects how smooth/round the arc is.
// `c` is the color of the arc/outline.
pub fn (ctx &Context) draw_arc_line(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int,
	c Color) {
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
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := radius * ctx.scale * math.sinf(start_angle)
	mut yy := radius * ctx.scale * math.cosf(start_angle)

	ctx.r.begin_line_strip()
	ctx.r.v2f(nx + xx, ny + yy)
	for i := 0; i < segments; i++ {
		xx, yy = xx + yy * tan_factor, yy - xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
		ctx.r.v2f(nx + xx, ny + yy)
	}
	ctx.r.end()
}

// draw_arc_empty draws the outline of an arc.
// `x`,`y` defines the end point of the arc (center of the circle that the arc is part of).
// `inner_radius` defines the radius of the arc (length from the center point where the arc is drawn).
// `thickness` defines how wide the arc is drawn.
// `start_angle` is the angle in radians at which the arc starts.
// `end_angle` is the angle in radians at which the arc ends.
// `segments` affects how smooth/round the arc is.
// `c` is the color of the arc outline.
pub fn (ctx &Context) draw_arc_empty(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32,
	segments int, c Color) {
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
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(end_angle - start_angle) / f32(segments)
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)

	ctx.r.begin_line_strip()

	// outer
	mut xx := outer_radius * ctx.scale * math.sinf(start_angle)
	mut yy := outer_radius * ctx.scale * math.cosf(start_angle)
	sxx, syy := xx, yy
	ctx.r.v2f(nx + xx, ny + yy)
	for i := 0; i < segments; i++ {
		xx, yy = xx + yy * tan_factor, yy - xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
		ctx.r.v2f(nx + xx, ny + yy)
	}

	// inner
	xx = inner_radius * ctx.scale * math.sinf(end_angle)
	yy = inner_radius * ctx.scale * math.cosf(end_angle)
	ctx.r.v2f(nx + xx, ny + yy)
	for i := 0; i < segments; i++ {
		xx, yy = xx - yy * tan_factor, yy + xx * tan_factor
		xx *= rad_factor
		yy *= rad_factor
		ctx.r.v2f(nx + xx, ny + yy)
	}

	ctx.r.v2f(nx + sxx, ny + syy)
	ctx.r.end()
}

// draw_arc_filled draws a filled arc.
// `x`,`y` defines the central point of the arc (center of the circle that the arc is part of).
// `inner_radius` defines the radius of the arc (length from the center point where the arc is drawn).
// `thickness` defines how wide the arc is drawn.
// `start_angle` is the angle in radians at which the arc starts.
// `end_angle` is the angle in radians at which the arc ends.
// `segments` affects how smooth/round the arc is.
// `c` is the fill color of the arc.
pub fn (ctx &Context) draw_arc_filled(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32,
	segments int, c Color) {
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
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

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

	ctx.r.begin_triangle_strip()
	ctx.r.v2f(nx + ix, ny + iy)
	ctx.r.v2f(nx + ox, ny + oy)
	for i := 0; i < segments; i++ {
		ix, iy = ix + iy * tan_factor, iy - ix * tan_factor
		ix *= rad_factor
		iy *= rad_factor
		ctx.r.v2f(nx + ix, ny + iy)
		ox, oy = ox + oy * tan_factor, oy - ox * tan_factor
		ox *= rad_factor
		oy *= rad_factor
		ctx.r.v2f(nx + ox, ny + oy)
	}
	ctx.r.end()
}

// draw_ellipse_empty draws the outline of an ellipse.
// `x`,`y` defines the center of the ellipse.
// `rw` defines the *width* radius of the ellipse.
// `rh` defines the *height* radius of the ellipse.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_ellipse_empty(x f32, y f32, rw f32, rh f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nrw := rw * ctx.scale
	nrh := rh * ctx.scale
	ctx.r.begin_line_strip()
	for i := 0; i < 360; i += 10 {
		ctx.r.v2f(nx + math.sinf(f32(math.radians(i))) * nrw, ny + math.cosf(f32(math.radians(i))) * nrh)
	}
	ctx.r.v2f(nx, ny + nrh)
	ctx.r.end()
}

// draw_ellipse_thick draws the outline of an ellipse.
// `x`,`y` defines the center of the ellipse.
// `rw` defines the *width* radius of the ellipse.
// `rh` defines the *height* radius of the ellipse.
// `th` defines the *thickness* of the ellipse.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_ellipse_thick(x f32, y f32, rw f32, rh f32, th f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nrw := rw * ctx.scale
	nrh := rh * ctx.scale
	nth := th * ctx.scale
	ctx.r.begin_triangle_strip()
	for i := 0; i < 360; i += 10 {
		xfactor := math.sinf(f32(math.radians(i)))
		yfactor := math.cosf(f32(math.radians(i)))

		ctx.r.v2f(nx + xfactor * (nrw - nth / 2), ny + yfactor * (nrh - nth / 2))
		ctx.r.v2f(nx + xfactor * (nrw + nth / 2), ny + yfactor * (nrh + nth / 2))
	}
	ctx.r.v2f(nx, ny + (nrh - nth / 2))
	ctx.r.v2f(nx, ny + (nrh + nth / 2))
	ctx.r.end()
}

// draw_ellipse_filled draws an opaque ellipse.
// `x`,`y` defines the center of the ellipse.
// `rw` defines the *width* radius of the ellipse.
// `rh` defines the *height* radius of the ellipse.
// `c` is the fill color.
pub fn (ctx &Context) draw_ellipse_filled(x f32, y f32, rw f32, rh f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nrw := rw * ctx.scale
	nrh := rh * ctx.scale
	ctx.r.begin_triangle_strip()
	for i := 0; i < 360; i += 10 {
		ctx.r.v2f(nx, ny)
		ctx.r.v2f(nx + math.sinf(f32(math.radians(i))) * nrw, ny + math.cosf(f32(math.radians(i))) * nrh)
	}
	ctx.r.v2f(nx, ny + nrh)
	ctx.r.end()
}

// draw_ellipse_empty_rotate draws the outline of an ellipse.
// `x`,`y` defines the center of the ellipse.
// `rw` defines the *width* radius of the ellipse.
// `rh` defines the *height* radius of the ellipse.
// `rota` defines the *rotation* angle of the ellipse, in radians.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_ellipse_empty_rotate(x f32, y f32, rw f32, rh f32, rota f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nrw := rw * ctx.scale
	nrh := rh * ctx.scale
	cos_rot := math.cosf(rota)
	sin_rot := math.sinf(rota)
	ctx.r.begin_line_strip()
	for i := 0; i < 360; i += 10 {
		x_current := math.sinf(f32(math.radians(i))) * nrw
		y_current := math.cosf(f32(math.radians(i))) * nrh

		ctx.r.v2f(nx + x_current * cos_rot - y_current * sin_rot, ny + x_current * sin_rot +
			y_current * cos_rot)
	}
	ctx.r.v2f(nx - nrh * sin_rot, ny + nrh * cos_rot)
	ctx.r.end()
}

// draw_ellipse_thick_rotate draws the outline of an ellipse.
// `x`,`y` defines the center of the ellipse.
// `rw` defines the *width* radius of the ellipse.
// `rh` defines the *height* radius of the ellipse.
// `th` defines the *thickness* of the ellipse.
// `rota` defines the *rotation* angle of the ellipse, in radians.
// `c` is the color of the outline.
pub fn (ctx &Context) draw_ellipse_thick_rotate(x f32, y f32, rw f32, rh f32, th f32, rota f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nrw := rw * ctx.scale
	nrh := rh * ctx.scale
	nth := th * ctx.scale
	cos_rot := math.cosf(rota)
	sin_rot := math.sinf(rota)
	ctx.r.begin_triangle_strip()
	for i := 0; i < 360; i += 10 {
		xfactor := math.sinf(f32(math.radians(i)))
		yfactor := math.cosf(f32(math.radians(i)))

		ctx.r.v2f(nx + xfactor * (nrw - nth / 2) * cos_rot - yfactor * (nrh - nth / 2) * sin_rot, ny +
			yfactor * (nrh - nth / 2) * cos_rot + xfactor * (nrw - nth / 2) * sin_rot)
		ctx.r.v2f(nx + xfactor * (nrw + nth / 2) * cos_rot - yfactor * (nrh + nth / 2) * sin_rot, ny +
			yfactor * (nrh + nth / 2) * cos_rot + xfactor * (nrw + nth / 2) * sin_rot)
	}
	ctx.r.v2f(nx - (nrh - nth / 2) * sin_rot, ny + (nrh - nth / 2) * cos_rot)
	ctx.r.v2f(nx - (nrh + nth / 2) * sin_rot, ny + (nrh + nth / 2) * cos_rot)
	ctx.r.end()
}

// draw_ellipse_filled_rotate draws an opaque rotated ellipse.
// `x`,`y` defines the center of the ellipse.
// `rw` defines the *width* radius of the ellipse.
// `rh` defines the *height* radius of the ellipse.
// `rota` defines the *rotation* angle of the ellipse, in radians.
// `c` is the fill color.
pub fn (ctx &Context) draw_ellipse_filled_rotate(x f32, y f32, rw f32, rh f32, rota f32, c Color) {
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	nx := x * ctx.scale
	ny := y * ctx.scale
	nrw := rw * ctx.scale
	nrh := rh * ctx.scale
	cos_rot := math.cosf(rota)
	sin_rot := math.sinf(rota)
	ctx.r.begin_triangle_strip()
	for i := 0; i < 360; i += 10 {
		ctx.r.v2f(nx, ny)
		x_current := math.sinf(f32(math.radians(i))) * nrw
		y_current := math.cosf(f32(math.radians(i))) * nrh
		ctx.r.v2f(nx + x_current * cos_rot - y_current * sin_rot, ny + x_current * sin_rot +
			y_current * cos_rot)
	}
	ctx.r.v2f(nx - nrh * sin_rot, ny + nrh * cos_rot)
	ctx.r.end()
}

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
	if steps <= 0 || steps >= 20000 || points.len != 8 {
		return
	}
	if c.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.r.c4b(c.r, c.g, c.b, c.a)

	ctx.r.begin_line_strip()

	p1_x, p1_y := points[0], points[1]
	p2_x, p2_y := points[6], points[7]

	ctrl_p1_x, ctrl_p1_y := points[2], points[3]
	ctrl_p2_x, ctrl_p2_y := points[4], points[5]

	// The constant 3 is actually points.len() - 1;

	step := f32(1) / steps
	ctx.r.v2f(p1_x * ctx.scale, p1_y * ctx.scale)
	for u := f32(0); u <= f32(1); u += step {
		pow_2_u := u * u
		pow_3_u := pow_2_u * u

		x := pow_3_u * (p2_x + 3 * (ctrl_p1_x - ctrl_p2_x) - p1_x) +
			3 * pow_2_u * (p1_x - 2 * ctrl_p1_x + ctrl_p2_x) + 3 * u * (ctrl_p1_x - p1_x) + p1_x

		y := pow_3_u * (p2_y + 3 * (ctrl_p1_y - ctrl_p2_y) - p1_y) +
			3 * pow_2_u * (p1_y - 2 * ctrl_p1_y + ctrl_p2_y) + 3 * u * (ctrl_p1_y - p1_y) + p1_y

		ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	}
	ctx.r.v2f(p2_x * ctx.scale, p2_y * ctx.scale)

	ctx.r.end()
}

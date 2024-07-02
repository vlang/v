module gg

import math
import sokol.sgl

// draw_cubic_bezier_recursive draws a cubic Bézier curve, also known as a spline, from four points,
// where the first and the last points, *will* be part of the curve, and the middle 2 points are control ones.
// Unlike `draw_cubic_bezier_in_steps`, this method does not use a fixed number of steps for the whole curve,
// but tries to produce more tesselation points dynamically for the curvier parts.
@[direct_array_access]
pub fn (ctx &Context) draw_cubic_bezier_recursive(points []f32, c Color) {
	if points.len < 8 {
		return
	}
	ctx.draw_cubic_bezier_recursive_scalar(points[0], points[1], points[2], points[3],
		points[4], points[5], points[6], points[7], c)
}

// draw_cubic_bezier_recursive_scalar is the same as `draw_cubic_bezier_recursive`, except that the `points` are given
// as indiviual x,y f32 scalar parameters, and not in a single dynamic array parameter.
pub fn (ctx &Context) draw_cubic_bezier_recursive_scalar(x1 f32, y1 f32, x2 f32, y2 f32, x3 f32, y3 f32,
	x4 f32, y4 f32, c Color) {
	if c.a == 0 {
		return
	}
	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	sgl.v2f(x1 * ctx.scale, y1 * ctx.scale)
	ctx.cubic_bezier_rec(x1, y1, x2, y2, x3, y3, x4, y4, 0)
	sgl.v2f(x4 * ctx.scale, y4 * ctx.scale)
	sgl.end()
}

// based on nsvg__flattenCubicBez, from https://github.com/memononen/nanosvg/ :
fn (ctx &Context) cubic_bezier_rec(x1 f32, y1 f32, x2 f32, y2 f32, x3 f32, y3 f32, x4 f32, y4 f32, level int) {
	if level > 10 {
		return
	}
	dx, dy := x4 - x1, y4 - y1
	d2 := math.abs((x2 - x4) * dy - (y2 - y4) * dx)
	d3 := math.abs((x3 - x4) * dy - (y3 - y4) * dx)
	if (d2 + d3) * (d2 + d3) < 0.25 * (dx * dx + dy * dy) {
		sgl.v2f(x4 * ctx.scale, y4 * ctx.scale)
		return
	}
	x12, y12 := 0.5 * (x1 + x2), 0.5 * (y1 + y2)
	x23, y23 := 0.5 * (x2 + x3), 0.5 * (y2 + y3)
	x34, y34 := 0.5 * (x3 + x4), 0.5 * (y3 + y4)
	x234, y234 := 0.5 * (x23 + x34), 0.5 * (y23 + y34)
	x123, y123 := 0.5 * (x12 + x23), 0.5 * (y12 + y23)
	x1234, y1234 := 0.5 * (x123 + x234), 0.5 * (y123 + y234)
	ctx.cubic_bezier_rec(x1, y1, x12, y12, x123, y123, x1234, y1234, level + 1)
	ctx.cubic_bezier_rec(x1234, y1234, x234, y234, x34, y34, x4, y4, level + 1)
}

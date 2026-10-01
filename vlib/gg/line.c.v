// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math
import sokol.sgl

// draw_line_with_thickness draws a centered line between the points `x,y` and `x2,y2`
// with the given `thickness` and color `c`.
pub fn (ctx &Context) draw_line_with_thickness(x f32, y f32, x2 f32, y2 f32, thickness f32, c Color) {
	if thickness <= 0 {
		return
	}

	dx := x2 - x
	dy := y2 - y
	length := math.sqrtf(dx * dx + dy * dy)
	if length <= 0 {
		return
	}

	if c.a != 255 {
		sgl.load_pipeline(ctx.pipeline.alpha)
	}

	radius_scale := thickness / (2.0 * length)
	radius_x := -radius_scale * dy
	radius_y := radius_scale * dx

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_triangle_strip()
	sgl.v2f((x - radius_x) * ctx.scale, (y - radius_y) * ctx.scale)
	sgl.v2f((x + radius_x) * ctx.scale, (y + radius_y) * ctx.scale)
	sgl.v2f((x2 - radius_x) * ctx.scale, (y2 - radius_y) * ctx.scale)
	sgl.v2f((x2 + radius_x) * ctx.scale, (y2 + radius_y) * ctx.scale)
	sgl.end()
}

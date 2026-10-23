// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gg.render

// draw_rect_filled_gradient draws a filled rectangle with a color specified for each corner.
// Colors are specified counter-clockwise: top-left, bottom-left, bottom-right, top-right.
pub fn (ctx &Context) draw_rect_filled_gradient(x f32, y f32, w f32, h f32, top_left Color, bottom_left Color, bottom_right Color, top_right Color) {
	if top_left.a != 255 || bottom_left.a != 255 || bottom_right.a != 255 || top_right.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}

	ctx.r.begin_quads()
	ctx.r.c4b(top_left.r, top_left.g, top_left.b, top_left.a)
	ctx.r.v2f(x * ctx.scale, y * ctx.scale)
	ctx.r.c4b(bottom_left.r, bottom_left.g, bottom_left.b, bottom_left.a)
	ctx.r.v2f(x * ctx.scale, (y + h) * ctx.scale)
	ctx.r.c4b(bottom_right.r, bottom_right.g, bottom_right.b, bottom_right.a)
	ctx.r.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	ctx.r.c4b(top_right.r, top_right.g, top_right.b, top_right.a)
	ctx.r.v2f((x + w) * ctx.scale, y * ctx.scale)
	ctx.r.end()
}

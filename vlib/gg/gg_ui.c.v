module gg

import math

// Stuff for ui from now for screenshot (that would be interesting for gg if screenshot is implemented also for gg)

// required for ui.DrawDevice interface (with &gg.Context as an instance)
pub fn (ctx &Context) scissor_rect(x int, y int, w int, h int) {
	ctx.r.scissor_rect(int(ctx.scale * x), int(ctx.scale * y), int(ctx.scale * w),
		int(ctx.scale * h), true)
}

struct PixelScissorRect {
	x      int
	y      int
	width  int
	height int
}

fn pixel_scissor_rect(x f64, y f64, w f64, h f64, scale f64) PixelScissorRect {
	s := if scale > 0 { scale } else { 1.0 }
	x0 := int(math.floor(x * s))
	y0 := int(math.floor(y * s))
	x1 := int(math.ceil((x + math.max(w, 0)) * s))
	y1 := int(math.ceil((y + math.max(h, 0)) * s))
	return PixelScissorRect{
		x:      x0
		y:      y0
		width:  math.max(0, x1 - x0)
		height: math.max(0, y1 - y0)
	}
}

// scissor_rectf converts logical clip edges to framebuffer pixels once,
// rounding the outer edges away from the clip. Scaling an integer origin and
// extent separately truncates twice and can leave a one- or two-pixel seam.
pub fn (ctx &Context) scissor_rectf(x f64, y f64, w f64, h f64) {
	clip := pixel_scissor_rect(x, y, w, h, f64(ctx.scale))
	ctx.r.scissor_rect(clip.x, clip.y, clip.width, clip.height, true)
}

// empty function
pub fn (ctx &Context) has_text_style() bool {
	return false
}

// empty function
pub fn (ctx &Context) set_text_style(font_name string, font_path string, size int, color Color, align int,
	vertical_align int) {
}

// default draw_text (draw_text_def but without set_text_cfg)
pub fn (ctx &Context) draw_text_default(x int, y int, text string) {
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	ctx.ft.fons.draw_text(x * scale, y * scale, text) // TODO: check offsets/alignment
}

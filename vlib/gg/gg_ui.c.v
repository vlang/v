module gg

import math

// Stuff for ui from now for screenshot (that would be interesting for gg if screenshot is implemented also for gg)

fn scissor_rect_screen_axis(scale f32, start int, size int) (int, int) {
	if size <= 0 {
		return int(scale * start), 0
	}
	lower, upper := rect_filled_screen_axis(scale, start, size)
	origin := int(math.floor(lower))
	return origin, int(math.ceil(upper)) - origin
}

// scissor_rect clips drawing to the given rectangle in logical pixels.
// The pixel bounds enclose the scaled rectangle, using the same alignment as filled
// rectangles for one-unit dimensions. Nonpositive dimensions produce an empty clip.
// required for ui.DrawDevice interface (with &gg.Context as an instance)
pub fn (ctx &Context) scissor_rect(x int, y int, w int, h int) {
	sx, sw := scissor_rect_screen_axis(ctx.scale, x, w)
	sy, sh := scissor_rect_screen_axis(ctx.scale, y, h)
	ctx.r.scissor_rect(sx, sy, sw, sh, true)
}

struct PixelScissorRect {
	x      int
	y      int
	width  int
	height int
}

fn pixel_scissor_rect(x f64, y f64, w f64, h f64, scale f64) PixelScissorRect {
	if w <= 0 || h <= 0 {
		return PixelScissorRect{}
	}
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

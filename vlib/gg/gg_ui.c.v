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

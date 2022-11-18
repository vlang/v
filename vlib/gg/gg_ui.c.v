module gg

import gx
import sokol.sgl

// Stuff for ui from now for screenshot (that would be interesting for gg if screenshot is implemented also for gg)

// required for ui.DrawDevice interface (with &gg.Context as an instance)
pub fn (ctx &Context) scissor_rect(x int, y int, w int, h int) {
	sgl.scissor_rect(int(dpi_scale() * x), int(dpi_scale() * y), int(dpi_scale() * w),
		int(dpi_scale() * h), true)
}

pub fn (ctx &Context) has_text_style() bool {
	return false
}

pub fn (ctx &Context) set_text_style(font_name string, font_path string, size int, color gx.Color, align int, vertical_align int) {}

// default draw_text (draw_text_def but without set_text_cfg)
pub fn (ctx &Context) draw_text_default(x int, y int, text string) {
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	ctx.ft.fons.draw_text(x * scale, y * scale, text) // TODO: check offsets/alignment
}

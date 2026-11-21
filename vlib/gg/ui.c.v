module gg

import math

// ui_panel draws a filled panel with a thin top and left border.
pub fn ui_panel(ctx &Context, r Rect, theme UITheme) {
	ctx.draw_rect_filled(r.x, r.y, r.width, r.height, theme.panel)
	ctx.draw_rect_filled(r.x, r.y, r.width, 1, theme.border)
	ctx.draw_rect_filled(r.x, r.y, 1, r.height, theme.border)
}

// ui_button draws a button and reports whether it was clicked this frame.
// Pass the current mouse position and the just-pressed flag from InputState.
pub fn ui_button(ctx &Context, r Rect, label string, mouse Vec2, clicked bool, theme UITheme) bool {
	hover := ui_hit(r, mouse)
	bg := if hover { theme.panel_hover } else { theme.panel }
	ctx.draw_rect_filled(r.x, r.y, r.width, r.height, bg)
	ctx.draw_rect_filled(r.x, r.y, r.width, 1, theme.border)
	ctx.draw_rect_filled(r.x, r.y, 1, r.height, theme.border)
	ctx.draw_text_def(int(r.x + theme.padding), int(r.y + theme.padding), label)
	return hover && clicked
}

// ui_slider draws a horizontal slider bar filled to value.
pub fn ui_slider(ctx &Context, r Rect, value f32, min f32, max f32, theme UITheme) {
	bar_height := f32(4)
	cy := r.y + r.height / 2
	ctx.draw_rect_filled(r.x, cy - bar_height / 2, r.width, bar_height, theme.panel_hover)
	t := if max > min { math.clip((value - min) / (max - min), 0, 1) } else { f32(0) }
	ctx.draw_rect_filled(r.x, cy - bar_height / 2, r.width * t, bar_height, theme.accent)
}

// ui_toggle draws a small on/off switch.
pub fn ui_toggle(ctx &Context, r Rect, on bool, theme UITheme) {
	bg := if on { theme.accent } else { theme.panel_hover }
	ctx.draw_rect_filled(r.x, r.y, r.width, r.height, bg)
	knob_x := if on { r.x + r.width / 2 } else { r.x }
	ctx.draw_rect_filled(knob_x, r.y, r.width / 2, r.height, theme.text)
}

// ui_hud draws a panel with one text line per entry.
pub fn ui_hud(ctx &Context, lines []string, r Rect, theme UITheme) {
	ui_panel(ctx, r, theme)
	for i, line in lines {
		y := int(r.y + theme.padding + f32(i) * 16)
		ctx.draw_text_def(int(r.x + theme.padding), y, line)
	}
}

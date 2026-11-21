module fx

import gg

// draw_line_chart plots values as a polyline inside r with an x axis. Values
// are scaled to their own min/max; fewer than two points draws nothing.
pub fn draw_line_chart(ctx &gg.Context, values []f32, r gg.Rect, color gg.Color) {
	if values.len < 2 || r.width <= 0 || r.height <= 0 {
		return
	}
	lo, hi := chart_min_max(values)
	norm := chart_normalize(values, lo, hi)
	ctx.draw_rect_filled(r.x, r.y + r.height - 1, r.width, 1, gg.Color{90, 98, 120, 255})
	step := r.width / f32(values.len - 1)
	for i in 1 .. values.len {
		x1 := r.x + step * f32(i - 1)
		y1 := r.y + r.height - norm[i - 1] * r.height
		x2 := r.x + step * f32(i)
		y2 := r.y + r.height - norm[i] * r.height
		ctx.draw_line(x1, y1, x2, y2, color)
	}
}

// draw_bar_chart plots values as vertical bars. Colors are matched by index;
// entries without a color use the theme accent.
pub fn draw_bar_chart(ctx &gg.Context, values []f32, r gg.Rect, colors []gg.Color) {
	if values.len == 0 || r.width <= 0 || r.height <= 0 {
		return
	}
	lo, hi := chart_min_max(values)
	norm := chart_normalize(values, lo, hi)
	step := r.width / f32(values.len)
	for i, v in norm {
		bar_height := v * r.height
		c := if i < colors.len { colors[i] } else { accent_color() }
		ctx.draw_rect_filled(r.x + step * f32(i) + 1, r.y + r.height - bar_height, step - 2,
			bar_height, c)
	}
}

fn accent_color() gg.Color {
	return default_ui_theme().accent
}

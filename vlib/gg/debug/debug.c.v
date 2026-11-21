module debug

import gg
import gg.fx

// draw renders all queued gizmos. Callers usually call clear() afterwards.
pub fn (g &Gizmos) draw(ctx &gg.Context) {
	for item in g.items {
		match item.kind {
			.line {
				ctx.draw_line(item.a.x, item.a.y, item.b.x, item.b.y, item.color)
			}
			.circle {
				ctx.draw_circle_empty(item.a.x, item.a.y, item.b.x, item.color)
			}
			.rect {
				ctx.draw_line(item.a.x, item.a.y, item.b.x, item.a.y, item.color)
				ctx.draw_line(item.b.x, item.a.y, item.b.x, item.b.y, item.color)
				ctx.draw_line(item.b.x, item.b.y, item.a.x, item.b.y, item.color)
				ctx.draw_line(item.a.x, item.b.y, item.a.x, item.a.y, item.color)
			}
			.cross {
				half := item.b.x / 2
				ctx.draw_line(item.a.x - half, item.a.y, item.a.x + half, item.a.y, item.color)
				ctx.draw_line(item.a.x, item.a.y - half, item.a.x, item.a.y + half, item.color)
			}
		}
	}
}

// draw draws the newest log lines into a panel using the given theme.
pub fn (l &DebugLog) draw(ctx &gg.Context, r gg.Rect, theme fx.UITheme) {
	lines := l.recent(int(r.height / 16) - 1)
	mut all := ['console']
	all << lines
	fx.ui_hud(ctx, all, r, theme)
}

// draw renders the frame time history as a small bar graph.
pub fn (g &FrameGraph) draw(ctx &gg.Context, r gg.Rect, color gg.Color) {
	if g.samples.len == 0 || r.width <= 0 || r.height <= 0 {
		return
	}
	peak := g.worst()
	if peak <= 0 {
		return
	}
	step := r.width / f32(g.samples.len)
	for i, sample in g.samples {
		h := sample / peak * r.height
		ctx.draw_rect_filled(r.x + step * f32(i), r.y + r.height - h, step - 1, h, color)
	}
}

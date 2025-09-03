module main

import gg
import sokol.sgl

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

const cmax_x = 115
const cmax_y = 81
const cs = 10

const c_fg = gg.rgb(20, 30, 255)
const c_em = gg.rgb(255, 50, 0)

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		bg_color:      gg.white
		width:         cmax_x * cs
		height:        cmax_y * cs
		create_window: true
		window_title:  'Grid with many rectangles (drawn with no context)'
		frame_fn:      frame
		user_data:     app
	)
	app.gg.run()
}

fn frame(app &App) {
	app.gg.begin()
	// Note: this uses 2 separate loops, with each doing its own drawing for performance reasons.
	// The underlying Sokol library can eliminate sgl begin/end calls, when multiple primitives of the
	// same kind are drawn one after the other, without context changes.
	// However, filled rectangles are drawn with quads, while empty rectangles are drawn with lines.
	// In this case, if the draw calls are put in the same loop, using app.gg.draw_rect_filled/5 and app.gg.draw_rect_empty/5,
	// sokol can not batch the begin/end calls, and their overhead becomes much bigger.
	//
	// That is why:
	// a) we use several loops here.
	// b) we use a single sgl.begin_quads() before the first loop, and a single sgl.end() after it.
	// c) we use draw_rect_filled_no_context/5 inside the loop, instead of draw_rect_filled/5 .
	// e) we use a single sgl.begin_lines() before the second loop, and a single sgl.end() after it.
	// f) we use draw_rect_empty_no_context/5 inside the second loop, instead of draw_rect_empty/5 .
	// Note: the separation of the loops/kinds of draws, is several times more important for eliminating
	// the performance overhead (12-15% CPU usage), compared to the use of draw_rect_filled_no_context
	// vs draw_rect_filled (1-2% CPU usage), because of Sokol's optimisation.

	$if !do_not_draw_rect_filled ? {
		sgl.begin_quads()
		for y in 0 .. cmax_y {
			for x in 0 .. cmax_x {
				cy, cx := y * cs, x * cs
				app.gg.draw_rect_filled_no_context(cx, cy, cs - 2, cs - 2, c_fg)
			}
		}
		sgl.end()
	}

	$if draw_rect_empty ? {
		sgl.begin_lines()
		for y in 0 .. cmax_y {
			for x in 0 .. cmax_x {
				cy, cx := y * cs, x * cs
				app.gg.draw_rect_empty_no_context(cx + 2, cy + 2, cs - 3, cs - 3, c_em)
			}
		}
		sgl.end()
	}

	app.gg.end()
}

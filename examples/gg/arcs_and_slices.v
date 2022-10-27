module main

import gg
import gx
import math

const (
	win_width  = 700
	win_height = 800
	bg_color   = gx.white
	colour     = gx.black
)

enum Selection {
	segs = 0
	len
}

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	mouse struct {
	mut:
		x f32
		y f32
	}

	sel  Selection
	segs int = 8
}

fn main() {
	mut app := &App{
		gg: 0
	}
	app.gg = gg.new_context(
		width: win_width
		height: win_height
		create_window: true
		window_title: 'Arcs and Slices'
		user_data: app
		bg_color: bg_color
		frame_fn: on_frame
		event_fn: on_event
	)
	app.gg.run()
}

fn on_frame(mut app App) {
	app.gg.begin()

	start := math.tau * app.mouse.y / (win_width * app.gg.scale)
	end := math.tau * app.mouse.x / (win_width * app.gg.scale)

	segs := if app.sel == .segs { '[$app.segs]' } else { '$app.segs' }
	app.gg.draw_text_def(10, 10, 'Segments: $segs')
	app.gg.draw_text_def(250, 10, 'Drawing Angles (radians)')
	app.gg.draw_text_def(200, 26, 'Start: $start°')
	app.gg.draw_text_def(350, 26, 'End: $end°')
	mut x, mut y := 0, -80

	y += 150
	x = 20
	app.gg.draw_text_def(10, y + 40, 'slice')
	x += 150
	app.gg.draw_slice_empty(x, y + 60, 50, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=50 empty')
	x += 150
	app.gg.draw_slice_empty(x, y + 60, 0, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=0 empty')
	x += 150
	app.gg.draw_slice_filled(x, y + 60, 50, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=50 filled')
	x += 150
	app.gg.draw_slice_filled(x, y + 60, 0, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=0 filled')

	y += 150
	x = 20
	app.gg.draw_text_def(10, y + 40, 'arc_empty')
	x += 150
	app.gg.draw_arc_empty(x, y + 60, 30, 20, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=[30,50]')
	x += 150
	app.gg.draw_arc_empty(x, y + 60, -10, 60, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=[-10,50]')
	x += 150
	app.gg.draw_arc_empty(x, y + 60, 50, 0, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=[50,50]')
	x += 150
	app.gg.draw_arc_empty(x, y + 60, 0, 0, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=[0,0]')

	y += 150
	x = 20
	app.gg.draw_text_def(10, y + 40, 'arc_filled')
	x += 150
	app.gg.draw_arc_filled(x, y + 60, 30, 20, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=[30,50]')
	x += 150
	app.gg.draw_arc_filled(x, y + 60, -10, 60, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=[-10,50]')
	x += 150
	app.gg.draw_arc_filled(x, y + 60, 50, 0, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=[50,50]')
	x += 150
	app.gg.draw_arc_filled(x, y + 60, 0, 0, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=[0,0]')

	y += 150
	x = 20
	app.gg.draw_text_def(10, y + 40, 'arc_line')
	x += 150
	app.gg.draw_arc_line(x, y + 60, 50, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=50')
	x += 150
	app.gg.draw_arc_line(x, y + 60, 0, start, end, app.segs, colour)
	app.gg.draw_text_def(x - 50, y + 120, 'r=0')

	y += 150
	app.gg.draw_text_def(10, y + 20, 'Use arrow keys to increase/decrease number of segments.')
	app.gg.draw_text_def(10, y + 36, 'Use the mouse to adjust the start/end angles, in radians. Mouse position (0,0) is at the top-left of the window.')
	app.gg.draw_text_def(10, y + 52, 'Note: because y=0 is at the top of the screen and not the bottom, angle=0 is at the bottom of an arc, not the top!')
	app.gg.draw_text_def(10, y + 68, 'Compared to a graph, where y=0 is at the bottom, arcs therefore appear y-flipped.')

	app.gg.end()
}

fn on_event(e &gg.Event, mut app App) {
	match e.typ {
		.key_down {
			match e.key_code {
				.escape {
					app.gg.quit()
				}
				.up {
					app.sel = unsafe { Selection(math.max(0, int(app.sel) - 1)) }
				}
				.down {
					app.sel = unsafe { Selection(math.min(int(Selection.len) - 1, int(app.sel) + 1)) }
				}
				.left {
					match app.sel {
						.segs {
							app.segs = math.max(1, app.segs / 2)
						}
						else {}
					}
				}
				.right {
					match app.sel {
						.segs {
							app.segs = math.min(64, app.segs * 2)
						}
						else {}
					}
				}
				else {}
			}
		}
		.mouse_move {
			app.mouse.x = e.mouse_x
			app.mouse.y = e.mouse_y
		}
		else {}
	}
}

import term.ui as tui

const (
	colors = [
		tui.Color{33, 150, 243},
		tui.Color{0, 150, 136},
		tui.Color{205, 220, 57},
		tui.Color{255, 152, 0},
		tui.Color{244, 67, 54},
		tui.Color{156, 39, 176},
	]
)

struct Point {
	x int
	y int
}

struct App {
mut:
	tui       &tui.Context = unsafe { nil }
	points    []Point
	color     tui.Color = colors[0]
	color_idx int
	cut_rate  f64 = 5
}

fn frame(x voidptr) {
	mut app := &App(x)

	app.tui.clear()

	if app.points.len > 0 {
		app.tui.set_bg_color(app.color)
		mut last := app.points[0]
		for point in app.points {
			// if the cursor moves quickly enough, different events are not
			// necessarily touching, so we need to draw a line between them
			app.tui.draw_line(last.x, last.y, point.x, point.y)
			last = point
		}
		app.tui.reset()

		l := int(app.points.len / app.cut_rate) + 1
		app.points = app.points[l..].clone()
	}

	ww := app.tui.window_width

	app.tui.bold()
	app.tui.draw_text(ww / 6, 2, 'V term.input: cursor chaser demo')
	app.tui.draw_text((ww - ww / 6) - 14, 2, 'cut rate: ${(100 / app.cut_rate):3.0f}%')
	app.tui.horizontal_separator(3)
	app.tui.reset()
	app.tui.flush()
}

fn event(e &tui.Event, x voidptr) {
	mut app := &App(x)

	match e.typ {
		.key_down {
			match e.code {
				.escape {
					exit(0)
				}
				.space, .enter {
					app.color_idx++
					if app.color_idx == colors.len {
						app.color_idx = 0
					}
					app.color = colors[app.color_idx]
				}
				else {}
			}
		}
		.mouse_move, .mouse_drag, .mouse_down {
			app.points << Point{e.x, e.y}
		}
		.mouse_scroll {
			d := if e.direction == .up { 0.1 } else { -0.1 }
			app.cut_rate += d
			if app.cut_rate < 1 {
				app.cut_rate = 1
			}
		}
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.tui = tui.init(
		user_data: app
		frame_fn: frame
		event_fn: event
		hide_cursor: true
	)
	app.tui.run()?
}

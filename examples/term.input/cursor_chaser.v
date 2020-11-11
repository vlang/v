import term.input as ti

struct Point {
	x int
	y int
}

const (
	colors = [
		ti.Color{33, 150, 243}
		ti.Color{0, 150, 136}
		ti.Color{205, 220, 57}
		ti.Color{255, 152, 0}
		ti.Color{244, 67, 54}
		ti.Color{156, 39, 176}
	]
)

struct App {
mut:
	ti        &ti.Context = 0
	points    []Point
	color     ti.Color = colors[0]
	color_idx int
	cut_rate  f64 = 5
}

fn frame(x voidptr) {
	mut app := &App(x)

	app.ti.clear()

	if app.points.len > 0 {
		app.ti.set_bg_color(app.color)
		mut last := app.points[0]
		for segment in app.points {
			// if the cursor moveds quickly enough, different events are not
			// necessarily touching, so we need to draw a line between them
			app.ti.draw_line(last.x, last.y, segment.x, segment.y)
			last = segment
		}
		app.ti.reset()

		l := int(app.points.len / app.cut_rate) + 1
		app.points = app.points[l..].clone()
	}

	ww := app.ti.window_width

	app.ti.bold()
	app.ti.draw_text(ww / 6, 2, 'V term.input: cursor chaser demo')
	app.ti.draw_text((ww - ww / 6) - 14, 2, 'cut rate: ${(100 / app.cut_rate):3.0f}%')
	app.ti.horizontal_separator(3)
	app.ti.reset()
	app.ti.flush()
}

fn event(e &ti.Event, x voidptr) {
	mut app := &App(x)

	match e.typ {
		.key_down {
			match e.code {
				.escape {
					app.ti.set_cursor_position(0, 0)
					app.ti.flush()
					exit(0)
				}
				.space, .enter {
					app.color_idx++
					if app.color_idx == colors.len { app.color_idx = 0 }
					app.color = colors[app.color_idx]
				} else {}
			}
		} .mouse_move, .mouse_drag, .mouse_down {
			app.points << Point{ e.x, e.y }
		} .mouse_scroll {
			d := if e.direction == .up { 0.1 } else { -0.1 }
			app.cut_rate += d
			if app.cut_rate < 1 { app.cut_rate = 1 }
		} else {}
	}
}

mut app := &App{}
app.ti = ti.init(
	user_data: app,
	frame_fn: frame,
	event_fn: event,

	hide_cursor: true
)

app.ti.run()

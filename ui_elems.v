import term_input as input

import rand

struct Rect {
mut:
	c  input.Color
	x  int
	y  int
	x2 int
	y2 int
}

struct App {
mut:
	ti       &input.Context = 0
	rects    []Rect
	cur_rect Rect
	is_drag  bool
	redraw   bool
}

fn random_color() input.Color {
	return {
		r: byte(rand.intn(256))
		g: byte(rand.intn(256))
		b: byte(rand.intn(256))
	}
}

fn event(e &input.Event, x voidptr) {
	mut app := &App(x)
	match e.typ {
		.mouse_down {
			app.is_drag = true
			app.cur_rect = {
				c: random_color()
				x:  e.x
				y:  e.y
				x2: e.x
				y2: e.y
			}
		}
		.mouse_drag {
			app.cur_rect.x2 = e.x
			app.cur_rect.y2 = e.y
		} .mouse_up {
			app.rects << app.cur_rect
			app.is_drag = false
		} .key_down {
			if e.code == .c { app.rects.clear() }
			else if e.code == .escape { exit(0) }
		} else {}
	}
	app.redraw = true	
}

fn frame(x voidptr) {
	mut app := &App(x)
	if !app.redraw { return }

	app.ti.clear()

	for rect in app.rects {
		app.ti.set_bg_color(rect.c)
		app.ti.draw_rect(rect.x, rect.y, rect.x2, rect.y2)
	}

	if app.is_drag {
		r := app.cur_rect
		app.ti.set_bg_color(r.c)
		app.ti.draw_empty_rect(r.x, r.y, r.x2, r.y2)
	}

	app.ti.reset_bg_color()
	app.ti.flush()
	app.redraw = false
}


mut app := &App{}
app.ti = input.init(
	user_data: app,
	event_fn: event,
	frame_fn: frame

	hide_cursor: true
	frame_rate: 60
)

app.ti.run()

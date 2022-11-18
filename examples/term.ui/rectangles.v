import term.ui as tui
import rand

struct Rect {
mut:
	c  tui.Color
	x  int
	y  int
	x2 int
	y2 int
}

struct App {
mut:
	tui      &tui.Context = unsafe { nil }
	rects    []Rect
	cur_rect Rect
	is_drag  bool
	redraw   bool
}

fn random_color() tui.Color {
	return tui.Color{
		r: rand.u8()
		g: rand.u8()
		b: rand.u8()
	}
}

fn event(e &tui.Event, x voidptr) {
	mut app := &App(x)
	match e.typ {
		.mouse_down {
			app.is_drag = true
			app.cur_rect = Rect{
				c: random_color()
				x: e.x
				y: e.y
				x2: e.x
				y2: e.y
			}
		}
		.mouse_drag {
			app.cur_rect.x2 = e.x
			app.cur_rect.y2 = e.y
		}
		.mouse_up {
			app.rects << app.cur_rect
			app.is_drag = false
		}
		.key_down {
			if e.code == .c {
				app.rects.clear()
			} else if e.code == .escape {
				exit(0)
			}
		}
		else {}
	}
	app.redraw = true
}

fn frame(x voidptr) {
	mut app := &App(x)
	if !app.redraw {
		return
	}

	app.tui.clear()

	for rect in app.rects {
		app.tui.set_bg_color(rect.c)
		app.tui.draw_rect(rect.x, rect.y, rect.x2, rect.y2)
	}

	if app.is_drag {
		r := app.cur_rect
		app.tui.set_bg_color(r.c)
		app.tui.draw_empty_rect(r.x, r.y, r.x2, r.y2)
	}

	app.tui.reset_bg_color()
	app.tui.flush()
	app.redraw = false
}

fn main() {
	mut app := &App{}
	app.tui = tui.init(
		user_data: app
		event_fn: event
		frame_fn: frame
		hide_cursor: true
		frame_rate: 60
	)
	app.tui.run()?
}

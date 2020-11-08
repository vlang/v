import term
import term_input as input

struct Color {
	r byte
	g byte
	b byte
}

const (
	colors = [
		Color{244, 67, 54}
		Color{233, 30, 99}
		Color{156, 39, 176}
		Color{103, 58, 183}
		Color{63, 81, 181}
		Color{33, 150, 243}
		Color{3, 169, 244}
		Color{0, 188, 212}
		Color{0, 150, 136}
		Color{76, 175, 80}
		Color{139, 195, 74}
		Color{205, 220, 57}
		Color{255, 235, 59}
		Color{255, 193, 7}
		Color{255, 152, 0}
		Color{255, 87, 34}
		Color{121, 85, 72}
		Color{158, 158, 158}
		Color{96, 125, 139}
	]
)

struct Segment {
	x int
	y int
}

struct App {
mut:
	ti        &input.Context = 0
	segments  []Segment
	color     Color = colors[0]
	color_idx int
	// msg      string
}

fn init(x voidptr) {
	// println('INIT')
}

fn frame(x voidptr) {
	mut app := &App(x)
	// println('FRAME')
	term.erase_clear()
	term.erase_del_clear()

	if app.segments.len > 0 {
		c := app.color
		for s in app.segments {
			term.set_cursor_position(x: s.x, y: s.y)
			print(term.bg_rgb(c.r, c.g, c.b, 'V'))
		}

		l := app.segments.len / 8
		app.segments = app.segments[l..].clone()

		// app.segments = app.segments[1..]
	}
}

fn event(e &input.Event, x voidptr) {
	mut app := &App(x)
	// println('EVENT ${e.typ:-12} - $e')

	if e.typ == .key_down {
		match e.code {
			.escape {
				term.set_cursor_position(x: 0, y: 0)
				exit(0)
			}
			.space, .enter {
				app.color_idx++
				if app.color_idx == colors.len { app.color_idx = 0 }
				app.color = colors[app.color_idx]
			} else {}
		}
	} else if e.typ in [.mouse_move, .mouse_drag, .mouse_down] {
		c := Segment{ x: e.x, y: e.y }
		app.segments << c
	}
}

mut app := &App{}
app.ti = input.init(
	user_data: app,
	init_fn: init,
	frame_fn: frame,
	event_fn: event

	capture_events: true
	frame_rate: 30
)

app.ti.run()

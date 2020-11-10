module main

import math
import term
import term_input as input

struct Color {
	r byte
	g byte
	b byte
}

const (
	colors = [
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
		Color{244, 67, 54}
		Color{233, 30, 99}
		Color{156, 39, 176}
		Color{103, 58, 183}
	]

	origin = term.Coord{ x: 0, y: 0 }


	// Try changing this to see how it affects things
	frame_rate = 30
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

	last_pos  Segment = { x: -1, y: -1 }
	// msg      string

	cut_rate  f64 = frame_rate / 20.0
}

// Fills in any pixels between s1 and s2, using a slightly modified Bresenham's line algorithm:
// https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm#Algorithm
fn (s1 Segment) to(s2 Segment) []Segment {
	mut res := []Segment{}

	mut x0, x1 := s1.x, s2.x
	mut y0, y1 := s1.y, s2.y

	sx := if x0 < x1 { 1 } else { -1 }
	sy := if y0 < y1 { 1 } else { -1 }
	dx := math.abs(x1-x0)
	dy := -math.abs(y1-y0)

	mut err := dx + dy

	for {
		res << Segment{ x0, y0 }
		if x0 == x1 && y0 == y1 { break }
		e2 := 2 * err
		if e2 >= dy {
			err += dy
			x0 += sx
		}
		if e2 <= dx {
			err += dx
			y0 += sy
		}
	}
	return res
}

fn init(x voidptr) {
	// println('INIT')
}

fn frame(x voidptr) {
	mut app := &App(x)
	// println('FRAME')

	if app.segments.len > 0 {
		term.erase_clear()
		term.erase_del_clear()

		c := app.color
		for s in app.segments {
			term.set_cursor_position(x: s.x, y: s.y)
			print(term.bg_rgb(c.r, c.g, c.b, ' '))
		}

		l := int(app.segments.len / app.cut_rate)
		app.segments = app.segments[l..].clone()
	}

	term.set_cursor_position(origin)
	term.erase_line_toend()
	println('len: ${app.segments.len:-3} | cut rate: ${(100 / app.cut_rate):3.0f}% | cut len: ${int(app.segments.len / app.cut_rate)}')

}

fn event(e &input.Event, x voidptr) {
	mut app := &App(x)
	// println('EVENT ${e.typ:-12} - $e')

	match e.typ {
		.key_down {
			match e.code {
				.escape {
					term.set_cursor_position(origin)
					exit(0)
				}
				.space, .enter {
					app.color_idx++
					if app.color_idx == colors.len { app.color_idx = 0 }
					app.color = colors[app.color_idx]
				} else {}
			}
		} .mouse_move, .mouse_drag, .mouse_down {
			c := Segment{ x: e.x, y: e.y }

			if app.last_pos.x == -1 && app.last_pos.y == -1 { app.last_pos = c } // ignore initial position
			app.segments << c.to(app.last_pos)
			app.last_pos = c
		} .mouse_scroll {
			d := if e.direction == .up { 0.1 } else { -0.1 }
			app.cut_rate += d
			if app.cut_rate < 1 { app.cut_rate = 1 }
		} else {}
	}
}

mut app := &App{}
app.ti = input.init(
	user_data: app,
	init_fn: init,
	frame_fn: frame,
	event_fn: event

	capture_events: true
	frame_rate: frame_rate
)

term.hide_cursor()
app.ti.run()

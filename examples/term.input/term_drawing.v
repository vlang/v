module main

import term.input as ti

// The color palette, taken from Google's Material design
const (
	colors = [
		[
			ti.Color{239, 154, 154}
			ti.Color{244, 143, 177}
			ti.Color{206, 147, 216}
			ti.Color{179, 157, 219}
			ti.Color{159, 168, 218}
			ti.Color{144, 202, 249}
			ti.Color{129, 212, 250}
			ti.Color{128, 222, 234}
			ti.Color{128, 203, 196}
			ti.Color{165, 214, 167}
			ti.Color{197, 225, 165}
			ti.Color{230, 238, 156}
			ti.Color{255, 245, 157}
			ti.Color{255, 224, 130}
			ti.Color{255, 204, 128}
			ti.Color{255, 171, 145}
			ti.Color{188, 170, 164}
			ti.Color{238, 238, 238}
			ti.Color{176, 190, 197}
		], [
			ti.Color{244, 67, 54}
			ti.Color{233, 30, 99}
			ti.Color{156, 39, 176}
			ti.Color{103, 58, 183}
			ti.Color{63, 81, 181}
			ti.Color{33, 150, 243}
			ti.Color{3, 169, 244}
			ti.Color{0, 188, 212}
			ti.Color{0, 150, 136}
			ti.Color{76, 175, 80}
			ti.Color{139, 195, 74}
			ti.Color{205, 220, 57}
			ti.Color{255, 235, 59}
			ti.Color{255, 193, 7}
			ti.Color{255, 152, 0}
			ti.Color{255, 87, 34}
			ti.Color{121, 85, 72}
			ti.Color{158, 158, 158}
			ti.Color{96, 125, 139}
		], [
			ti.Color{198, 40, 40}
			ti.Color{173, 20, 87}
			ti.Color{106, 27, 154}
			ti.Color{69, 39, 160}
			ti.Color{40, 53, 147}
			ti.Color{21, 101, 192}
			ti.Color{2, 119, 189}
			ti.Color{0, 131, 143}
			ti.Color{0, 105, 92}
			ti.Color{46, 125, 50}
			ti.Color{85, 139, 47}
			ti.Color{158, 157, 36}
			ti.Color{249, 168, 37}
			ti.Color{255, 143, 0}
			ti.Color{239, 108, 0}
			ti.Color{216, 67, 21}
			ti.Color{78, 52, 46}
			ti.Color{66, 66, 66}
			ti.Color{55, 71, 79}
		]
	]
)

const (
	frame_rate = 30 // fps
	msg_display_time = 5 * frame_rate

	w = 200
	h = 100

	space  = ' '
	spaces = '  '

	select_color = 'Select color:'
	select_size  = 'Size: ＋  －'

	help_1 = '╭────────╮'
	help_2 = '│  HELP  │'
	help_3 = '╰────────╯'
)

struct App {
mut:
	ti              &ti.Context = 0
	header_text     []string
	mouse_pos       Point
	msg             string
	msg_hide_tick   int
	primary_color   ti.Color = colors[1][6]
	secondary_color ti.Color = colors[1][9]
	drawing         [][]ti.Color = [][]ti.Color{ len: h, init: []ti.Color{ len: w } }
	size            int = 1
	should_redraw   bool = true
	is_dragging     bool
}

struct Point {
	x int
	y int
}

fn main() {
	mut app := &App{}
	app.ti = ti.init(
		user_data: app
		frame_fn: frame
		event_fn: event

		frame_rate: frame_rate
		hide_cursor: true
	)

	app.ti.run()
}

fn frame(x voidptr) {
	mut app := &App(x)

	mut redraw := app.should_redraw

	if app.msg != '' && app.ti.frame_count >= app.msg_hide_tick {
		app.msg = ''
		redraw = true
	}

	if redraw {
		app.render(false)
		app.should_redraw = false
	}
}

fn event(event &ti.Event, x voidptr) {
	mut app := &App(x)

	match event.typ {
		.mouse_down {
			app.is_dragging = true
			if app.ti.window_height - event.y < 5 {
				app.footer_click(event)
			} else {
				app.paint(event)
			}
		} .mouse_up {
			app.is_dragging = false
		} .mouse_drag {
			app.mouse_pos = { x: event.x, y: event.y }
			app.paint(event)
		} .mouse_move {
			app.mouse_pos = { x: event.x, y: event.y }
		} .mouse_scroll {
			if event.direction == .down { app.inc_size() } else { app.dec_size() }
		} .key_down {
			match event.code {
				.c {
					app.drawing = [][]ti.Color{ len: h, init: []ti.Color{ len: w } }
				} .escape {
					app.render(true)
					exit(0)
				} else {}
			}
		} else {}
	}

	app.should_redraw = true
}

fn (mut app App) render(paint_only bool) {
	app.ti.clear()

	app.draw_header()
	app.draw_content()

	if !paint_only {
		app.draw_footer()
		app.draw_cursor()
	}

	app.ti.flush()
}

fn (mut app App) set_pixel(x_ int, y_ int, c ti.Color) {
	// Term coords start at 1, and adjust for the header
	x, y := x_ - 1, y_ - 4
	if y < 0 || app.ti.window_height - y < 3 { return }
	if y >= app.drawing.len || x < 0 || x >= app.drawing[0].len { return }
	app.drawing[y][x] = c
}

fn (mut app App) paint(event &ti.Event) {
	x_start, y_start := int(f32((event.x - 1) / 2) - app.size / 2 + 1), event.y - app.size / 2
	color := if event.button == .primary { app.primary_color } else { app.secondary_color }

	for x in x_start .. x_start + app.size {
		for y in y_start .. y_start + app.size {
			app.set_pixel(x, y, color)
		}
	}
}

fn (mut app App) draw_content() {
	w, mut h := app.ti.window_width / 2, app.ti.window_height - 8
	if h > app.drawing.len {
		h = app.drawing.len
	}

	for row_idx, row in app.drawing[..h] {
		app.ti.set_cursor_position(0, row_idx + 4)
		mut last := ti.Color{ 0, 0, 0 }
		for cell in row[..w] {
			if cell.r == 0 && cell.g == 0 && cell.b == 0 {
				if !(cell.r == last.r && cell.g == last.g && cell.b == last.b) {
					app.ti.reset()
				}
			} else {
				if !(cell.r == last.r && cell.g == last.g && cell.b == last.b) {
					app.ti.set_bg_color(cell)
				}
			}
			app.ti.write(spaces)
			last = cell
		}
		app.ti.reset()
	}
}

fn (mut app App) draw_cursor() {
	if app.mouse_pos.y in [3, app.ti.window_height - 5] {
		// inside the horizontal separators
		return
	}

	cursor_color := if app.is_dragging { ti.Color{ 220, 220, 220 } } else { ti.Color{ 160, 160, 160 } }
	app.ti.set_bg_color(cursor_color)
	
	if app.mouse_pos.y >= 3 && app.mouse_pos.y <= app.ti.window_height - 4 {
		// inside the main content
		mut x_start := int(f32((app.mouse_pos.x - 1) / 2) - app.size / 2 + 1) * 2 - 1
		mut y_start := app.mouse_pos.y - app.size / 2
		mut x_end := x_start + app.size * 2 - 1
		mut y_end := y_start + app.size - 1

		if x_start < 1 { x_start = 1 }
		if y_start < 4 { y_start = 4 }
		if x_end > app.ti.window_width { x_end = app.ti.window_width }
		if y_end > app.ti.window_height - 5 { y_end = app.ti.window_height - 5 }

		app.ti.draw_rect(x_start, y_start, x_end, y_end)
	} else {
		app.ti.draw_text(app.mouse_pos.x, app.mouse_pos.y, space)
	}
	app.ti.reset()
}

fn (mut app App) draw_header() {
	if app.msg != '' {
		app.ti.set_color(r: 0, g: 0, b: 0)
		app.ti.set_bg_color(r: 220, g: 220, b: 220)
		app.ti.draw_text(0, 0, ' $app.msg ')
		app.ti.reset()
	}

	app.ti.draw_text(3, 2, /* 'tick: $app.ti.frame_count | ' + */ 'terminal size: ($app.ti.window_width, $app.ti.window_height) | primary color: $app.primary_color.hex() | secondary color: $app.secondary_color.hex()')
	app.ti.horizontal_separator(3)
}

fn (mut app App) draw_footer() {
	ww, wh := app.ti.window_width, app.ti.window_height

	app.ti.horizontal_separator(wh - 4)

	for i, color_row in colors {
		for j, color in color_row {
			x := j * 3 + 19
			y := wh - 3 + i

			app.ti.set_bg_color(color)
			app.ti.draw_rect(x, y, x+1, y)
		}
	}
	app.ti.reset_bg_color()

	app.ti.draw_text(3, wh - 3, select_color)
	app.ti.bold()
	app.ti.draw_text(3, wh - 1, select_size)
	app.ti.reset()

	if ww >= 90 {
		app.ti.draw_text(80, wh - 3, help_1)
		app.ti.draw_text(80, wh - 2, help_2)
		app.ti.draw_text(80, wh - 1, help_3)
	}
}

[inline]
fn (mut app App) inc_size() {
	if app.size < 20 { app.size++ }
	app.show_msg('inc. size: $app.size', 1)
}

[inline]
fn (mut app App) dec_size() {
	if app.size > 1 { app.size-- }
	app.show_msg('dec. size: $app.size', 1)
}

fn (mut app App) footer_click(event &ti.Event) {
	footer_y := 3 - (app.ti.window_height - event.y)
	match event.x {
		8...11 {
			app.inc_size()
		} 12...15 {
			app.dec_size()
		} 18...75 {
			if (event.x % 3) == 0 { return } // Inside the gap between tiles
			idx := footer_y * 19 - 6 + event.x / 3
			color := colors[idx / 19][idx % 19]
			if event.button == .primary { app.primary_color = color } else { app.secondary_color = color }
			app.show_msg('set ${event.button.str().to_lower()} color idx: $idx', 1)
		} else {}
	}
}

fn (mut app App) show_msg(text string, time int) {
	frames := time * frame_rate
	app.msg_hide_tick = if time > 0 { int(app.ti.frame_count) + frames } else { -1 }
	app.msg = text
}

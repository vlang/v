// Copyright (c) 2020 Raúl Hernández. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import term.ui

// The color palette, taken from Google's Material design
const (
	colors = [
		[
			ui.Color{239, 154, 154},
			ui.Color{244, 143, 177},
			ui.Color{206, 147, 216},
			ui.Color{179, 157, 219},
			ui.Color{159, 168, 218},
			ui.Color{144, 202, 249},
			ui.Color{129, 212, 250},
			ui.Color{128, 222, 234},
			ui.Color{128, 203, 196},
			ui.Color{165, 214, 167},
			ui.Color{197, 225, 165},
			ui.Color{230, 238, 156},
			ui.Color{255, 245, 157},
			ui.Color{255, 224, 130},
			ui.Color{255, 204, 128},
			ui.Color{255, 171, 145},
			ui.Color{188, 170, 164},
			ui.Color{238, 238, 238},
			ui.Color{176, 190, 197},
		],
		[
			ui.Color{244, 67, 54},
			ui.Color{233, 30, 99},
			ui.Color{156, 39, 176},
			ui.Color{103, 58, 183},
			ui.Color{63, 81, 181},
			ui.Color{33, 150, 243},
			ui.Color{3, 169, 244},
			ui.Color{0, 188, 212},
			ui.Color{0, 150, 136},
			ui.Color{76, 175, 80},
			ui.Color{139, 195, 74},
			ui.Color{205, 220, 57},
			ui.Color{255, 235, 59},
			ui.Color{255, 193, 7},
			ui.Color{255, 152, 0},
			ui.Color{255, 87, 34},
			ui.Color{121, 85, 72},
			ui.Color{120, 120, 120},
			ui.Color{96, 125, 139},
		],
		[
			ui.Color{198, 40, 40},
			ui.Color{173, 20, 87},
			ui.Color{106, 27, 154},
			ui.Color{69, 39, 160},
			ui.Color{40, 53, 147},
			ui.Color{21, 101, 192},
			ui.Color{2, 119, 189},
			ui.Color{0, 131, 143},
			ui.Color{0, 105, 92},
			ui.Color{46, 125, 50},
			ui.Color{85, 139, 47},
			ui.Color{158, 157, 36},
			ui.Color{249, 168, 37},
			ui.Color{255, 143, 0},
			ui.Color{239, 108, 0},
			ui.Color{216, 67, 21},
			ui.Color{78, 52, 46},
			ui.Color{33, 33, 33},
			ui.Color{55, 71, 79},
		],
	]
)

const (
	frame_rate       = 30 // fps
	msg_display_time = 5 * frame_rate
	w                = 200
	h                = 100
	space            = ' '
	spaces           = '  '
	select_color     = 'Select color: '
	select_size      = 'Size: ＋  －'
	help_1           = '╭────────╮'
	help_2           = '│  HELP  │'
	help_3           = '╰────────╯'
)

struct App {
mut:
	ui                  &ui.Context = unsafe { nil }
	header_text         []string
	mouse_pos           Point
	msg                 string
	msg_hide_tick       int
	primary_color       ui.Color     = colors[1][6]
	secondary_color     ui.Color     = colors[1][9]
	primary_color_idx   int          = 25
	secondary_color_idx int          = 28
	bg_color            ui.Color     = ui.Color{0, 0, 0}
	drawing             [][]ui.Color = [][]ui.Color{len: h, init: []ui.Color{len: w}}
	size                int  = 1
	should_redraw       bool = true
	is_dragging         bool
}

struct Point {
mut:
	x int
	y int
}

fn main() {
	mut app := &App{}
	app.ui = ui.init(
		user_data: app
		frame_fn: frame
		event_fn: event
		frame_rate: frame_rate
		hide_cursor: true
		window_title: 'V terminal pixelart drawing app'
	)
	app.mouse_pos.x = 40
	app.mouse_pos.y = 15
	app.ui.clear()
	app.ui.run()?
}

fn frame(x voidptr) {
	mut app := &App(x)
	mut redraw := app.should_redraw
	if app.msg != '' && app.ui.frame_count >= app.msg_hide_tick {
		app.msg = ''
		redraw = true
	}
	if redraw {
		app.render(false)
		app.should_redraw = false
	}
}

fn event(event &ui.Event, x voidptr) {
	mut app := &App(x)
	match event.typ {
		.mouse_down {
			app.is_dragging = true
			if app.ui.window_height - event.y < 5 {
				app.footer_click(event)
			} else {
				app.paint(event)
			}
		}
		.mouse_up {
			app.is_dragging = false
		}
		.mouse_drag {
			app.mouse_pos = Point{
				x: event.x
				y: event.y
			}
			app.paint(event)
		}
		.mouse_move {
			app.mouse_pos = Point{
				x: event.x
				y: event.y
			}
		}
		.mouse_scroll {
			app.mouse_pos = Point{
				x: event.x
				y: event.y
			}
			d := event.direction == .down
			if event.modifiers.has(.ctrl) {
				p := !event.modifiers.has(.shift)
				c := if d {
					if p { app.primary_color_idx - 1 } else { app.secondary_color_idx - 1 }
				} else {
					if p { app.primary_color_idx + 1 } else { app.secondary_color_idx + 1 }
				}
				app.select_color(p, c)
			} else {
				if d {
					app.inc_size()
				} else {
					app.dec_size()
				}
			}
		}
		.key_down {
			match event.code {
				.f1, ._1 {
					oevent := *event
					nevent := ui.Event{
						...oevent
						button: ui.MouseButton.left
						x: app.mouse_pos.x
						y: app.mouse_pos.y
					}
					app.paint(nevent)
				}
				.f2, ._2 {
					oevent := *event
					nevent := ui.Event{
						...oevent
						button: ui.MouseButton.right
						x: app.mouse_pos.x
						y: app.mouse_pos.y
					}
					app.paint(nevent)
				}
				.space, .enter {
					oevent := *event
					nevent := ui.Event{
						...oevent
						button: .left
						x: app.mouse_pos.x
						y: app.mouse_pos.y
					}
					app.paint(nevent)
				}
				.delete, .backspace {
					oevent := *event
					nevent := ui.Event{
						...oevent
						button: .middle
						x: app.mouse_pos.x
						y: app.mouse_pos.y
					}
					app.paint(nevent)
				}
				.j, .down {
					if event.modifiers.has(.shift) {
						app.set_pixel((1 + app.mouse_pos.x) / 2, app.mouse_pos.y, app.primary_color)
					}
					app.mouse_pos.y++
				}
				.k, .up {
					if event.modifiers.has(.shift) {
						app.set_pixel((1 + app.mouse_pos.x) / 2, app.mouse_pos.y, app.primary_color)
					}
					app.mouse_pos.y--
				}
				.h, .left {
					if event.modifiers.has(.shift) {
						app.set_pixel((1 + app.mouse_pos.x) / 2, app.mouse_pos.y, app.primary_color)
					}
					app.mouse_pos.x -= 2
				}
				.l, .right {
					if event.modifiers.has(.shift) {
						app.set_pixel((1 + app.mouse_pos.x) / 2, app.mouse_pos.y, app.primary_color)
					}
					app.mouse_pos.x += 2
				}
				.t {
					p := !event.modifiers.has(.alt)
					c := if event.modifiers.has(.shift) {
						if p { app.primary_color_idx - 19 } else { app.secondary_color_idx - 19 }
					} else {
						if p { app.primary_color_idx + 19 } else { app.secondary_color_idx + 19 }
					}
					app.select_color(p, c)
				}
				.r {
					p := !event.modifiers.has(.alt)
					c := if event.modifiers.has(.shift) {
						if p { app.primary_color_idx - 1 } else { app.secondary_color_idx - 1 }
					} else {
						if p { app.primary_color_idx + 1 } else { app.secondary_color_idx + 1 }
					}
					app.select_color(p, c)
				}
				.plus {
					app.inc_size()
				}
				.minus {
					app.dec_size()
				}
				.c {
					app.drawing = [][]ui.Color{len: h, init: []ui.Color{len: w}}
				}
				.q, .escape {
					app.render(true)
					exit(0)
				}
				else {}
			}
		}
		else {}
	}
	app.should_redraw = true
}

fn (mut app App) render(paint_only bool) {
	app.ui.clear()
	app.draw_header()
	app.draw_content()
	if !paint_only {
		app.draw_footer()
		app.draw_cursor()
	}
	app.ui.flush()
}

fn (mut app App) select_color(primary bool, idx int) {
	c := (idx + 57) % 57
	cx := c % 19
	cy := (c / 19) % 3
	color := colors[cy][cx]
	if primary {
		app.primary_color_idx = c % (19 * 3)
		app.primary_color = color
	} else {
		app.secondary_color_idx = c % (19 * 3)
		app.secondary_color = color
	}
	c_str := if primary { 'primary' } else { 'secondary' }
	app.show_msg('set $c_str color idx: $idx', 1)
}

fn (mut app App) set_pixel(x_ int, y_ int, c ui.Color) {
	// Term coords start at 1, and adjust for the header
	x, y := x_ - 1, y_ - 4
	if y < 0 || app.ui.window_height - y < 3 {
		return
	}
	if y >= app.drawing.len || x < 0 || x >= app.drawing[0].len {
		return
	}
	app.drawing[y][x] = c
}

fn (mut app App) paint(event &ui.Event) {
	if event.y < 4 || app.ui.window_height - event.y < 4 {
		return
	}
	x_start, y_start := int(f32((event.x - 1) / 2) - app.size / 2 + 1), event.y - app.size / 2
	color := match event.button {
		.left { app.primary_color }
		.right { app.secondary_color }
		else { app.bg_color }
	}
	for x in x_start .. x_start + app.size {
		for y in y_start .. y_start + app.size {
			app.set_pixel(x, y, color)
		}
	}
}

fn (mut app App) draw_content() {
	w_, mut h_ := app.ui.window_width / 2, app.ui.window_height - 8
	if h_ > app.drawing.len {
		h_ = app.drawing.len
	}
	for row_idx, row in app.drawing[..h_] {
		app.ui.set_cursor_position(0, row_idx + 4)
		mut last := ui.Color{0, 0, 0}
		for cell in row[..w_] {
			if cell.r == 0 && cell.g == 0 && cell.b == 0 {
				if !(cell.r == last.r && cell.g == last.g && cell.b == last.b) {
					app.ui.reset()
				}
			} else {
				if !(cell.r == last.r && cell.g == last.g && cell.b == last.b) {
					app.ui.set_bg_color(cell)
				}
			}
			app.ui.write(spaces)
			last = cell
		}
		app.ui.reset()
	}
}

fn (mut app App) draw_cursor() {
	if app.mouse_pos.y in [3, app.ui.window_height - 5] {
		// inside the horizontal separators
		return
	}
	cursor_color := if app.is_dragging { ui.Color{220, 220, 220} } else { ui.Color{160, 160, 160} }
	app.ui.set_bg_color(cursor_color)
	if app.mouse_pos.y >= 3 && app.mouse_pos.y <= app.ui.window_height - 4 {
		// inside the main content
		mut x_start := int(f32((app.mouse_pos.x - 1) / 2) - app.size / 2 + 1) * 2 - 1
		mut y_start := app.mouse_pos.y - app.size / 2
		mut x_end := x_start + app.size * 2 - 1
		mut y_end := y_start + app.size - 1
		if x_start < 1 {
			x_start = 1
		}
		if y_start < 4 {
			y_start = 4
		}
		if x_end > app.ui.window_width {
			x_end = app.ui.window_width
		}
		if y_end > app.ui.window_height - 5 {
			y_end = app.ui.window_height - 5
		}
		app.ui.draw_rect(x_start, y_start, x_end, y_end)
	} else {
		app.ui.draw_text(app.mouse_pos.x, app.mouse_pos.y, space)
	}
	app.ui.reset()
}

fn (mut app App) draw_header() {
	if app.msg != '' {
		app.ui.set_color(
			r: 0
			g: 0
			b: 0
		)
		app.ui.set_bg_color(
			r: 220
			g: 220
			b: 220
		)
		app.ui.draw_text(0, 0, ' $app.msg ')
		app.ui.reset()
	}
	//'tick: $app.ui.frame_count | ' +
	app.ui.draw_text(3, 2, 'terminal size: ($app.ui.window_width, $app.ui.window_height) | primary color: $app.primary_color.hex() | secondary color: $app.secondary_color.hex()')
	app.ui.horizontal_separator(3)
}

fn (mut app App) draw_footer() {
	_, wh := app.ui.window_width, app.ui.window_height
	app.ui.horizontal_separator(wh - 4)
	for i, color_row in colors {
		for j, color in color_row {
			x := j * 3 + 19
			y := wh - 3 + i
			app.ui.set_bg_color(color)
			if app.primary_color_idx == j + (i * 19) {
				app.ui.set_color(r: 0, g: 0, b: 0)
				app.ui.draw_text(x, y, '><')
				app.ui.reset_color()
			} else if app.secondary_color_idx == j + (i * 19) {
				app.ui.set_color(r: 255, g: 255, b: 255)
				app.ui.draw_text(x, y, '><')
				app.ui.reset_color()
			} else {
				app.ui.draw_rect(x, y, x + 1, y)
			}
		}
	}
	app.ui.reset_bg_color()
	app.ui.draw_text(3, wh - 3, select_color)
	app.ui.bold()
	app.ui.draw_text(3, wh - 1, '$select_size $app.size')
	app.ui.reset()

	// TODO: help button
	// if ww >= 90 {
	// 	app.ui.draw_text(80, wh - 3, help_1)
	// 	app.ui.draw_text(80, wh - 2, help_2)
	// 	app.ui.draw_text(80, wh - 1, help_3)
	// }
}

[inline]
fn (mut app App) inc_size() {
	if app.size < 30 {
		app.size++
	}
	app.show_msg('inc. size: $app.size', 1)
}

[inline]
fn (mut app App) dec_size() {
	if app.size > 1 {
		app.size--
	}
	app.show_msg('dec. size: $app.size', 1)
}

fn (mut app App) footer_click(event &ui.Event) {
	footer_y := 3 - (app.ui.window_height - event.y)
	match event.x {
		8...11 {
			app.inc_size()
		}
		12...15 {
			app.dec_size()
		}
		18...75 {
			if (event.x % 3) == 0 {
				// Inside the gap between tiles
				return
			}
			idx := footer_y * 19 - 6 + event.x / 3
			if idx < 0 || idx > 56 {
				return
			}
			app.select_color(event.button == .left, idx)
		}
		else {}
	}
}

fn (mut app App) show_msg(text string, time int) {
	frames := time * frame_rate
	app.msg_hide_tick = if time > 0 { int(app.ui.frame_count) + frames } else { -1 }
	app.msg = text
}

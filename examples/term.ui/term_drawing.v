// Copyright (c) 2020 Raúl Hernández. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import term.ui as tui

// The color palette, taken from Google's Material design
const (
	colors = [
		[
			tui.Color{239, 154, 154},
			tui.Color{244, 143, 177},
			tui.Color{206, 147, 216},
			tui.Color{179, 157, 219},
			tui.Color{159, 168, 218},
			tui.Color{144, 202, 249},
			tui.Color{129, 212, 250},
			tui.Color{128, 222, 234},
			tui.Color{128, 203, 196},
			tui.Color{165, 214, 167},
			tui.Color{197, 225, 165},
			tui.Color{230, 238, 156},
			tui.Color{255, 245, 157},
			tui.Color{255, 224, 130},
			tui.Color{255, 204, 128},
			tui.Color{255, 171, 145},
			tui.Color{188, 170, 164},
			tui.Color{238, 238, 238},
			tui.Color{176, 190, 197},
		],
		[
			tui.Color{244, 67, 54},
			tui.Color{233, 30, 99},
			tui.Color{156, 39, 176},
			tui.Color{103, 58, 183},
			tui.Color{63, 81, 181},
			tui.Color{33, 150, 243},
			tui.Color{3, 169, 244},
			tui.Color{0, 188, 212},
			tui.Color{0, 150, 136},
			tui.Color{76, 175, 80},
			tui.Color{139, 195, 74},
			tui.Color{205, 220, 57},
			tui.Color{255, 235, 59},
			tui.Color{255, 193, 7},
			tui.Color{255, 152, 0},
			tui.Color{255, 87, 34},
			tui.Color{121, 85, 72},
			tui.Color{120, 120, 120},
			tui.Color{96, 125, 139},
		],
		[
			tui.Color{198, 40, 40},
			tui.Color{173, 20, 87},
			tui.Color{106, 27, 154},
			tui.Color{69, 39, 160},
			tui.Color{40, 53, 147},
			tui.Color{21, 101, 192},
			tui.Color{2, 119, 189},
			tui.Color{0, 131, 143},
			tui.Color{0, 105, 92},
			tui.Color{46, 125, 50},
			tui.Color{85, 139, 47},
			tui.Color{158, 157, 36},
			tui.Color{249, 168, 37},
			tui.Color{255, 143, 0},
			tui.Color{239, 108, 0},
			tui.Color{216, 67, 21},
			tui.Color{78, 52, 46},
			tui.Color{33, 33, 33},
			tui.Color{55, 71, 79},
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
	tui                 &tui.Context = 0
	header_text         []string
	mouse_pos           Point
	msg                 string
	msg_hide_tick       int
	primary_color       tui.Color = colors[1][6]
	secondary_color     tui.Color = colors[1][9]
	primary_color_idx   int = 25
	secondary_color_idx int = 28
	bg_color            tui.Color = tui.Color{0, 0, 0}
	drawing             [][]tui.Color = [][]tui.Color{len: h, init: []tui.Color{len: w}}
	size                int = 1
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
	app.tui = tui.init({
		user_data: app
		frame_fn: frame
		event_fn: event
		frame_rate: frame_rate
		hide_cursor: true
		window_title: 'V terminal pixelart drawing app'
	})
	app.mouse_pos.x = 40
	app.mouse_pos.y = 15
	app.tui.clear()
	app.tui.run()
}

fn frame(x voidptr) {
	mut app := &App(x)
	mut redraw := app.should_redraw
	if app.msg != '' && app.tui.frame_count >= app.msg_hide_tick {
		app.msg = ''
		redraw = true
	}
	if redraw {
		app.render(false)
		app.should_redraw = false
	}
}

fn event(event &tui.Event, x voidptr) {
	mut app := &App(x)
	match event.typ {
		.mouse_down {
			app.is_dragging = true
			if app.tui.window_height - event.y < 5 {
				app.footer_click(event)
			} else {
				app.paint(event)
			}
		}
		.mouse_up {
			app.is_dragging = false
		}
		.mouse_drag {
			app.mouse_pos = {
				x: event.x
				y: event.y
			}
			app.paint(event)
		}
		.mouse_move {
			app.mouse_pos = {
				x: event.x
				y: event.y
			}
		}
		.mouse_scroll {
			app.mouse_pos = {
				x: event.x
				y: event.y
			}
			d := event.direction == .down
			if event.modifiers & tui.ctrl != 0 {
				p := event.modifiers & tui.shift == 0
				c := if d {
					if p { app.primary_color_idx - 1 } else { app.secondary_color_idx - 1 }
				} else {
					if p { app.primary_color_idx + 1 } else { app.secondary_color_idx + 1 }
				}
				app.select_color(p, c)
			} else {
				if d { app.inc_size() } else { app.dec_size() }
			}
		}
		.key_down {
			match event.code {
				.f1, ._1 {
					oevent := *event
					nevent := { oevent | button: tui.MouseButton.left, x: app.mouse_pos.x , y: app.mouse_pos.y }
					app.paint(nevent)
				}
				.f2, ._2 {
					oevent := *event
					nevent := { oevent | button: tui.MouseButton.right, x: app.mouse_pos.x , y: app.mouse_pos.y }
					app.paint(nevent)
				}
				.space {
					oevent := *event
					nevent := { oevent | button: tui.MouseButton.middle, x: app.mouse_pos.x , y: app.mouse_pos.y }
					app.paint(nevent)
				}
				.j, .down {
					if event.modifiers & tui.shift != 0 {
						app.set_pixel((1 + app.mouse_pos.x) / 2, app.mouse_pos.y, app.primary_color)
					}
					app.mouse_pos.y++
				}
				.k, .up {
					if event.modifiers & tui.shift != 0 {
						app.set_pixel((1 + app.mouse_pos.x) / 2, app.mouse_pos.y, app.primary_color)
					}
					app.mouse_pos.y--
				}
				.h, .left {
					if event.modifiers & tui.shift != 0 {
						app.set_pixel((1 + app.mouse_pos.x) / 2, app.mouse_pos.y, app.primary_color)
					}
					app.mouse_pos.x -= 2
				}
				.l, .right {
					if event.modifiers & tui.shift != 0 {
						app.set_pixel((1 + app.mouse_pos.x) / 2, app.mouse_pos.y, app.primary_color)
					}
					app.mouse_pos.x += 2
				}
				.t {
					p := event.modifiers & tui.alt == 0
					c := if event.modifiers & tui.shift != 0 {
						if p { app.primary_color_idx - 19 } else { app.secondary_color_idx - 19 }
					} else {
						if p { app.primary_color_idx + 19 } else { app.secondary_color_idx + 19 }
					}
					app.select_color(p, c)
				}
				.r {
					p := event.modifiers & tui.alt == 0
					c := if event.modifiers & tui.shift != 0 {
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
					app.drawing = [][]tui.Color{len: h, init: []tui.Color{len: w}}
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
	app.tui.clear()
	app.draw_header()
	app.draw_content()
	if !paint_only {
		app.draw_footer()
		app.draw_cursor()
	}
	app.tui.flush()
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

fn (mut app App) set_pixel(x_ int, y_ int, c tui.Color) {
	// Term coords start at 1, and adjust for the header
	x, y := x_ - 1, y_ - 4
	if y < 0 || app.tui.window_height - y < 3 {
		return
	}
	if y >= app.drawing.len || x < 0 || x >= app.drawing[0].len {
		return
	}
	app.drawing[y][x] = c
}

fn (mut app App) paint(event &tui.Event) {
	if event.y < 4 || app.tui.window_height - event.y < 4 {
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
	w_, mut h_ := app.tui.window_width / 2, app.tui.window_height - 8
	if h_ > app.drawing.len {
		h_ = app.drawing.len
	}
	for row_idx, row in app.drawing[..h_] {
		app.tui.set_cursor_position(0, row_idx + 4)
		mut last := tui.Color{0, 0, 0}
		for cell in row[..w_] {
			if cell.r == 0 && cell.g == 0 && cell.b == 0 {
				if !(cell.r == last.r && cell.g == last.g && cell.b == last.b) {
					app.tui.reset()
				}
			} else {
				if !(cell.r == last.r && cell.g == last.g && cell.b == last.b) {
					app.tui.set_bg_color(cell)
				}
			}
			app.tui.write(spaces)
			last = cell
		}
		app.tui.reset()
	}
}

fn (mut app App) draw_cursor() {
	if app.mouse_pos.y in [3, app.tui.window_height - 5] {
		// inside the horizontal separators
		return
	}
	cursor_color := if app.is_dragging { tui.Color{220, 220, 220} } else { tui.Color{160, 160, 160} }
	app.tui.set_bg_color(cursor_color)
	if app.mouse_pos.y >= 3 && app.mouse_pos.y <= app.tui.window_height - 4 {
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
		if x_end > app.tui.window_width {
			x_end = app.tui.window_width
		}
		if y_end > app.tui.window_height - 5 {
			y_end = app.tui.window_height - 5
		}
		app.tui.draw_rect(x_start, y_start, x_end, y_end)
	} else {
		app.tui.draw_text(app.mouse_pos.x, app.mouse_pos.y, space)
	}
	app.tui.reset()
}

fn (mut app App) draw_header() {
	if app.msg != '' {
		app.tui.set_color({
			r: 0
			g: 0
			b: 0
		})
		app.tui.set_bg_color({
			r: 220
			g: 220
			b: 220
		})
		app.tui.draw_text(0, 0, ' $app.msg ')
		app.tui.reset()
	}
	app.tui.draw_text(3, 2, /* 'tick: $app.tui.frame_count | ' + */ 'terminal size: ($app.tui.window_width, $app.tui.window_height) | primary color: $app.primary_color.hex() | secondary color: $app.secondary_color.hex()')
	app.tui.horizontal_separator(3)
}

fn (mut app App) draw_footer() {
	_, wh := app.tui.window_width, app.tui.window_height
	app.tui.horizontal_separator(wh - 4)
	for i, color_row in colors {
		for j, color in color_row {
			x := j * 3 + 19
			y := wh - 3 + i
			app.tui.set_bg_color(color)
			if app.primary_color_idx == j + (i * 19) {
				app.tui.set_color(r: 0, g: 0, b: 0)
				app.tui.draw_text(x, y, '><')
				app.tui.reset_color()
			} else if app.secondary_color_idx == j + (i * 19) {
				app.tui.set_color(r: 255, g: 255, b: 255)
				app.tui.draw_text(x, y, '><')
				app.tui.reset_color()
			} else {
				app.tui.draw_rect(x, y, x + 1, y)
			}
		}
	}
	app.tui.reset_bg_color()
	app.tui.draw_text(3, wh - 3, select_color)
	app.tui.bold()
	app.tui.draw_text(3, wh - 1, '$select_size $app.size')
	app.tui.reset()

	// TODO: help button
	// if ww >= 90 {
	// 	app.tui.draw_text(80, wh - 3, help_1)
	// 	app.tui.draw_text(80, wh - 2, help_2)
	// 	app.tui.draw_text(80, wh - 1, help_3)
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

fn (mut app App) footer_click(event &tui.Event) {
	footer_y := 3 - (app.tui.window_height - event.y)
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
			if idx < 0 || idx > 56 { return }
			app.select_color(event.button == .left, idx)
		}
		else {}
	}
}

fn (mut app App) show_msg(text string, time int) {
	frames := time * frame_rate
	app.msg_hide_tick = if time > 0 { int(app.tui.frame_count) + frames } else { -1 }
	app.msg = text
}

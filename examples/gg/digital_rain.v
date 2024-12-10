// Creates the digital rain effect from the movie, "The Matrix"
module main

import gg
import gx
import rand
import time

const font_size = 20
const rain_drops = '0123456789!@#$%^&*()-=+[]{}|;:<>?~bdjpqtvz'.bytes()

struct App {
mut:
	ctx          &gg.Context = unsafe { nil }
	rows         int
	cols         int
	char_width   int
	char_height  int
	screen_size  gg.Size
	should_calc  bool = true
	rain_columns []RainColumn
	delay        time.Duration = time.millisecond * 100
}

struct RainColumn {
mut:
	col   int  // character based postion
	len   int  // length of the rain column in characters
	head  int  // y position of the rain column
	drops []u8 // no retained graphics in gg, store entire column
}

fn main() {
	mut app := &App{}
	rain(mut app)
}

fn rain(mut app App) {
	app.ctx = gg.new_context(
		bg_color:     gx.rgb(0, 0, 0)
		width:        app.screen_size.width
		height:       app.screen_size.height
		user_data:    app
		window_title: 'Digital Rain'
		init_fn:      fn (mut app App) {
			gg.toggle_fullscreen()
		}
		event_fn:     fn (event &gg.Event, mut app App) {
			vprintln('event.typ: ${event.typ} | event.char_code: ${event.char_code}')
			if event.typ == .resized {
				app.should_calc = true
				return
			}
			if event.typ == .char && event.char_code == `f` {
				gg.toggle_fullscreen()
				return
			}
			if event.typ == .key_up && event.key_code == .up {
				app.delay = app.delay + 50 * time.millisecond
			}
			if event.typ == .key_up && event.key_code == .down {
				new_delay := app.delay - 50 * time.millisecond
				app.delay = if new_delay > 0 { new_delay } else { 0 }
			}
		}
		frame_fn:     frame
	)
	app.ctx.run()
}

fn frame(mut app App) {
	app.ctx.begin()
	if app.should_calc {
		app.should_calc = false
		calc_sizes(mut app)
	}
	// gradually add rain columns
	if app.rain_columns.len < app.cols / 4 * 3 {
		app.rain_columns << random_rain_column(app.cols, app.rows)
	}
	// update and print all rain columns
	for mut rc in app.rain_columns {
		update_rain_column(mut rc, app.cols, app.rows)
		draw_rain_column(rc, app)
	}
	app.ctx.draw_text(app.screen_size.width / 2 - 190, app.screen_size.height - 15, 'press `f` to toggle fullscreen, Up/Down arrows to change speed',
		color: gx.gray
	)
	app.ctx.end()
	vprintln('frame: ${app.ctx.frame} | app.cols: ${app.cols} | app.rows: ${app.rows} | app.rain_columns.len: ${app.rain_columns.len} | app.delay: ${app.delay}')
	time.sleep(app.delay)
}

@[if verbose ?]
fn vprintln(msg string) {
	println(msg)
}

fn calc_sizes(mut app App) {
	app.screen_size = gg.window_size()
	app.ctx.set_text_cfg(gx.TextCfg{
		size:  font_size
		color: gx.green
		mono:  true
	})
	// figure out how big character is in pixels
	// Pad it or it looks too squashed
	app.char_width, app.char_height = app.ctx.text_size('M')
	app.char_width += 3
	app.char_height += 1
	// determine the size of the matrix in rows and columns
	app.cols = app.screen_size.width / app.char_width
	app.rows = app.screen_size.height / app.char_height
	vprintln('app.cols: ${app.cols} | app.rows: ${app.rows}')
}

fn update_rain_column(mut rc RainColumn, width int, height int) {
	rc.head += 1
	if rc.head > height + rc.len {
		rc = random_rain_column(width, height)
	}
}

fn draw_rain_column(rc RainColumn, app App) {
	mut y := 0
	x := rc.col * app.char_width
	end := rc.head - rc.len
	for i in 0 .. app.rows - 1 {
		if i >= end && i < rc.head {
			alpha := match i - end {
				0 { u8(75) }
				1 { u8(100) }
				2 { u8(125) }
				3 { u8(150) }
				4 { u8(175) }
				5 { u8(200) }
				6 { u8(225) }
				else { u8(255) }
			}
			at_head := i == rc.head - 1
			cfg := gx.TextCfg{
				size:  font_size
				color: gg.Color{
					r: if at_head { u8(255) } else { 0 }
					g: 255
					b: if at_head { u8(255) } else { 0 }
					a: alpha
				}
				mono:  true
			}
			if i < rc.drops.len {
				app.ctx.draw_text(x, y, rc.drops[i].ascii_str(), cfg)
				app.ctx.draw_text(x, y, rc.drops[(i + 10) % rc.drops.len].ascii_str(),
					cfg)
			} else {
				vprintln('BAD i: ${i} | rc.drops.len: ${rc.drops.len}')
			}
		}
		y += app.char_height
	}
}

fn random_rain_column(max_col int, max_height int) RainColumn {
	min_len := 6
	mut rc := RainColumn{
		col:   rand.int_in_range(0, max_col) or { 0 }
		len:   rand.int_in_range(min_len, max_height / 4 * 3) or { min_len }
		drops: []u8{cap: max_height}
	}
	for _ in 0 .. max_height {
		rc.drops << random_rain_drop()
	}
	return rc
}

fn random_rain_drop() u8 {
	return rand.element(rain_drops) or { rain_drops[0] }
}

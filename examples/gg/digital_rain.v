module main

import gg
import gx
import rand
import time

const snooze = time.millisecond * 100
const rain_drops = '0123456789!@#$%^&*()-=+[]{}|;:<>?~bdjpqtvz'

struct App {
mut:
	ctx          &gg.Context = unsafe { nil }
	rows         int
	cols         int
	char_width   int
	char_height  int
	screen_size  gg.Size
	rain_columns []RainColumn
}

struct RainColumn {
mut:
	col   int  // character based postion
	len   int  // length of the rain column in characters
	head  int  // y position of the rain column
	drops []u8 // between 0 .. 1.0
}

fn main() {
	mut app := &App{}
	rain(mut app)
}

fn rain(mut app App) {
	// calc rows and columns to use as matrix
	app.screen_size = gg.screen_size()

	// Create the drawing context
	app.ctx = gg.new_context(
		bg_color: gx.rgb(0, 0, 0)
		width: app.screen_size.width
		height: app.screen_size.height
		user_data: app
		window_title: 'Digital Rain'
		frame_fn: frame
	)
	app.ctx.run()
}

fn frame(mut app App) {
	app.ctx.begin()
	if app.rows == 0 {
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
	app.ctx.end()

	time.sleep(snooze)
}

fn calc_sizes(mut app App) {
	app.ctx.set_text_cfg(gx.TextCfg{
		size: 20
		color: gx.green
		mono: true
	})

	app.char_width, app.char_height = app.ctx.text_size('M')
	app.char_width += 3
	app.char_height += 1

	app.cols = app.screen_size.width / app.char_width
	app.rows = app.screen_size.height / app.char_height
}

fn update_rain_column(mut rc RainColumn, width int, height int) {
	rc.head += 1
	if rc.head > height + rc.len {
		rc = random_rain_column(width, height)
	}
}

fn draw_rain_column(rc RainColumn, app App) {
	end := rc.head - rc.len
	x := rc.col * app.char_width
	mut y := 0
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
				size: 20
				color: gg.Color{
					r: if at_head { u8(255) } else { 0 }
					g: 255
					b: if at_head { u8(255) } else { 0 }
					a: alpha
				}
				mono: true
			}
			app.ctx.draw_text(x, y, rc.drops[i].ascii_str(), cfg)
		}
		y += app.char_height
	}
}

fn random_rain_column(max_col int, max_height int) RainColumn {
	min_len := 6
	mut rc := RainColumn{
		col: rand.int_in_range(0, max_col) or { 0 }
		len: rand.int_in_range(min_len, max_height / 4 * 3) or { min_len }
		drops: []u8{cap: max_height}
	}
	for _ in 0 .. max_height {
		rc.drops << random_rain_drop()
	}
	return rc
}

fn random_rain_drop() u8 {
	idx := rand.int_in_range(0, rain_drops.len) or { 0 }
	return rain_drops[idx]
}

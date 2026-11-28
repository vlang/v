module main

// raylib [core] example - text file loading
// Loads a text file, splits it into lines and lets the mouse wheel scroll
// through them. PORT NOTE: gg's TextCfg handles wrapping with max_width,
// so the manual word-wrapping loop from the C version is unnecessary.

import gg
import os

const screen_width = 800
const screen_height = 450
const font_size = 20
const text_top = 25 + font_size
const file_name = 'resources/text_file.txt'

const ray_white = gg.Color{245, 245, 245, 255}
const beige = gg.Color{245, 245, 220, 255}
const maroon = gg.Color{128, 0, 0, 255}
const red = gg.Color{255, 0, 0, 255}

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	lines []string
	cam   gg.Camera2D
	max_y f32
}

fn main() {
	mut app := &App{}
	content := os.read_file(file_name) or {
		'raylib text file loading example.\n\nThis fallback text is shown when resources/text_file.txt is missing.\nScroll with the mouse wheel to read the document.'
	}
	app.lines = content.split('\n')
	app.cam = gg.default_camera2d()
	app.max_y = f32(app.lines.len * (font_size + 10))
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - text file loading'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	app.cam.target.y -= f32(ctx.scroll_y) * font_size * 1.5
	if app.cam.target.y < 0 {
		app.cam.target.y = 0
	}
	limit := app.max_y - f32(screen_height) + text_top
	if app.cam.target.y > limit && limit > 0 {
		app.cam.target.y = limit
	}

	mut bar_h := f32(screen_height) * 100 / (app.max_y - f32(screen_height) + 1)
	if bar_h > f32(screen_height - text_top) {
		bar_h = f32(screen_height - text_top)
	}
	bar_y := text_top + (f32(screen_height - text_top) - bar_h) * (app.cam.target.y / (app.max_y + 1))

	ctx.begin()
	ctx.begin_2d(app.cam)
	mut y := text_top
	for line in app.lines {
		ctx.draw_text(10, y, line,
			max_width: screen_width - 20
			size:      font_size
			color:     red
		)
		y += font_size + 10
	}
	ctx.end_2d()

	ctx.draw_rect(x: 0, y: 0, w: screen_width, h: text_top - 10, color: beige)
	ctx.draw_text(10, 10, 'File: ${file_name}', size: font_size, color: maroon)
	ctx.draw_rect(x: screen_width - 5, y: bar_y, w: 5, h: bar_h, color: maroon)
	ctx.end()
}

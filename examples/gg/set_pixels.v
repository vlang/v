module main

import gg
import gx

struct App {
mut:
	gg &gg.Context
}

fn main() {
	mut app := &App{
		gg: 0
	}
	app.gg = gg.new_context(
		bg_color: gx.rgb(174, 198, 255)
		width: 100
		height: 100
		window_title: 'Set Pixels'
		frame_fn: frame
		user_data: app
	)
	app.gg.run()
}

fn frame(mut app App) {
	mut pixels := []f32{}

	for x in 30 .. 60 {
		for y in 30 .. 60 {
			pixels << f32(x)
			pixels << f32(y)
		}
	}

	app.gg.begin()
	app.gg.set_pixels(pixels, gx.red)
	app.gg.end()
}

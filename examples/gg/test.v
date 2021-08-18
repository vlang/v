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
		width: 600
		height: 400
		window_title: 'Polygons'
		frame_fn: frame
		user_data: app
	)
	app.gg.run()
}

fn frame(mut app App) {
	app.gg.scale = 3
	app.gg.begin()
	app.gg.set_pixels([f32(4), 4, 8, 8], gx.red)
	app.gg.end()
}

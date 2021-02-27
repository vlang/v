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
	app.gg.begin()
	app.gg.draw_convex_poly([f32(100.0), 100.0, 200.0, 100.0, 300.0, 200.0, 200.0, 300.0, 100.0,
		300.0,
	], gx.blue)
	app.gg.draw_empty_poly([f32(50.0), 50.0, 70.0, 60.0, 90.0, 80.0, 70.0, 110.0], gx.black)
	app.gg.draw_triangle(450, 142, 530, 280, 370, 280, gx.red)
	app.gg.end()
}

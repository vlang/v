module main

import gg
import gx
import os

const (
	win_width  = 600
	win_height = 300
)

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	image gg.Image
}

fn main() {
	mut app := &App{
		gg: 0
	}
	app.gg = gg.new_context(
		bg_color: gx.white
		width: win_width
		height: win_height
		create_window: true
		window_title: 'Rectangles'
		frame_fn: frame
		user_data: app
		init_fn: init_images
	)
	mut logo_path := os.resource_abs_path(os.join_path('..', 'assets', 'logo.png'))
	app.image = app.gg.create_image(logo_path)
	app.gg.run()
}

fn init_images(mut app App) {
	// app.image = gg.create_image('logo.png')
}

fn frame(app &App) {
	app.gg.begin()
	app.draw()
	app.gg.end()
}

fn (app &App) draw() {
	// app.gg.draw_text_def(200,20, 'hello world!')
	// app.gg.draw_text_def(300,300, 'привет')
	app.gg.draw_rect_filled(10, 10, 100, 30, gx.blue)
	app.gg.draw_rect_empty(110, 150, 80, 40, gx.black)
	app.gg.draw_image(230, 30, app.image.width, app.image.height, app.image)
}

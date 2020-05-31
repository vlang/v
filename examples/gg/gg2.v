module main

import gg2 as gg
import gx
import os

const (
	win_width = 600
	win_height = 300
)

struct App {
mut:
	gg &gg.GG
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		bg_color: gx.white
		width: win_width
		height: win_height
		use_ortho: true // This is needed for 2D drawing
		create_window: true
		window_title: 'Empty window'
		frame_fn: frame
		user_data: app
		//font_path: os.resource_abs_path('assets/fonts/RobotoMono-Regular.ttf')
	)
	app.gg.run()
}

fn frame(user_data voidptr) {
	mut app := &App(user_data)
	mut gg := app.gg
	gg.begin()
	/*
	if gg.fons == 0 {
		gg.init_font()
	}
	*/
	app.draw()
	//C.sfons_flush(gg.fons)
	gg.end()
}

fn (app &App) draw() {
	//app.gg.draw_text_def(200,20, 'hello world!')
	//app.gg.draw_text_def(300,300, 'привет')
	app.gg.draw_rect(10, 10, 100, 30, gx.blue)
	app.gg.draw_empty_rect(10, 150, 80, 40, gx.green)
}

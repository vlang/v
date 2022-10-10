module main

import os
import gg
import gx
import sokol.sapp

const (
	max_files = 12
	text      = 'Drag&Drop here max $max_files files.'
	text_size = 16
)

struct App {
mut:
	gg                &gg.Context = unsafe { nil }
	dropped_file_list []string    = []string{}
}

fn main() {
	mut font_path := os.resource_abs_path(os.join_path('..', 'assets', 'fonts', 'RobotoMono-Regular.ttf'))
	mut app := &App{
		gg: 0
	}
	app.gg = gg.new_context(
		bg_color: gx.rgb(174, 198, 255)
		width: 600
		height: 400
		window_title: 'Drag and drop'
		frame_fn: frame
		font_path: font_path
		user_data: app
		event_fn: my_event_manager
		// drag & drop
		enable_dragndrop: true
		max_dropped_files: max_files
		max_dropped_file_path_length: 2048
	)
	app.gg.run()
}

fn my_event_manager(mut ev gg.Event, mut app App) {
	// drag&drop event
	if ev.typ == .files_droped {
		num_dropped := sapp.get_num_dropped_files()
		app.dropped_file_list.clear()
		for i in 0 .. num_dropped {
			app.dropped_file_list << sapp.get_dropped_file_path(i)
		}
	}
}

fn frame(mut app App) {
	app.gg.begin()

	mut txt_conf := gx.TextCfg{
		color: gx.black
		align: .left
		size: int(text_size * app.gg.scale + 0.5)
	}
	app.gg.draw_text(12, 12, text, txt_conf)

	mut y := 40
	for c, f in app.dropped_file_list {
		app.gg.draw_text(12, y, '[$c] $f', txt_conf)
		y += text_size
	}

	app.gg.end()
}

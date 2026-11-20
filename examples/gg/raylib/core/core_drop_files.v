module main

// raylib [core] example - drop files
// Drag files onto the window; gg surfaces them through sokol.sapp's
// dropped-file API, enabled with enable_dragndrop in the context config.

import gg
import sokol.sapp

const screen_width = 800
const screen_height = 450
const max_paths = 100

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 128}

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	paths []string
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:                        screen_width
		height:                       screen_height
		window_title:                 'raylib [core] example - drop files'
		bg_color:                     ray_white
		frame_fn:                     frame
		event_fn:                     on_event
		enable_dragndrop:             true
		max_dropped_files:            max_paths
		max_dropped_file_path_length: 2048
		user_data:                    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .files_dropped {
		return
	}
	n := sapp.get_num_dropped_files()
	for i in 0 .. n {
		if app.paths.len >= max_paths {
			break
		}
		app.paths << sapp.get_dropped_file_path(i)
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	if app.paths.len == 0 {
		ctx.draw_text(100, 40, 'Drop your files to this window!',
			size:  20
			color: dark_gray
		)
	} else {
		ctx.draw_text(100, 40, 'Dropped files:', size: 20, color: dark_gray)
		for i, path in app.paths {
			y := 85 + 40 * i
			ctx.draw_rect(x: 0, y: f32(y), w: screen_width, h: 40, color: if i % 2 == 0 { light_gray } else { light_gray })
			ctx.draw_text(120, y + 15, path, size: 10, color: gray)
			if y > screen_height {
				break
			}
		}
		ctx.draw_text(100, 110 + 40 * app.paths.len, 'Drop new files...',
			size:  20
			color: dark_gray
		)
	}
	ctx.end()
}

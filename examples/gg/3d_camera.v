module main

import gg

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	dragging bool
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{}
	app.cam.orbit(30, 20, 8)
	app.gg = gg.new_context(
		width:        640
		height:       480
		window_title: 'gg 3D camera'
		bg_color:     gg.rgb(20, 24, 32)
		depth:        true
		event_fn:     on_event
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	match e.typ {
		.mouse_down {
			app.dragging = true
		}
		.mouse_up {
			app.dragging = false
		}
		.mouse_move {
			if app.dragging {
				app.cam.orbit_by(e.mouse_dx, e.mouse_dy, 0)
			}
		}
		.mouse_scroll {
			app.cam.orbit_by(0, 0, e.scroll_y)
		}
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.begin_3d(app.cam)
	ctx.draw_grid(20, 1.0, gg.gray)
	ctx.draw_cube(gg.Vec3{-1.5, 0, 0}, gg.Vec3{2, 2, 2}, gg.red)
	ctx.draw_cube(gg.Vec3{1.5, 1, 0}, gg.Vec3{1.5, 1.5, 1.5}, gg.blue)
	ctx.end_3d()
	ctx.end()
}

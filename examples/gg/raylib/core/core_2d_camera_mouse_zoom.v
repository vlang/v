module main

// raylib [core] example - 2d camera mouse zoom
// Zoom around the point under the mouse cursor. Mode 0 uses the wheel,
// mode 1 zooms while the right button is held and the mouse moves.
// Keys 1 and 2 select the mode; dragging with the left button pans.

import gg
import math

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{128, 0, 0, 255}
const light_gray = gg.Color{200, 200, 200, 255}

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	cam        gg.Camera2D
	zoom_mode  int
	right_down bool
	mouse_x    f32
	mouse_y    f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.default_camera2d()
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 2d camera mouse zoom'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	match e.typ {
		.key_down {
			if !e.key_repeat {
				if e.key_code == ._1 {
					app.zoom_mode = 0
				} else if e.key_code == ._2 {
					app.zoom_mode = 1
				}
			}
		}
		.mouse_down {
			if e.mouse_button == .right {
				app.right_down = true
				zoom_around_mouse(mut app, 0)
			}
		}
		.mouse_up {
			if e.mouse_button == .right {
				app.right_down = false
			}
		}
		else {}
	}
}

fn zoom_around_mouse(mut app &App, wheel f32) {
	ctx := app.gg
	mouse := gg.Vec2{f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y)}
	world := gg.screen_to_world(mouse, &app.cam, f32(ctx.width), f32(ctx.height))
	app.cam.offset = mouse
	app.cam.target = world
	scale := f64(wheel)
	app.cam.zoom = f32(math.exp(math.log(f64(app.cam.zoom)) + 0.2 * scale))
	if app.cam.zoom < 0.125 {
		app.cam.zoom = 0.125
	}
	if app.cam.zoom > 64 {
		app.cam.zoom = 64
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	app.mouse_x = f32(ctx.mouse_pos_x)
	app.mouse_y = f32(ctx.mouse_pos_y)

	// Pan with the left button, like raylib's GetMouseDelta()/camera.zoom.
	if ctx.mouse_buttons == .left {
		app.cam.target.x -= f32(ctx.mouse_dx) / app.cam.zoom
		app.cam.target.y -= f32(ctx.mouse_dy) / app.cam.zoom
	}

	if app.zoom_mode == 0 {
		if ctx.scroll_y != 0 {
			// Re-anchor using the last mouse position recorded by gg.
			mouse := gg.Vec2{app.mouse_x, app.mouse_y}
			world := gg.screen_to_world(mouse, &app.cam, f32(ctx.width), f32(ctx.height))
			app.cam.offset = mouse
			app.cam.target = world
			app.cam.zoom = f32(math.exp(math.log(f64(app.cam.zoom)) + 0.2 * f64(ctx.scroll_y)))
			if app.cam.zoom < 0.125 {
				app.cam.zoom = 0.125
			}
			if app.cam.zoom > 64 {
				app.cam.zoom = 64
			}
		}
	} else if app.right_down {
		app.cam.zoom = f32(math.exp(math.log(f64(app.cam.zoom)) + 0.005 * f64(ctx.mouse_dx)))
		if app.cam.zoom < 0.125 {
			app.cam.zoom = 0.125
		}
		if app.cam.zoom > 64 {
			app.cam.zoom = 64
		}
	}

	ctx.begin()
	ctx.begin_2d(app.cam)
	// PORT NOTE: raylib draws a 3D grid rotated into the XY plane here. gg's
	// Camera2D is 2D only, so we draw an equivalent flat grid with lines.
	for i in -20 .. 21 {
		p := f32(i) * 50
		ctx.draw_line(p, -1000, p, 1000, light_gray)
		ctx.draw_line(-1000, p, 1000, p, light_gray)
	}
	ctx.draw_circle_filled(400, 225, 50, maroon)
	ctx.end_2d()

	ctx.draw_circle_filled(app.mouse_x, app.mouse_y, 4, dark_gray)
	ctx.draw_text(int(app.mouse_x) - 44, int(app.mouse_y) - 24,
		'[${int(app.mouse_x)}, ${int(app.mouse_y)}]',
		size:  20
		color: gg.black
	)
	ctx.draw_text(20, 20, '[1][2] Select mouse zoom mode (Wheel or Move)',
		size:  20
		color: dark_gray
	)
	if app.zoom_mode == 0 {
		ctx.draw_text(20, 50, 'Mouse left button drag to move, mouse wheel to zoom',
			size:  20
			color: dark_gray
		)
	} else {
		ctx.draw_text(20, 50, 'Mouse left button drag to move, mouse press and move to zoom',
			size:  20
			color: dark_gray
		)
	}
	ctx.end()
}

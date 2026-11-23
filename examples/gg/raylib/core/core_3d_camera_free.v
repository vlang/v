module main

// raylib [core] example - 3d camera free
// Fly around with WASD/QE and the mouse; Z restores the origin target.

import gg
import sokol.sapp

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const sky_blue = gg.Color{102, 191, 255, 128}
const blue = gg.Color{0, 121, 241, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const black = gg.Color{0, 0, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	gg  &gg.Context = unsafe { nil }
	cam gg.Camera3D = gg.Camera3D{ position: gg.Vec3{10, 10, 10}, target: gg.Vec3{0, 0, 0} }
}

fn main() {
	mut app := &App{}
	app.cam.fovy = 45
	sapp.lock_mouse(true)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 3d camera free'
		bg_color:     ray_white
		depth:        true
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .z && !e.key_repeat {
		app.cam.target = gg.Vec3{0, 0, 0}
		app.cam.position = gg.Vec3{10, 10, 10}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.free, app.gg, dt)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.draw_cube(gg.Vec3{}, gg.Vec3{2, 2, 2}, gg.red)
	ctx.draw_cube_wires(gg.Vec3{}, gg.Vec3{2, 2, 2}, maroon)
	ctx.draw_grid(10, 1.0, gg.gray)
	ctx.end_3d()

	ctx.draw_rect(x: 10, y: 10, w: 320, h: 93, color: sky_blue)
	ctx.draw_rect_empty(10, 10, 320, 93, blue)
	ctx.draw_text(20, 20, 'Free camera default controls:', size: 10, color: black)
	ctx.draw_text(40, 40, '- Mouse Wheel to Zoom in-out', size: 10, color: dark_gray)
	ctx.draw_text(40, 60, '- Mouse Wheel Pressed to Pan', size: 10, color: dark_gray)
	ctx.draw_text(40, 80, '- Z to zoom to (0, 0, 0)', size: 10, color: dark_gray)
	ctx.end()
}

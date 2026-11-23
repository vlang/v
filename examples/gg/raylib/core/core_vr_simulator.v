module main

// raylib [core] example - vr simulator
// PORT NOTE: gg has no VR stereo config, lens-distortion shader or HMD
// support. This example keeps the scene and the side-by-side stereo camera
// idea: two offset Camera3Ds render into half of the window each.

import gg
import math

const screen_width = 800
const screen_height = 450
const half_width = screen_width / 2

const ray_white = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{128, 0, 0, 255}
const gray = gg.Color{130, 130, 130, 255}

struct App {
mut:
	gg  &gg.Context = unsafe { nil }
	cam gg.Camera3D = gg.Camera3D{ position: gg.Vec3{5, 2, 5}, target: gg.Vec3{0, 2, 0}, fovy: 60 }
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - vr simulator'
		bg_color:     ray_white
		depth:        true
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	for eye in 0 .. 2 {
		ctx.set_viewport(eye * half_width, 0, half_width, screen_height)
		mut cam := app.cam
		offset := gg.Vec3{if eye == 0 { -0.032 } else { 0.032 }, 0, 0}
		cam.position = cam.position + offset
		cam.target = cam.target + offset
		ctx.set_camera(cam)
		ctx.begin_3d() or { panic(err.msg()) }
		ctx.draw_cube(gg.Vec3{}, gg.Vec3{2, 2, 2}, gg.red)
		ctx.draw_cube_wires(gg.Vec3{}, gg.Vec3{2, 2, 2}, maroon)
		ctx.draw_grid(40, 1.0, gray)
		ctx.end_3d()
	}
	ctx.set_viewport(0, 0, ctx.width, ctx.height)
	ctx.draw_rect(x: half_width - 2, y: 0, w: 4, h: screen_height, color: gray)
	ctx.draw_text(10, screen_height - 20,
		'VR SIMULATOR (stereo viewports; no lens distortion in gg)',
		size:  10
		color: maroon
	)
	ctx.end()
}

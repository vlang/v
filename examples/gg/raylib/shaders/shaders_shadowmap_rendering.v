// Port of raylib [shaders] example - shadowmap rendering.
// PORT NOTE: shader shadowmap.vs/fs with a light-space depth pass requires gg user-shader
// support (not yet available); approximated with a dark projected ellipse beneath each object
// and Material3D directional lighting.
module main

import gg
import math

const win_width = 800
const win_height = 450

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	cam   gg.Camera3D
	cube  gg.Mesh3D
	robot gg.Mesh3D
	angle f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{10, 10, 10}, target: gg.Vec3{}, fovy: 45 }
	app.cam.orbit(30, 20, 17)
	app.cube = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.robot = gg.gen_mesh_sphere(0.8, 10, 16)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - shadowmap rendering'
		bg_color:      gg.white
		depth:         true
		user_data:     app
		update_fn:     update
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.gg, dt)
	app.angle += dt
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.draw_plane(gg.Vec3{}, 14, 14, gg.rgb(220, 220, 220))
	// Fake contact shadows as flattened dark discs.
	ctx.draw_sphere(gg.Vec3{-1.5, 0.02, 0}, 0.7, 6, 12, gg.Color{40, 40, 40, 90})
	ctx.draw_sphere(gg.Vec3{1.5, 0.02, 0}, 0.7, 6, 12, gg.Color{40, 40, 40, 90})
	mat := gg.Material3D{
		color:   gg.white
		ambient: 0.15
		light:   gg.Light3D{ direction: gg.Vec3{0.35, -1, -0.35}.normalized(), color: gg.white }
	}
	ctx.draw_material_mesh(&app.cube, gg.Vec3{-1.5, 0.6, 0}, gg.Vec3{}, 1, &mat, app.cam)
	rx := math.sinf(app.angle) * 0.5
	ctx.draw_material_mesh(&app.robot, gg.Vec3{1.5, 0.8, 0}, gg.Vec3{0, rx * 57.3, 0}, 1, &mat,
		app.cam)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Shadowmap pass not available; contact shadows approximated',
		size:  14
		color: gg.dark_gray
	)
	ctx.end()
}

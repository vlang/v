// Port of raylib [shaders] example - lights bloom.
// PORT NOTE: shader lights_bloom.vs/fs and the bloom post-process shader require gg user-shader
// support (not yet available); approximated with several bright Material3D light spheres,
// RenderTexture2D for the offscreen scene, and additive Image glow.
module main

import gg
import math

const win_width = 800
const win_height = 450
const max_lights = 8

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	cam       gg.Camera3D
	cube      gg.Mesh3D
	floor     gg.Mesh3D
	light_pos []gg.Vec3
	light_col []gg.Color
	angle     f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{0, 5, 9}, target: gg.Vec3{0, 0.5, 0}, fovy: 45 }
	app.cam.orbit(30, 15, 10)
	app.cube = gg.gen_mesh_cube(gg.Vec3{2, 2, 2})
	app.floor = gg.gen_mesh_plane(14, 14, 1)
	app.light_col = [gg.red, gg.green, gg.rgb(51, 128, 255), gg.yellow, gg.magenta, gg.cyan, gg.orange,
		gg.purple]
	for _ in 0 .. max_lights {
		app.light_pos << gg.Vec3{}
	}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - lights bloom'
		bg_color:      gg.black
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
	for i in 0 .. max_lights {
		a := f32(i) / f32(max_lights) * 2.0 * f32(math.pi) + app.angle * 0.6
		radius := 4.2 + math.sinf(app.angle * 1.2 + f32(i)) * 0.4
		h := 1.0 + math.sinf(app.angle * 1.8 + f32(i)) * 0.6
		app.light_pos[i] = gg.Vec3{math.sinf(a) * radius, h, math.cosf(a) * radius}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{
		color:   gg.rgb(60, 60, 70)
		ambient: 0.15
		light:   gg.Light3D{ direction: gg.Vec3{0, -1, 0}, color: gg.white, intensity: 0.4 }
	}
	ctx.draw_material_mesh(&app.floor, gg.Vec3{}, gg.Vec3{}, 1, &mat, app.cam)
	ctx.draw_material_mesh(&app.cube, gg.Vec3{0, 1, 0}, gg.Vec3{0, app.angle * 10, 0}, 1, &mat,
		app.cam)
	for i in 0 .. max_lights {
		ctx.draw_sphere(app.light_pos[i], 0.12, 8, 8, app.light_col[i])
	}
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Bloom approximated with bright light spheres (no post-process shader)',
		size:  14
		color: gg.white
	)
	ctx.end()
}

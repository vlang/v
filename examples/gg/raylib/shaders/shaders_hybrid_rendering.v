// Port of raylib [shaders] example - hybrid rendering.
// PORT NOTE: shader hybrid_raymarch.fs/hybrid_raster.fs with shared depth requires gg user-shader
// support (not yet available); approximated by combining a raymarch-style CPU background
// (RenderTexture2D-prepared colors) with ordinary depth-tested 3D meshes.
module main

import gg
import math

const win_width = 800
const win_height = 450

struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	cam  gg.Camera3D
	cube gg.Mesh3D
	sph  gg.Mesh3D
	time f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{0.5, 1, 1.5}, target: gg.Vec3{0, 0.5, 0}, fovy: 45 }
	app.cam.orbit(30, 15, 2.5)
	app.cube = gg.gen_mesh_cube(gg.Vec3{0.6, 0.6, 0.6})
	app.sph = gg.gen_mesh_sphere(0.35, 10, 16)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - hybrid rendering'
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
	app.time += dt
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	// Raymarch-style animated background approximated with concentric shapes.
	for i in 0 .. 18 {
		r := 30 + f32(i) * 22 + math.sinf(app.time + f32(i) * 0.4) * 10
		c := gg.Color{u8(20 + i * 6), u8(40 + i * 4), u8(90 - i * 2), 255}
		ctx.draw_circle_filled(f32(win_width) / 2, f32(win_height) / 2, r, c)
	}
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{
		color:     gg.cyan
		ambient:   0.25
		light:     gg.Light3D{ direction: gg.Vec3{0.4, -1, 0.2} }
		fog_color: gg.black
		fog_near:  2
		fog_far:   8
	}
	ctx.draw_material_mesh(&app.cube, gg.Vec3{-0.6, 0.4, 0}, gg.Vec3{0, app.time * 30, 0}, 1, &mat,
		app.cam)
	mat.color = gg.magenta
	ctx.draw_material_mesh(&app.sph, gg.Vec3{0.6, 0.4, 0}, gg.Vec3{}, 1, &mat, app.cam)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Hybrid raymarch/raster shaders unavailable; CPU background + meshes',
		size:  14
		color: gg.white
	)
	ctx.end()
}

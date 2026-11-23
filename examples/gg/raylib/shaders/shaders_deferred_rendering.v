// Port of raylib [shaders] example - deferred rendering.
// PORT NOTE: shader gbuffer.vs/fs + deferred_shading.vs/fs with multiple render targets require
// gg user-shader support (not yet available); approximated with forward-lit Material3D meshes.
module main

import gg
import rand

const win_width = 800
const win_height = 450
const max_cubes = 30

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	cam   gg.Camera3D
	plane gg.Mesh3D
	cube  gg.Mesh3D
	cols  []gg.Color
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{5, 4, 5}, target: gg.Vec3{0, 1, 0}, fovy: 60 }
	app.cam.orbit(30, 20, 8)
	app.plane = gg.gen_mesh_plane(10, 10, 1)
	app.cube = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	for _ in 0 .. max_cubes {
		app.cols << gg.Color{u8(rand.intn(155) or { 80 } + 100), u8(rand.intn(155) or { 80 } + 100), u8(rand.intn(155) or { 80 } + 100), 255}
	}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - deferred rendering'
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
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	floor_mat := gg.Material3D{ color: gg.rgb(200, 200, 200), ambient: 0.5 }
	ctx.draw_material_mesh(&app.plane, gg.Vec3{}, gg.Vec3{}, 1, &floor_mat, app.cam)
	for i in 0 .. max_cubes {
		x := f32(i % 6 - 3) * 1.6
		z := f32(i / 6 - 2) * 1.6
		mat := gg.Material3D{
			color:   app.cols[i]
			ambient: 0.2
			light:   gg.Light3D{ direction: gg.Vec3{0.5, -1, 0.3} }
		}
		ctx.draw_material_mesh(&app.cube, gg.Vec3{x, 0.6, z}, gg.Vec3{}, 1, &mat, app.cam)
	}
	ctx.end_3d()
	ctx.draw_text(10, 10, 'G-buffer deferred shading unavailable; forward Material3D shown',
		size:  14
		color: gg.dark_gray
	)
	ctx.end()
}

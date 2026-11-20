// Port of raylib [shaders] example - simple mask.
// PORT NOTE: shader mask.fs with a two-texture mask requires gg user-shader support (not yet
// available); the masked look is approximated with Material3D meshes, fog and alternating colors.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg     &gg.Context = unsafe { nil }
	cam    gg.Camera3D
	torus  gg.Mesh3D
	cube   gg.Mesh3D
	sphere gg.Mesh3D
	frame  int
	rot    gg.Vec3
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{0, 1, 2}, target: gg.Vec3{}, fovy: 45 }
	app.cam.orbit(30, 10, 3)
	app.torus = gg.gen_mesh_sphere(0.6, 12, 16)
	app.cube = gg.gen_mesh_cube(gg.Vec3{0.8, 0.8, 0.8})
	app.sphere = gg.gen_mesh_sphere(1, 12, 16)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - simple mask'
		bg_color:      gg.dark_blue
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
	app.cam.update(.first_person, app.gg, dt)
	app.frame++
	app.rot.x += 0.01
	app.rot.y += 0.005
	app.rot.z -= 0.0025
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{
		color:     gg.rgb(120, 80, 200)
		ambient:   0.3
		light:     gg.Light3D{ direction: gg.Vec3{0.5, -1, 0.2} }
		fog_color: gg.dark_blue
		fog_near:  3
		fog_far:   10
	}
	ctx.draw_material_mesh(&app.torus, gg.Vec3{0.5, 0, 0}, app.rot, 1, &mat, app.cam)
	mat.color = gg.orange
	ctx.draw_material_mesh(&app.cube, gg.Vec3{-0.5, 0, 0}, gg.Vec3{0, 50, 0}, 1, &mat, app.cam)
	mat.color = gg.rgb(60, 60, 90)
	ctx.draw_material_mesh(&app.sphere, gg.Vec3{0, 0, -1.5}, gg.Vec3{}, 1, &mat, app.cam)
	ctx.draw_grid(10, 1, gg.blue)
	ctx.end_3d()
	ctx.draw_text(10, win_height - 30, 'Frame: ${app.frame}', size: 18, color: gg.white)
	ctx.draw_text(10, 10, 'Two-texture mask shader approximated with lit/fogged meshes',
		size:  14
		color: gg.white
	)
	ctx.end()
}

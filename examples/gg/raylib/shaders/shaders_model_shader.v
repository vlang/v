// Port of raylib [shaders] example - model shader.
// PORT NOTE: shader grayscale.fs requires gg user-shader support (not yet available);
// approximated with a neutral gray Material3D on generated meshes (a custom
// model shader is unavailable; gg does load OBJ/glTF via loaders.load_model).
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	cam  gg.Camera3D
	mesh gg.Mesh3D
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{4, 4, 4}, target: gg.Vec3{0, 1, -1}, fovy: 45 }
	app.cam.orbit(30, 15, 7)
	app.mesh = gg.gen_mesh_cube(gg.Vec3{2, 2, 2})
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - model shader'
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
	app.cam.update(.free, app.gg, dt)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{
		color:   gg.gray
		ambient: 0.6
	}
	ctx.draw_material_mesh(&app.mesh, gg.Vec3{}, gg.Vec3{}, 1, &mat, app.cam)
	ctx.draw_grid(10, 1, gg.gray)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Grayscale model shader approximated with a gray material',
		size:  16
		color: gg.dark_gray
	)
	ctx.end()
}

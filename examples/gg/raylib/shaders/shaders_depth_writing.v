// Port of raylib [shaders] example - depth writing.
// PORT NOTE: shader depth_write.fs (gl_FragDepth inversion) requires gg user-shader support
// (not yet available); approximated with a RenderTexture2D and ordinary depth-tested cubes.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg     &gg.Context = unsafe { nil }
	cam    gg.Camera3D
	target &gg.RenderTexture2D = unsafe { nil }
	cube1  gg.Mesh3D
	cube2  gg.Mesh3D
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{2, 2, 3}, target: gg.Vec3{0, 0.5, 0}, fovy: 45 }
	app.cam.orbit(30, 20, 4.5)
	app.cube1 = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.cube2 = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - depth writing'
		bg_color:      gg.white
		depth:         true
		user_data:     app
		init_fn:       init
		update_fn:     update
		frame_fn:      frame
		cleanup_fn:    cleanup
		create_window: true
	)
	app.gg.run()
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.target = gg.new_render_texture2d(win_width, win_height) or { return }
}

fn cleanup(data voidptr) {
	app := unsafe { &App(data) }
	if !isnil(app.target) {
		mut rt := app.target
		rt.free()
	}
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
	mat1 := gg.Material3D{ color: gg.purple, ambient: 0.4 }
	mat2 := gg.Material3D{ color: gg.yellow, ambient: 0.4 }
	ctx.draw_material_mesh(&app.cube1, gg.Vec3{0, 0.5, 1}, gg.Vec3{}, 1, &mat1, app.cam)
	ctx.draw_material_mesh(&app.cube2, gg.Vec3{0, 0.5, -1}, gg.Vec3{}, 1, &mat2, app.cam)
	ctx.draw_cube_wires(gg.Vec3{0, 0.5, 1}, gg.Vec3{1.1, 1.1, 1.1}, gg.red)
	ctx.draw_cube_wires(gg.Vec3{0, 0.5, -1}, gg.Vec3{1.1, 1.1, 1.1}, gg.dark_green)
	ctx.draw_grid(10, 1, gg.gray)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Custom depth texture / gl_FragDepth unavailable; standard depth used',
		size:  14
		color: gg.dark_gray
	)
	ctx.end()
}

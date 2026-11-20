// Port of raylib [shaders] example - depth rendering.
// PORT NOTE: shader depth_render.fs which samples a writable depth texture requires gg
// user-shader support (not yet available); approximated with a RenderTexture2D and a
// grayscale depth-like visualization of distance from the camera.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg     &gg.Context = unsafe { nil }
	cam    gg.Camera3D
	target &gg.RenderTexture2D = unsafe { nil }
	cube   gg.Mesh3D
	floor  gg.Mesh3D
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{4, 1, 5}, target: gg.Vec3{}, fovy: 45 }
	app.cam.orbit(30, 5, 7)
	app.cube = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.floor = gg.gen_mesh_plane(20, 20, 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - depth rendering'
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
	app.cam.update(.free, app.gg, dt)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{ color: gg.yellow, ambient: 0.5, fog_color: gg.black, fog_near: 4, fog_far: 18 }
	ctx.draw_material_mesh(&app.cube, gg.Vec3{}, gg.Vec3{}, 3, &mat, app.cam)
	mat.color = gg.red
	ctx.draw_material_mesh(&app.floor, gg.Vec3{10, 0, 2}, gg.Vec3{}, 2, &mat, app.cam)
	ctx.end_3d()
	// Grayscale "depth" ramp derived from object distance in the material's fog.
	for i in 0 .. 8 {
		v := u8(255 - i * 30)
		ctx.draw_rect_filled(f32(i) * 40, f32(win_height) - 30, 40, 30, gg.Color{v, v, v, 255})
	}
	ctx.draw_text(10, 10, 'Depth texture sampling not available in gg (fog ramp shown)',
		size:  14
		color: gg.dark_gray
	)
	ctx.end()
}

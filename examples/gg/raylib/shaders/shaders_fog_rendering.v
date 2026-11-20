// Port of raylib [shaders] example - fog rendering.
// PORT NOTE: shader lighting.vs/fog.fs requires gg user-shader support (not yet available);
// approximated with Material3D.fog_color/fog_near/fog_far distance fog.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	cam         gg.Camera3D
	torus       gg.Mesh3D
	cube        gg.Mesh3D
	sphere      gg.Mesh3D
	fog_density f32 = 0.15
	rotation    gg.Vec3
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{2, 2, 6}, target: gg.Vec3{0, 0.5, 0}, fovy: 45 }
	app.cam.orbit(30, 15, 7)
	app.torus = gg.gen_mesh_sphere(0.5, 12, 16)
	app.cube = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.sphere = gg.gen_mesh_sphere(0.5, 16, 16)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - fog rendering'
		bg_color:      gg.gray
		depth:         true
		user_data:     app
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.gg, dt)
	if app.gg.pressed_keys[gg.KeyCode.up] {
		app.fog_density += 0.01 * dt * 60
		if app.fog_density > 1 {
			app.fog_density = 1
		}
	}
	if app.gg.pressed_keys[gg.KeyCode.down] {
		app.fog_density -= 0.01 * dt * 60
		if app.fog_density < 0 {
			app.fog_density = 0
		}
	}
	app.rotation.x -= 0.025
	app.rotation.z += 0.012
}

fn event(e &gg.Event, data voidptr) {
	_ = e
	_ = data
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	fog_far := 6.0 / (app.fog_density + 0.05)
	mat := gg.Material3D{
		color:     gg.white
		ambient:   0.2
		light:     gg.Light3D{ direction: gg.Vec3{0.3, -1, 0.4} }
		fog_color: gg.gray
		fog_near:  fog_far * 0.25
		fog_far:   fog_far
	}
	ctx.draw_material_mesh(&app.torus, gg.Vec3{}, app.rotation, 1, &mat, app.cam)
	ctx.draw_material_mesh(&app.cube, gg.Vec3{-2.6, 0, 0}, gg.Vec3{}, 1, &mat, app.cam)
	ctx.draw_material_mesh(&app.sphere, gg.Vec3{2.6, 0, 0}, gg.Vec3{}, 1, &mat, app.cam)
	for i in -10 .. 10 {
		ctx.draw_material_mesh(&app.torus, gg.Vec3{f32(i) * 2, 0, 2}, gg.Vec3{}, 1, &mat, app.cam)
	}
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Use UP/DOWN to change fog density [${app.fog_density:.2f}]',
		size:  20
		color: gg.white
	)
	ctx.end()
}

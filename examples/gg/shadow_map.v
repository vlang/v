// A real shadow map: depth is rendered from the light's point of view and
// sampled while shading, so the cube casts a shadow onto the ground plane.
module main

import gg

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	cam    gg.Camera3D
	shadow &gg.ShadowMap3D = unsafe { nil }
	angle  f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.new_camera3d(gg.Vec3{0, 4, 9}, gg.Vec3{})
	app.cam.orbit(30, 20, 9)
	app.shadow = gg.new_shadow_map3d(512, gg.Vec3{-0.5, -1, -0.3})!
	app.ctx = gg.new_context(
		width:        800
		height:       600
		window_title: 'gg 3D shadow map'
		depth:        true
		frame_fn:     frame
		update_fn:    update
		user_data:    app
	)
	app.ctx.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.angle += dt * 30
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	mat := gg.Material3D{
		color:   gg.white
		ambient: 0.3
		light:   gg.Light3D{ direction: gg.Vec3{-0.5, -1, -0.3}, intensity: 0.9 }
	}

	// Pass 1: capture the light's depth from the scene geometry.
	ctx.r.rewind()
	plane := gg.gen_mesh_plane(16, 16, 4)
	ctx.draw_material_mesh(&plane, gg.Vec3{0, -1, 0}, gg.Vec3{}, 1.0, &mat, app.cam)
	cube := gg.gen_mesh_cube(gg.Vec3{1, 2, 1})
	ctx.draw_material_mesh(&cube, gg.Vec3{0, 0.2, 0}, gg.Vec3{0, app.angle, 0}, 1.0, &mat, app.cam)
	mut shadow := app.shadow
	shadow.strength = 0.95
	shadow.capture_depth(ctx, gg.Vec3{}, 8, 0.1, 40)

	// Pass 2: re-record with shadow sampling for the real GPU frame.
	ctx.r.rewind()
	ctx.set_shadow_map(shadow)
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	plane2 := gg.gen_mesh_plane(16, 16, 4)
	ctx.draw_material_mesh(&plane2, gg.Vec3{0, -1, 0}, gg.Vec3{}, 1.0, &mat, app.cam)
	cube2 := gg.gen_mesh_cube(gg.Vec3{1, 2, 1})
	ctx.draw_material_mesh(&cube2, gg.Vec3{0, 0.2, 0}, gg.Vec3{0, app.angle, 0}, 1.0, &mat, app.cam)
	ctx.end_3d()
	ctx.clear_shadow_map()
	ctx.end()
}

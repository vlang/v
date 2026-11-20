module main

import gg
import os.asset

struct App {
mut:
	gg     &gg.Context = unsafe { nil }
	cam    gg.Camera3D
	mesh   gg.Mesh3D
	sphere gg.Mesh3D
	image  gg.Image
	angle  f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{}
	app.cam.orbit(30, 15, 9)
	app.mesh = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.sphere = gg.gen_mesh_sphere(0.8, 8, 16)
	app.gg = gg.new_context(
		width:        640
		height:       480
		window_title: 'gg 3D material'
		bg_color:     gg.rgb(20, 24, 32)
		depth:        true
		init_fn:      init_image
		update_fn:    update
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn init_image(data voidptr) {
	mut app := unsafe { &App(data) }
	path := asset.get_path('../assets', 'logo.png')
	app.image = app.gg.create_image(path) or {
		eprintln('could not load logo.png: ${err}')
		return
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.angle += dt * 30.0
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.draw_grid(20, 1.0, gg.gray)
	mut mat := gg.Material3D{
		color:     gg.red
		ambient:   0.2
		light:     gg.Light3D{ direction: gg.Vec3{0.5, -1, 0.25} }
		fog_color: gg.rgb(20, 24, 32)
		fog_near:  5
		fog_far:   20
	}
	ctx.draw_material_mesh(&app.mesh, gg.Vec3{-1.5, 0, 0}, gg.Vec3{0, app.angle, 0}, 1.0,
		&mat, app.cam)
	mat.color = gg.blue
	ctx.draw_material_mesh(&app.sphere, gg.Vec3{1.5, 0, 0}, gg.Vec3{}, 1.0, &mat, app.cam)
	if app.image.ok {
		ctx.draw_billboard(app.cam, &app.image, gg.Vec3{0, 2.2, 0}, 1.5, 1.5, gg.white)
	}
	ctx.end_3d()
	ctx.end()
}

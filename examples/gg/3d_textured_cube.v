module main

import gg
import os.asset

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	cube     gg.Mesh3D
	image    gg.Image
	angle    f32
	show_col bool
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{}
	app.cam.orbit(30, 15, 6)
	app.cube = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.gg = gg.new_context(
		width:        640
		height:       480
		window_title: 'gg textured cube'
		bg_color:     gg.rgb(20, 24, 32)
		depth:        true
		init_fn:      init_image
		event_fn:     on_event
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

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.show_col = !app.show_col
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.angle += dt * 35.0
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.begin_3d(app.cam)
	ctx.draw_grid(20, 1.0, gg.gray)
	mut mat := gg.Material3D{ ambient: 0.8 }
	if app.show_col {
		mat.color = gg.cyan
	} else if app.image.ok {
		mat.color = gg.white
		mat.texture = &app.image
	}
	ctx.draw_material_mesh(&app.cube, gg.Vec3{}, gg.Vec3{app.angle * 0.4, app.angle, 0}, 1.3,
		&mat, app.cam)
	ctx.end_3d()
	ctx.end()
}

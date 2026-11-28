module main

// Port of raylib models_billboard_rendering.c
// PORT NOTE: gg's draw_billboard() supports a whole image only; the rotated,
// source-rect-based DrawBillboardPro billboard is approximated with a second
// whole-image billboard of a different size.
import gg
import math

const bb_raywhite = gg.Color{245, 245, 245, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	tex      gg.Image
	rotation f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{5, 4, 5}
		target:   gg.Vec3{0, 2, 0}
		fovy:     45
	}
	app.tex = app.ctx.create_image('resources/billboard.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.rotation += 0.4
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	static_pos := gg.Vec3{0, 2, 0}
	rotating_pos := gg.Vec3{1, 2, 1}
	ds := math.sqrt(f64((app.cam.position.x - static_pos.x) * (app.cam.position.x - static_pos.x) +
		(app.cam.position.y - static_pos.y) * (app.cam.position.y - static_pos.y) +
		(app.cam.position.z - static_pos.z) * (app.cam.position.z - static_pos.z)))
	dr := math.sqrt(f64((app.cam.position.x - rotating_pos.x) * (app.cam.position.x - rotating_pos.x) +
		(app.cam.position.y - rotating_pos.y) * (app.cam.position.y - rotating_pos.y) +
		(app.cam.position.z - rotating_pos.z) * (app.cam.position.z - rotating_pos.z)))
	if ds > dr {
		app.ctx.draw_billboard(app.cam, &app.tex, static_pos, 2, 2, gg.white)
		app.ctx.draw_billboard(app.cam, &app.tex, rotating_pos, 2.2, 1.0, gg.white)
	} else {
		app.ctx.draw_billboard(app.cam, &app.tex, rotating_pos, 2.2, 1.0, gg.white)
		app.ctx.draw_billboard(app.cam, &app.tex, static_pos, 2, 2, gg.white)
	}
	app.ctx.end_3d()
	app.ctx.end()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.ctx, dt)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - billboard rendering'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     bb_raywhite
		depth:        true
	)
	app.ctx.run()
}

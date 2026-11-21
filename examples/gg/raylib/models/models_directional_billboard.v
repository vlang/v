module main

// Port of raylib models_directional_billboard.c
// PORT NOTE: gg's draw_billboard() cannot select a source rectangle from a
// sprite sheet, so the 8-direction/4-animation frame selection is reported in
// the HUD while a whole-image billboard is drawn at the origin.
import gg
import math

const db_raywhite = gg.Color{245, 245, 245, 255}
const db_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	cam   gg.Camera3D
	tex   gg.Image
	timer f32
	anim  int
	dir   f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{2, 1, 2}
		target:   gg.Vec3{0, 0.5, 0}
		fovy:     45
	}
	app.tex = app.ctx.create_image('resources/skillbot.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	// Direction frame derived from the camera angle around the billboard.
	base := math.atan2(f64(app.cam.position.z - 0), f64(app.cam.position.x - 2))
	app.dir = f32(math.floor(base / f64(math.pi) * 4.0 + 0.25))
	if app.dir < 0 {
		app.dir = 8 - math.abs(app.dir)
	}
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.draw_billboard(app.cam, &app.tex, gg.Vec3{}, 1, 1, gg.white)
	app.ctx.end_3d()
	app.ctx.draw_text(10, 10, 'animation: ${app.anim}', size: 20, color: db_darkgray)
	app.ctx.draw_text(10, 40, 'direction frame: ${app.dir:.0f} (source rect unavailable)',
		size: 20, color: db_darkgray)
	app.ctx.end()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.timer += dt
	if app.timer > 0.5 {
		app.timer = 0
		app.anim++
	}
	if app.anim >= 4 {
		app.anim = 0
	}
	app.cam.update(.orbital, app.ctx, dt)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - directional billboard'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     db_raywhite
		depth:        true
	)
	app.ctx.run()
}

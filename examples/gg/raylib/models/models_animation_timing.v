module main

// Port of raylib models_animation_timing.c
// PORT NOTE: raygui controls and animation poses are unavailable in gg; a
// text HUD reports the play state and speed while the model stays static.
import gg
import gg.loaders

const at_raywhite = gg.Color{245, 245, 245, 255}
const at_gray = gg.Color{128, 128, 128, 255}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	cam       gg.Camera3D
	model     gg.Model3D
	has_model bool
	pause     bool
	speed     f32 = 0.5
	frame     f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{6, 6, 6}
		target:   gg.Vec3{0, 2, 0}
		fovy:     45
	}
	app.model = loaders.load_model('resources/models/gltf/robot.glb') or { gg.Model3D{} }
	app.has_model = app.model.valid()
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	if app.has_model {
		app.ctx.draw_model(&app.model, app.cam)
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(10, 10, 'FRAME SPEED: ${app.speed:.1f} | P: pause (animation unavailable)',
		size:  16
		color: at_gray
	)
	app.ctx.draw_text(10, 390, 'CURRENT FRAME: ${app.frame:.2f} / (no keyframes in gg)',
		size:  16
		color: at_gray
	)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .p {
		app.pause = !app.pause
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if !app.pause {
		app.frame += app.speed * dt * 60
		if app.frame > 100 {
			app.frame = 0
		}
	}
	app.cam.update(.orbital, app.ctx, dt)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - animation timing'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     at_raywhite
		depth:        true
	)
	app.ctx.run()
}

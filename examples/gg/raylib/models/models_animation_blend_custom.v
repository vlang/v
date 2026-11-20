module main

// Port of raylib models_animation_blend_custom.c
// PORT NOTE: per-bone blending and CPU skinning require skeleton data that gg
// does not load. The character and equipment are drawn as static models.
import gg

const abc_raywhite = gg.Color{245, 245, 245, 255}
const abc_gray = gg.Color{128, 128, 128, 255}
const abc_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	cam       gg.Camera3D
	model     gg.Model3D
	has_model bool
	upper     bool = true
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{4, 4, 4}
		target:   gg.Vec3{0, 1, 0}
		fovy:     45
	}
	app.model = gg.load_model('resources/models/gltf/greenman.glb') or { gg.Model3D{} }
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
	app.ctx.draw_text(10, 10, 'ANIM 0: walk (blending unavailable in gg)', size: 20, color: abc_gray)
	app.ctx.draw_text(10, 40, 'ANIM 1: attack', size: 20, color: abc_gray)
	mode := if app.upper { 'Upper/Lower Body Blending' } else { 'Uniform Blending' }
	app.ctx.draw_text(10, 420, '[SPACE] Toggle blending mode: ${mode}', size: 20, color: abc_darkgray)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.upper = !app.upper
	}
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
		window_title: 'raylib [models] example - animation blend custom'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     abc_raywhite
		depth:        true
	)
	app.ctx.run()
}

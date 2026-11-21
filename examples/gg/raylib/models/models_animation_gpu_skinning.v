module main

// Port of raylib models_animation_gpu_skinning.c
// PORT NOTE: gg has no skinning shader support and does not load animation
// data; the model is displayed statically. LEFT/RIGHT keys switch the reported
// animation slot as in the original.
import gg
import gg.loaders

const ags_raywhite = gg.Color{245, 245, 245, 255}
const ags_gray = gg.Color{128, 128, 128, 255}
const ags_maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	cam       gg.Camera3D
	model     gg.Model3D
	has_model bool
	anim      int
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{5, 5, 5}
		target:   gg.Vec3{0, 1, 0}
		fovy:     45
	}
	app.model = loaders.load_model('resources/models/gltf/greenman.glb') or { gg.Model3D{} }
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
	app.ctx.draw_text(10, 40, 'Current animation: ${app.anim} (static - no GPU skinning)',
		size:  20
		color: ags_maroon
	)
	app.ctx.draw_text(10, 10, 'Use the LEFT/RIGHT keys to switch animation',
		size:  20
		color: ags_gray
	)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		if e.key_code == .right {
			app.anim++
		} else if e.key_code == .left && app.anim > 0 {
			app.anim--
		}
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
		window_title: 'raylib [models] example - animation gpu skinning'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     ags_raywhite
		depth:        true
	)
	app.ctx.run()
}

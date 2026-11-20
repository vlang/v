module main

// Port of raylib models_loading_gltf.c
// PORT NOTE: gg's load_model() imports geometry from GLB/GLTF but not the
// skeleton/animation channels. The model is drawn statically; LEFT/RIGHT keys
// still report the intended animation names but cannot drive a pose.
import gg

const gltf_raywhite = gg.Color{245, 245, 245, 255}
const gltf_gray = gg.Color{128, 128, 128, 255}
const gltf_maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	model    gg.Model3D
	has_model bool
	anim_index int
	names    []string
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{6, 6, 6}
		target:   gg.Vec3{0, 2, 0}
		fovy:     45
	}
	app.model = gg.load_model('resources/models/gltf/robot.glb') or { gg.Model3D{} }
	app.has_model = app.model.valid()
	app.names = ['idle', 'walk', 'run', 'attack']
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
	name := if app.anim_index < app.names.len { app.names[app.anim_index] } else { 'unknown' }
	app.ctx.draw_text(10, 40, 'Current animation: ${name} (static - no skinning)',
		size: 20, color: gltf_maroon)
	app.ctx.draw_text(10, 10, 'Use the LEFT/RIGHT keys to switch animation', size: 20, color: gltf_gray)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		if e.key_code == .right {
			app.anim_index = (app.anim_index + 1) % app.names.len
		} else if e.key_code == .left {
			app.anim_index = (app.anim_index + app.names.len - 1) % app.names.len
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
		window_title: 'raylib [models] example - loading gltf'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     gltf_raywhite
		depth:        true
	)
	app.ctx.run()
}

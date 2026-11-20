module main

// Port of raylib models_animation_blending.c
// PORT NOTE: gg's load_model() does not load glTF skins or animations, so
// UpdateModelAnimation()/UpdateModelAnimationEx() cannot be ported. The model
// is drawn statically while the state variables and key controls are kept to
// show what the original blending UI did.
import gg

const ab_raywhite = gg.Color{245, 245, 245, 255}
const ab_gray = gg.Color{128, 128, 128, 255}
const ab_blue = gg.Color{0, 121, 241, 255}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	cam        gg.Camera3D
	model      gg.Model3D
	has_model  bool
	pause      bool
	transition bool
	blend      f32
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
	if app.transition {
		app.ctx.draw_text(170, 50, 'ANIM TRANSITION BLENDING!', size: 30, color: ab_blue)
	}
	app.ctx.draw_text(10, 10, 'SPACE: blend animations | P: pause (skinning unavailable in gg)',
		size: 16, color: ab_gray)
	app.ctx.draw_text(180, 14, 'blend: ${app.blend:.2f}', size: 16, color: ab_gray)
	app.ctx.draw_text(0, 350, 'PRESS SPACE to START BLENDING', size: 20, color: ab_gray)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	if e.key_code == .p {
		app.pause = !app.pause
	} else if e.key_code == .space && !app.pause {
		app.transition = !app.transition
		app.blend = if app.transition { 0 } else { 1 }
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if app.transition && !app.pause {
		app.blend += dt / 2
		if app.blend > 1 {
			app.blend = 0
			app.transition = false
		}
	}
	app.cam.update(.orbital, app.ctx, dt)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - animation blending'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     ab_raywhite
		depth:        true
	)
	app.ctx.run()
}

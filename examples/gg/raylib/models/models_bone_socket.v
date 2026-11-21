module main

// Port of raylib models_bone_socket.c
// PORT NOTE: gg does not load skeletons, bone sockets or animation poses. The
// character and the three equipment models are drawn at fixed offsets around
// a static character instead of being attached to animated socket bones.
import gg
import gg.loaders

const bs_raywhite = gg.Color{245, 245, 245, 255}
const bs_gray = gg.Color{128, 128, 128, 255}
const bs_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	cam   gg.Camera3D
	model gg.Model3D
	equip [3]gg.Model3D
	show  [3]bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{5, 5, 5}
		target:   gg.Vec3{0, 2, 0}
		fovy:     45
	}
	app.model = loaders.load_model('resources/models/gltf/greenman.glb') or { gg.Model3D{} }
	app.equip[0] = loaders.load_model('resources/models/gltf/greenman_hat.glb') or { gg.Model3D{} }
	app.equip[1] = loaders.load_model('resources/models/gltf/greenman_sword.glb') or { gg.Model3D{} }
	app.equip[2] = loaders.load_model('resources/models/gltf/greenman_shield.glb') or { gg.Model3D{} }
	app.show = [true, true, true]!
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	offsets := [gg.Vec3{0, 2.2, 0}, gg.Vec3{0.6, 1.2, 0}, gg.Vec3{-0.6, 1.2, 0}]!
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	if app.model.valid() {
		app.ctx.draw_model(&app.model, app.cam)
	}
	for i in 0 .. 3 {
		if app.show[i] && app.equip[i].valid() {
			app.equip[i].pos = offsets[i]
			app.ctx.draw_model(&app.equip[i], app.cam)
		}
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(10, 10, 'Use the T/G to switch animation (unavailable)',
		size:  20
		color: bs_gray
	)
	app.ctx.draw_text(10, 35, 'Use F/H to rotate character (unavailable)', size: 20, color: bs_gray)
	app.ctx.draw_text(10, 60, 'Use 1,2,3 to toggle hat, sword and shield',
		size:  20
		color: bs_darkgray
	)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	if e.key_code == gg.KeyCode._1 {
		app.show[0] = !app.show[0]
	} else if e.key_code == gg.KeyCode._2 {
		app.show[1] = !app.show[1]
	} else if e.key_code == gg.KeyCode._3 {
		app.show[2] = !app.show[2]
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
		window_title: 'raylib [models] example - bone socket'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     bs_raywhite
		depth:        true
	)
	app.ctx.run()
}

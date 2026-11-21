module main

// Port of raylib models_yaw_pitch_roll.c
import gg
import gg.loaders

const ypr_raywhite = gg.Color{245, 245, 245, 255}
const ypr_darkgreen = gg.Color{0, 128, 0, 255}
const ypr_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	cam       gg.Camera3D
	model     gg.Model3D
	has_model bool
	texture   gg.Image
	pitch     f32
	yaw       f32
	roll      f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 50, -120}
		target:   gg.Vec3{}
		fovy:     30
	}
	app.texture = app.ctx.create_image('resources/models/obj/plane_diffuse.png') or { gg.Image{} }
	app.model = loaders.load_model('resources/models/obj/plane.obj') or { gg.Model3D{} }
	app.has_model = app.model.valid()
	if app.has_model {
		app.model.material.texture = &app.texture
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.pressed_keys[gg.KeyCode.down] {
		app.pitch += 0.6
	} else if app.ctx.pressed_keys[gg.KeyCode.up] {
		app.pitch -= 0.6
	} else {
		if app.pitch > 0.3 { app.pitch -= 0.3 }
		if app.pitch < -0.3 { app.pitch += 0.3 }
	}
	if app.ctx.pressed_keys[gg.KeyCode.s] {
		app.yaw -= 1
	} else if app.ctx.pressed_keys[gg.KeyCode.a] {
		app.yaw += 1
	} else {
		if app.yaw > 0 { app.yaw -= 0.5 }
		if app.yaw < 0 { app.yaw += 0.5 }
	}
	if app.ctx.pressed_keys[gg.KeyCode.left] {
		app.roll -= 1
	} else if app.ctx.pressed_keys[gg.KeyCode.right] {
		app.roll += 1
	} else {
		if app.roll > 0 { app.roll -= 0.5 }
		if app.roll < 0 { app.roll += 0.5 }
	}
	app.model.rotation = gg.Vec3{app.pitch, app.yaw, app.roll}

	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	if app.has_model {
		app.model.pos = gg.Vec3{0, -8, 0}
		app.ctx.draw_model(&app.model, app.cam)
	}
	app.ctx.draw_grid(10, 10.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_rect_filled(30, 370, 260, 70, gg.Color{0, 255, 0, 128})
	app.ctx.draw_rect_empty(30, 370, 260, 70, ypr_darkgreen)
	app.ctx.draw_text(40, 380, 'Pitch controlled with: KEY_UP / KEY_DOWN',
		size:  10
		color: ypr_darkgray
	)
	app.ctx.draw_text(40, 400, 'Roll controlled with: KEY_LEFT / KEY_RIGHT',
		size:  10
		color: ypr_darkgray
	)
	app.ctx.draw_text(40, 420, 'Yaw controlled with: KEY_A / KEY_S', size: 10, color: ypr_darkgray)
	app.ctx.draw_text(560, 430, '(c) WWI Plane Model created by GiaHanLam',
		size:  10
		color: ypr_darkgray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - yaw pitch roll'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     ypr_raywhite
		depth:        true
	)
	app.ctx.run()
}

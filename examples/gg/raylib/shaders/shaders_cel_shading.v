// Port of raylib [shaders] example - cel shading.
// PORT NOTE: shader cel.vs/cel.fs and the inverted-hull outline shader require gg user-shader
// support (not yet available); approximated with flat ambient levels in Material3D and a
// wireframe silhouette pass.
module main

import gg
import math

const win_width = 800
const win_height = 450

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	cam         gg.Camera3D
	mesh        gg.Mesh3D
	cel_enabled bool = true
	outline     bool = true
	bands       f32  = 10
	light_angle f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{9, 6, 9}, target: gg.Vec3{0, 1, 0}, fovy: 45 }
	app.cam.orbit(30, 20, 12)
	app.mesh = gg.gen_mesh_sphere(1.2, 16, 24)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - cel shading'
		bg_color:      gg.white
		depth:         true
		user_data:     app
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.gg, dt)
	app.light_angle -= dt * 0.3
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	match e.key_code {
		.z { app.cel_enabled = !app.cel_enabled }
		.c { app.outline = !app.outline }
		.e { app.bands += 1 }
		.q { app.bands -= 1 }
		else { return }
	}
	if app.bands < 2 {
		app.bands = 2
	}
	if app.bands > 20 {
		app.bands = 20
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	light_pos := gg.Vec3{math.sinf(app.light_angle) * 5, 5, math.cosf(app.light_angle) * 5}
	ambient := if app.cel_enabled { 1.0 / (app.bands / 2) } else { f32(0.4) }
	mat := gg.Material3D{
		color:   gg.rgb(200, 210, 220)
		ambient: ambient
		light:   gg.Light3D{ direction: (gg.Vec3{} - light_pos).normalized(), color: gg.white }
	}
	ctx.draw_material_mesh(&app.mesh, gg.Vec3{0, 1.2, 0}, gg.Vec3{}, 1, &mat, app.cam)
	if app.outline {
		ctx.draw_sphere(gg.Vec3{0, 1.2, 0}, 1.24, 12, 16, gg.dark_gray)
	}
	ctx.draw_sphere(light_pos, 0.2, 8, 8, gg.yellow)
	ctx.draw_grid(10, 10, gg.gray)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Cel: ${app.cel_enabled}  [Z]',
		size:  20
		color: if app.cel_enabled { gg.dark_green } else { gg.dark_gray }
	)
	ctx.draw_text(10, 35, 'Outline: ${app.outline}  [C]',
		size:  20
		color: if app.outline { gg.dark_green } else { gg.dark_gray }
	)
	ctx.draw_text(10, 60, 'Bands: ${app.bands:.0f}  [Q/E]', size: 20, color: gg.dark_gray)
	ctx.end()
}

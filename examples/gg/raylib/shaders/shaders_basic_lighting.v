// Port of raylib [shaders] example - basic lighting.
// PORT NOTE: shader lighting.vs/lighting.fs requires gg user-shader support (not yet available);
// approximated with Material3D per-vertex directional lighting (gg bakes lighting into colors).
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg      &gg.Context = unsafe { nil }
	cam     gg.Camera3D
	cube    gg.Mesh3D
	plane   gg.Mesh3D
	enabled []bool = [true, true, true, true]
	lights  []gg.Vec3
	colors  []gg.Color
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{2, 4, 6}, target: gg.Vec3{0, 0.5, 0}, fovy: 45 }
	app.cam.orbit(30, 20, 7.5)
	app.cube = gg.gen_mesh_cube(gg.Vec3{2, 4, 2})
	app.plane = gg.gen_mesh_plane(10, 10, 1)
	app.lights = [gg.Vec3{-2, 1, -2}, gg.Vec3{2, 1, 2}, gg.Vec3{-2, 1, 2}, gg.Vec3{2, 1, -2}]
	app.colors = [gg.yellow, gg.red, gg.green, gg.blue]
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - basic lighting'
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
	_ = dt
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.gg, dt)
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	idx := match e.key_code {
		.y { 0 }
		.r { 1 }
		.g { 2 }
		.b { 3 }
		else { return }
	}
	app.enabled[idx] = !app.enabled[idx]
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{
		color:   gg.white
		ambient: 0.1
		light:   gg.Light3D{ direction: gg.Vec3{0.4, -1, 0.3}, color: gg.white, intensity: 0.9 }
	}
	ctx.draw_material_mesh(&app.plane, gg.Vec3{}, gg.Vec3{}, 1, &mat, app.cam)
	ctx.draw_material_mesh(&app.cube, gg.Vec3{}, gg.Vec3{}, 1, &mat, app.cam)
	for i, pos in app.lights {
		c := app.colors[i]
		if app.enabled[i] {
			ctx.draw_sphere(pos, 0.2, 8, 8, c)
		} else {
			ctx.draw_sphere(pos, 0.2, 8, 8, gg.Color{c.r, c.g, c.b, 80})
		}
	}
	ctx.draw_grid(10, 1, gg.gray)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Use keys [Y][R][G][B] to toggle lights', size: 20, color: gg.dark_gray)
	ctx.draw_text(10, 40, 'gg Material3D lighting approximation', size: 14, color: gg.gray)
	ctx.end()
}

// Port of raylib [shaders] example - basic PBR.
// PORT NOTE: shader pbr.vs/pbr.fs with metalness/roughness/normal maps requires gg user-shader
// support (not yet available); approximated with Material3D lighting on a cube and emissive
// spheres for the four lights.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg      &gg.Context = unsafe { nil }
	cam     gg.Camera3D
	car     gg.Mesh3D
	floor   gg.Mesh3D
	lights  []gg.Vec3
	colors  []gg.Color
	enabled []bool = [true, true, true, true]
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{2, 2, 6}, target: gg.Vec3{0, 0.5, 0}, fovy: 45 }
	app.cam.orbit(30, 15, 7)
	app.car = gg.gen_mesh_cube(gg.Vec3{1.6, 0.8, 3})
	app.floor = gg.gen_mesh_plane(20, 20, 1)
	app.lights = [gg.Vec3{-1, 1, -2}, gg.Vec3{2, 1, 1}, gg.Vec3{-2, 1, 1}, gg.Vec3{1, 1, -2}]
	app.colors = [gg.yellow, gg.green, gg.red, gg.blue]
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - basic pbr'
		bg_color:      gg.black
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
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	idx := match e.key_code {
		._1 { 2 }
		._2 { 1 }
		._3 { 3 }
		._4 { 0 }
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
	floor_mat := gg.Material3D{
		color:   gg.rgb(60, 60, 65)
		ambient: 0.02
		light:   gg.Light3D{ direction: gg.Vec3{0, -1, 0}, intensity: 0.6 }
	}
	ctx.draw_material_mesh(&app.floor, gg.Vec3{}, gg.Vec3{}, 5, &floor_mat, app.cam)
	car_mat := gg.Material3D{
		color:   gg.rgb(180, 120, 90)
		ambient: 0.05
		light:   gg.Light3D{ direction: gg.Vec3{0.3, -1, 0.2}, color: gg.rgb(255, 162, 0) }
	}
	ctx.draw_material_mesh(&app.car, gg.Vec3{}, gg.Vec3{}, 0.25, &car_mat, app.cam)
	for i, pos in app.lights {
		if app.enabled[i] {
			ctx.draw_sphere(pos, 0.15, 8, 8, app.colors[i])
		}
	}
	ctx.end_3d()
	ctx.draw_text(10, 10, 'PBR maps not available: Material3D lighting approximation',
		size:  16
		color: gg.white
	)
	ctx.draw_text(10, 32, 'Keys 1-4 toggle lights', size: 14, color: gg.light_gray)
	ctx.end()
}

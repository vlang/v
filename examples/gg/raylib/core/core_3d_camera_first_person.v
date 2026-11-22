module main

// raylib [core] example - 3d camera first person
// Switch camera modes with 1/2/3/4 and projection with P. gg provides
// orbital, first-person and free modes; raylib's third-person mode is
// approximated by first-person with a visible player marker.

import gg
import rand
import sokol.sapp

const screen_width = 800
const screen_height = 450
const max_columns = 20

const ray_white = gg.Color{245, 245, 245, 255}
const sky_blue = gg.Color{102, 191, 255, 128}
const blue = gg.Color{0, 121, 241, 255}
const dark_blue = gg.Color{0, 82, 172, 255}
const black = gg.Color{0, 0, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}
const dark_purple = gg.Color{85, 0, 128, 255}
const purple = gg.Color{127, 0, 255, 255}
const lime = gg.Color{124, 252, 0, 255}
const gold = gg.Color{255, 203, 0, 255}

struct App {
mut:
	gg        &gg.Context   = unsafe { nil }
	cam       gg.Camera3D   = gg.Camera3D{ position: gg.Vec3{0, 2, 4}, target: gg.Vec3{0, 2, 0} }
	mode      gg.CameraMode = .first_person
	mode_key  int
	heights   []f32      = []
	positions []gg.Vec3  = []
	colors    []gg.Color = []
}

fn main() {
	mut app := &App{}
	app.cam.fovy = 60
	for _ in 0 .. max_columns {
		h := f32(rand.int_in_range(1, 12) or { 4 })
		app.heights << h
		app.positions << gg.Vec3{f32(rand.int_in_range(-15, 15) or { 0 }), h / 2, f32(rand.int_in_range(-15, 15) or { 0 })}
		app.colors << gg.Color{u8(rand.int_in_range(20, 255) or { 100 }), u8(rand.int_in_range(10, 55) or { 30 }), 30, 255}
	}
	sapp.lock_mouse(true)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 3d camera first person'
		bg_color:     ray_white
		depth:        true
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down || e.key_repeat {
		return
	}
	match e.key_code {
		._1 {
			app.mode = .free
			app.mode_key = 0
			app.cam.up = gg.Vec3{0, 1, 0}
		}
		._2 {
			app.mode = .first_person
			app.mode_key = 1
			app.cam.up = gg.Vec3{0, 1, 0}
		}
		._3 {
			// PORT NOTE: gg has no third-person camera mode.
			app.mode = .first_person
			app.mode_key = 2
			app.cam.up = gg.Vec3{0, 1, 0}
		}
		._4 {
			app.mode = .orbital
			app.mode_key = 3
			app.cam.up = gg.Vec3{0, 1, 0}
		}
		.p {
			if app.cam.projection == .perspective {
				app.cam.position = gg.Vec3{0, 2, -100}
				app.cam.target = gg.Vec3{0, 2, 0}
				app.cam.projection = .orthographic
				app.cam.ortho_size = 12
			} else {
				app.cam.position = gg.Vec3{0, 2, 10}
				app.cam.target = gg.Vec3{0, 2, 0}
				app.cam.projection = .perspective
				app.cam.fovy = 60
			}
		}
		else {}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(app.mode, app.gg, dt)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.draw_plane(gg.Vec3{}, 32, 32, gg.light_gray)
	ctx.draw_cube(gg.Vec3{-16, 2.5, 0}, gg.Vec3{1, 5, 32}, blue)
	ctx.draw_cube(gg.Vec3{16, 2.5, 0}, gg.Vec3{1, 5, 32}, lime)
	ctx.draw_cube(gg.Vec3{0, 2.5, 16}, gg.Vec3{32, 5, 1}, gold)
	for i in 0 .. max_columns {
		size := gg.Vec3{2, app.heights[i], 2}
		ctx.draw_cube(app.positions[i], size, app.colors[i])
		ctx.draw_cube_wires(app.positions[i], size, maroon)
	}
	if app.mode_key == 2 {
		ctx.draw_cube(app.cam.target, gg.Vec3{0.5, 0.5, 0.5}, purple)
		ctx.draw_cube_wires(app.cam.target, gg.Vec3{0.5, 0.5, 0.5}, dark_purple)
	}
	ctx.end_3d()

	ctx.draw_rect(x: 5, y: 5, w: 330, h: 100, color: sky_blue)
	ctx.draw_rect_empty(5, 5, 330, 100, blue)
	ctx.draw_text(15, 15, 'Camera controls:', size: 10, color: black)
	ctx.draw_text(15, 30, '- Move keys: W, A, S, D, Space, Left-Ctrl', size: 10, color: black)
	ctx.draw_text(15, 45, '- Look around: arrow keys or mouse', size: 10, color: black)
	ctx.draw_text(15, 60, '- Camera mode keys: 1, 2, 3, 4', size: 10, color: black)
	ctx.draw_text(15, 75, '- Zoom keys: num-plus, num-minus or mouse scroll',
		size:  10
		color: black
	)
	ctx.draw_text(15, 90, '- Camera projection key: P', size: 10, color: black)

	ctx.draw_rect(x: 600, y: 5, w: 195, h: 100, color: sky_blue)
	ctx.draw_rect_empty(600, 5, 195, 100, blue)
	names := ['FREE', 'FIRST_PERSON', 'THIRD_PERSON', 'ORBITAL']
	ctx.draw_text(610, 15, 'Camera status:', size: 10, color: black)
	ctx.draw_text(610, 30, '- Mode: ${names[app.mode_key]}', size: 10, color: black)
	ctx.draw_text(610, 45, '- Projection: ${if app.cam.projection == .perspective {
		'PERSPECTIVE'
	} else {
		'ORTHOGRAPHIC'
	}}',
		size:  10
		color: black
	)
	ctx.draw_text(610, 60,
		'- Position: (${app.cam.position.x:.3f}, ${app.cam.position.y:.3f}, ${app.cam.position.z:.3f})',
		size:  10
		color: black
	)
	ctx.draw_text(610, 75,
		'- Target: (${app.cam.target.x:.3f}, ${app.cam.target.y:.3f}, ${app.cam.target.z:.3f})',
		size:  10
		color: black
	)
	ctx.draw_text(610, 90, '- Up: (${app.cam.up.x:.3f}, ${app.cam.up.y:.3f}, ${app.cam.up.z:.3f})',
		size:  10
		color: black
	)
	ctx.end()
}

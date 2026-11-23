module main

// raylib [core] example - 3d picking
// Click the cube to select it. Right click toggles first-person camera
// control. gg's screen_to_world_ray + ray_box replace raylib's collision API.

import gg
import sokol.sapp

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const gray = gg.Color{130, 130, 130, 255}
const maroon = gg.Color{128, 0, 0, 255}
const green = gg.Color{0, 158, 47, 255}

const cube_pos = gg.Vec3{0, 1, 0}
const cube_size = gg.Vec3{2, 2, 2}

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	cam         gg.Camera3D = gg.Camera3D{ position: gg.Vec3{10, 10, 10}, target: gg.Vec3{0, 0, 0} }
	selected    bool
	controls_on bool
	ray         gg.Ray
}

fn main() {
	mut app := &App{}
	app.cam.fovy = 45
	sapp.lock_mouse(false)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 3d picking'
		bg_color:     ray_white
		depth:        true
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.fps.show = true
	app.gg.fps.x = 10
	app.gg.fps.y = 10
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .mouse_down || e.key_repeat {
		return
	}
	if e.mouse_button == .right {
		app.controls_on = !app.controls_on
		sapp.lock_mouse(app.controls_on)
	} else if e.mouse_button == .left {
		if app.selected {
			app.selected = false
		} else {
			ctx := app.gg
			app.ray = gg.screen_to_world_ray(f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y),
				app.cam, f32(ctx.width), f32(ctx.height))
			app.selected = gg.ray_box(app.ray, cube_pos, cube_size).hit
		}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if app.controls_on {
		app.cam.update(.first_person, app.gg, dt)
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	if app.selected {
		ctx.draw_cube(cube_pos, cube_size, gg.red)
		ctx.draw_cube_wires(cube_pos, cube_size, maroon)
		ctx.draw_cube_wires(cube_pos, cube_size + gg.Vec3{0.2, 0.2, 0.2}, green)
	} else {
		ctx.draw_cube(cube_pos, cube_size, gray)
		ctx.draw_cube_wires(cube_pos, cube_size, dark_gray)
	}
	ctx.draw_line_3d(app.ray.origin,
		app.ray.origin + app.ray.direction.scale(100), maroon)
	ctx.draw_grid(10, 1.0, gg.gray)
	ctx.end_3d()

	ctx.draw_text(240, 10, 'Try clicking on the box with your mouse!',
		size:  20
		color: dark_gray
	)
	if app.selected {
		ctx.draw_text((screen_width - 180) / 2, int(f32(screen_height) * 0.1), 'BOX SELECTED',
			size:  30
			color: green
		)
	}
	ctx.draw_text(10, 430, 'Right click mouse to toggle camera controls',
		size:  10
		color: gray
	)
	ctx.end()
}

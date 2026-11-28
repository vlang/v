module main

// raylib [core] example - world screen
// Projects a 3D point into 2D screen coordinates to place a label over a cube.
// gg exposes Camera3D.world_to_screen for exactly this purpose.

import gg
import sokol.sapp

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{128, 0, 0, 255}
const lime = gg.Color{124, 252, 0, 255}
const gray = gg.Color{130, 130, 130, 255}
const black = gg.Color{0, 0, 0, 255}
const cube_pos = gg.Vec3{0, 0, 0}

struct App {
mut:
	gg  &gg.Context = unsafe { nil }
	cam gg.Camera3D = gg.Camera3D{ position: gg.Vec3{10, 10, 10}, target: gg.Vec3{0, 0, 0} }
}

fn main() {
	mut app := &App{}
	app.cam.fovy = 45
	sapp.lock_mouse(true)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - world screen'
		bg_color:     ray_white
		depth:        true
		frame_fn:     frame
		update_fn:    update
		user_data:    app
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.first_person, app.gg, dt)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	screen_pos := gg.world_to_screen(gg.Vec3{0, 2.5, 0}, app.cam, ctx.width, ctx.height)

	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.draw_cube(cube_pos, gg.Vec3{2, 2, 2}, gg.red)
	ctx.draw_cube_wires(cube_pos, gg.Vec3{2, 2, 2}, maroon)
	ctx.draw_grid(10, 1.0, gg.gray)
	ctx.end_3d()

	ctx.draw_text(int(screen_pos.x) - 70, int(screen_pos.y), 'Enemy: 100/100',
		size:  20
		color: black
	)
	ctx.draw_text(10, 10,
		'Cube position in screen space coordinates: [${int(screen_pos.x)}, ${int(screen_pos.y)}]',
		size:  20
		color: lime
	)
	ctx.draw_text(10, 40, 'Text 2d should be always on top of the cube',
		size:  20
		color: gray
	)
	ctx.end()
}

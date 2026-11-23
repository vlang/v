module main

// raylib [core] example - 3d camera mode
// A fixed perspective camera looking at a red cube and a grid.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 3d camera mode'
		bg_color:     ray_white
		depth:        true
		frame_fn:     frame
		user_data:    app
	)
	app.gg.fps.show = true
	app.gg.fps.x = 10
	app.gg.fps.y = 10
	app.gg.run()
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	mut cam := gg.new_camera3d(gg.Vec3{0, 10, 10}, gg.Vec3{0, 0, 0})
	cam.up = gg.Vec3{0, 1, 0}
	cam.fovy = 45

	ctx.begin()
	ctx.set_camera(cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.draw_cube(gg.Vec3{}, gg.Vec3{2, 2, 2}, gg.red)
	ctx.draw_cube_wires(gg.Vec3{}, gg.Vec3{2, 2, 2}, maroon)
	ctx.draw_grid(10, 1.0, gg.gray)
	ctx.end_3d()
	ctx.draw_text(10, 40, 'Welcome to the third dimension!',
		size:  20
		color: dark_gray
	)
	ctx.end()
}

module main

// raylib [core] example - 3d camera split screen
// Two first-person cameras view the same world side by side.
// PORT NOTE: gg renders each view directly into a viewport instead of into
// two RenderTexture2Ds; the visible result is the same split screen.

import gg

const screen_width = 800
const screen_height = 450
const half_width = screen_width / 2
const tree_count = 5
const tree_spacing = f32(4)

const black = gg.Color{0, 0, 0, 255}
const sky_blue = gg.Color{102, 191, 255, 255}
const translucent_white = gg.Color{245, 245, 245, 204}
const beige = gg.Color{245, 245, 220, 255}
const lime = gg.Color{124, 252, 0, 255}
const brown = gg.Color{127, 106, 79, 255}
const maroon = gg.Color{128, 0, 0, 255}
const dark_blue = gg.Color{0, 82, 172, 255}

struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	cam1 gg.Camera3D = gg.Camera3D{
		position: gg.Vec3{0, 1, -3}
		target:   gg.Vec3{0, 1, 0}
		fovy:     45
	}
	cam2 gg.Camera3D = gg.Camera3D{
		position: gg.Vec3{-3, 3, 0}
		target:   gg.Vec3{0, 3, 0}
		fovy:     45
	}
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 3d camera split screen'
		bg_color:     sky_blue
		depth:        true
		frame_fn:     frame
		update_fn:    update
		user_data:    app
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	step := 10 * dt
	if ctx.pressed_keys[int(gg.KeyCode.w)] {
		app.cam1.position.z += step
		app.cam1.target.z += step
	} else if ctx.pressed_keys[int(gg.KeyCode.s)] {
		app.cam1.position.z -= step
		app.cam1.target.z -= step
	}
	if ctx.pressed_keys[int(gg.KeyCode.up)] {
		app.cam2.position.x += step
		app.cam2.target.x += step
	} else if ctx.pressed_keys[int(gg.KeyCode.down)] {
		app.cam2.position.x -= step
		app.cam2.target.x -= step
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_viewport(0, 0, half_width, screen_height)
	draw_view(ctx, app.cam1, app, 'PLAYER1: W/S to move', maroon)
	ctx.set_viewport(half_width, 0, half_width, screen_height)
	draw_view(ctx, app.cam2, app, 'PLAYER2: UP/DOWN to move', dark_blue)
	ctx.set_viewport(0, 0, ctx.width, ctx.height)
	ctx.draw_rect(x: half_width - 2, y: 0, w: 4, h: screen_height, color: gg.light_gray)
	ctx.end()
}

fn draw_view(ctx &gg.Context, cam gg.Camera3D, app &App, label string, label_color gg.Color) {
	ctx.set_camera(cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.draw_plane(gg.Vec3{}, 50, 50, beige)
	limit := f32(tree_count) * tree_spacing
	mut x := -limit
	for x <= limit {
		mut z := -limit
		for z <= limit {
			ctx.draw_cube(gg.Vec3{x, 1.5, z}, gg.Vec3{1, 1, 1}, lime)
			ctx.draw_cube(gg.Vec3{x, 0.5, z}, gg.Vec3{0.25, 1, 0.25}, brown)
			z += tree_spacing
		}
		x += tree_spacing
	}
	ctx.draw_cube(app.cam1.position, gg.Vec3{1, 1, 1}, gg.red)
	ctx.draw_cube(app.cam2.position, gg.Vec3{1, 1, 1}, gg.blue)
	ctx.end_3d()
	ctx.draw_rect(x: 0, y: 0, w: half_width, h: 40, color: translucent_white)
	ctx.draw_text(10, 10, label, size: 20, color: label_color)
}

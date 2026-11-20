module main

// raylib [core] example - 2d camera split screen
// Two players, each with its own Camera2D, rendered side by side.
// PORT NOTE: raylib uses two RenderTexture2Ds and composites them. gg's
// set_viewport provides the same side-by-side result directly, so no
// offscreen passes are needed here.

import gg

const screen_width = 800
const screen_height = 440
const player_size = f32(40)
const half_width = screen_width / 2

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const translucent_white = gg.Color{245, 245, 245, 153}
const maroon = gg.Color{128, 0, 0, 255}
const dark_blue = gg.Color{0, 82, 172, 255}

struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	p1   gg.Vec2     = gg.Vec2{200, 200}
	p2   gg.Vec2     = gg.Vec2{250, 200}
	cam1 gg.Camera2D
	cam2 gg.Camera2D
}

fn main() {
	mut app := &App{}
	app.cam1 = gg.default_camera2d()
	app.cam2 = gg.default_camera2d()
	app.cam1.offset = gg.Vec2{200, 200}
	app.cam2.offset = gg.Vec2{200, 200}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 2d camera split screen'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg

	if ctx.pressed_keys[int(gg.KeyCode.s)] {
		app.p1.y += 3
	} else if ctx.pressed_keys[int(gg.KeyCode.w)] {
		app.p1.y -= 3
	}
	if ctx.pressed_keys[int(gg.KeyCode.d)] {
		app.p1.x += 3
	} else if ctx.pressed_keys[int(gg.KeyCode.a)] {
		app.p1.x -= 3
	}
	if ctx.pressed_keys[int(gg.KeyCode.up)] {
		app.p2.y -= 3
	} else if ctx.pressed_keys[int(gg.KeyCode.down)] {
		app.p2.y += 3
	}
	if ctx.pressed_keys[int(gg.KeyCode.right)] {
		app.p2.x += 3
	} else if ctx.pressed_keys[int(gg.KeyCode.left)] {
		app.p2.x -= 3
	}
	app.cam1.target = app.p1
	app.cam2.target = app.p2

	ctx.begin()
	ctx.set_viewport(0, 0, half_width, screen_height)
	draw_world(ctx, app, 1)
	ctx.set_viewport(half_width, 0, half_width, screen_height)
	draw_world(ctx, app, 2)
	ctx.set_viewport(0, 0, ctx.width, ctx.height)

	ctx.draw_rect(x: half_width - 2, y: 0, w: 4, h: screen_height, color: light_gray)
	ctx.end()
}

fn draw_world(ctx &gg.Context, app &App, view int) {
	if view == 1 {
		ctx.begin_2d(app.cam1)
	} else {
		ctx.begin_2d(app.cam2)
	}
	for i in 0 .. int(screen_width / player_size) + 1 {
		x := f32(i) * player_size
		ctx.draw_line(x, 0, x, screen_height, light_gray)
	}
	for i in 0 .. int(screen_height / player_size) + 1 {
		y := f32(i) * player_size
		ctx.draw_line(0, y, screen_width, y, light_gray)
	}
	for i in 0 .. int(screen_width / player_size) {
		for j in 0 .. int(screen_height / player_size) {
			ctx.draw_text(int(10 + player_size * i), int(15 + player_size * j), '[${i},${j}]',
				size:  10
				color: light_gray
			)
		}
	}
	ctx.draw_rect(x: app.p1.x, y: app.p1.y, w: player_size, h: player_size, color: gg.red)
	ctx.draw_rect(x: app.p2.x, y: app.p2.y, w: player_size, h: player_size, color: gg.blue)
	ctx.end_2d()

	ctx.draw_rect(x: 0, y: 0, w: half_width, h: 30, color: translucent_white)
	if view == 1 {
		ctx.draw_text(10, 10, 'PLAYER1: W/S/A/D to move', size: 10, color: maroon)
	} else {
		ctx.draw_text(10, 10, 'PLAYER2: UP/DOWN/LEFT/RIGHT to move',
			size:  10
			color: dark_blue
		)
	}
}

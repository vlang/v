module main

// raylib [core] example - input keys
// Move a ball with the arrow keys. Continuous key state is read directly
// from the gg context's pressed_keys array (the gg equivalent of IsKeyDown).

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	ball gg.Vec2     = gg.Vec2{400, 225}
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input keys'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg

	if ctx.pressed_keys[int(gg.KeyCode.right)] {
		app.ball.x += 2
	}
	if ctx.pressed_keys[int(gg.KeyCode.left)] {
		app.ball.x -= 2
	}
	if ctx.pressed_keys[int(gg.KeyCode.up)] {
		app.ball.y -= 2
	}
	if ctx.pressed_keys[int(gg.KeyCode.down)] {
		app.ball.y += 2
	}

	ctx.begin()
	ctx.draw_text(10, 10, 'move the ball with arrow keys', size: 20, color: dark_gray)
	ctx.draw_circle_filled(app.ball.x, app.ball.y, 50, maroon)
	ctx.end()
}

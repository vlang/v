module main

// raylib [shapes] example - bouncing ball, ported to gg.
import gg

const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const darkgreen = gg.Color{0, 117, 44, 255}
const maroon = gg.Color{190, 33, 55, 255}
const red_c = gg.Color{230, 41, 55, 255}

struct App {
mut:
	ctx         &gg.Context = unsafe { nil }
	ball_x      f32
	ball_y      f32
	speed_x     f32  = 5
	speed_y     f32  = 4
	radius      f32  = 20
	gravity     f32  = 0.2
	use_gravity bool = true
	paused      bool
	frames      int
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()

	ctx.draw_circle_filled(app.ball_x, app.ball_y, app.radius, maroon)
	h := ctx.height
	ctx.draw_text(10, h - 25, 'PRESS SPACE to PAUSE BALL MOVEMENT',
		size:  20
		color: lightgray
	)
	if app.use_gravity {
		ctx.draw_text(10, h - 50, 'GRAVITY: ON (Press G to disable)',
			size:  20
			color: darkgreen
		)
	} else {
		ctx.draw_text(10, h - 50, 'GRAVITY: OFF (Press G to enable)',
			size:  20
			color: red_c
		)
	}
	if app.paused && ((app.frames / 30) % 2) == 1 {
		ctx.draw_text(350, 200, 'PAUSED',
			size:  30
			color: gray_c
		)
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		match e.key_code {
			.g { app.use_gravity = !app.use_gravity }
			.space { app.paused = !app.paused }
			else {}
		}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if app.paused {
		app.frames++
		return
	}
	step := dt * 60
	app.ball_x += app.speed_x * step
	app.ball_y += app.speed_y * step
	if app.use_gravity {
		app.speed_y += app.gravity * step
	}
	if app.ball_x >= f32(app.ctx.width) - app.radius || app.ball_x <= app.radius {
		app.speed_x *= -1
	}
	if app.ball_y >= f32(app.ctx.height) - app.radius || app.ball_y <= app.radius {
		app.speed_y *= -0.95
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - bouncing ball'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}

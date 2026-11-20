module main

// raylib [core] example - custom frame control
// PORT NOTE: gg/sokol owns the frame loop and pacing, so manual
// SwapScreenBuffer()/WaitTime() control is not available. The example keeps
// the original's constant-speed motion using the dt supplied by gg and
// displays the same timing/position information.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const sky_blue = gg.Color{102, 191, 255, 255}
const maroon = gg.Color{128, 0, 0, 255}
const black = gg.Color{0, 0, 0, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const gray = gg.Color{130, 130, 130, 255}
const lime = gg.Color{124, 252, 0, 255}
const green = gg.Color{0, 158, 47, 255}

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	position   f32
	time_count f32
	pause      bool
	target_fps int = 60
	delta_time f32
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - custom frame control'
		bg_color:     ray_white
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
		.space { app.pause = !app.pause }
		.up { app.target_fps += 20 }
		.down { app.target_fps -= 20 }
		else {}
	}
	if app.target_fps < 0 {
		app.target_fps = 0
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.delta_time = dt
	if app.pause {
		return
	}
	app.position += 200 * dt
	if app.position >= screen_width {
		app.position = 0
	}
	app.time_count += dt
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	for i in 0 .. screen_width / 200 {
		ctx.draw_rect(x: f32(i * 200), y: 0, w: 1, h: screen_height, color: sky_blue)
	}
	ctx.draw_circle_filled(app.position, f32(screen_height) / 2 - 25, 50, gg.red)
	ctx.draw_text(int(app.position) - 40, screen_height / 2 - 100,
		'${int(app.time_count * 1000):03} ms',
		size:  20
		color: maroon
	)
	ctx.draw_text(int(app.position) - 50, screen_height / 2 + 40, 'PosX: ${int(app.position):03}',
		size:  20
		color: black
	)
	ctx.draw_text(10, 10,
		'Circle is moving at a constant 200 pixels/sec,\nindependently of the frame rate.',
		size:  20
		color: dark_gray
	)
	ctx.draw_text(10, screen_height - 60, 'PRESS SPACE to PAUSE MOVEMENT',
		size:  20
		color: gray
	)
	ctx.draw_text(10, screen_height - 30, 'PRESS UP | DOWN to CHANGE TARGET FPS',
		size:  20
		color: gray
	)
	ctx.draw_text(screen_width - 220, 10, 'TARGET FPS: ${app.target_fps}',
		size:  20
		color: lime
	)
	if app.delta_time != 0 {
		ctx.draw_text(screen_width - 220, 40, 'CURRENT FPS: ${int(1 / app.delta_time)}',
			size:  20
			color: green
		)
	}
	ctx.end()
}

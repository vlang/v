module main

// raylib [core] example - custom logging
// PORT NOTE: gg does not expose raylib's SetTraceLogCallback. This example
// uses a small timestamped logger for the app's own messages and configures
// gg's fail_fn to route backend messages through it.

import gg
import time

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn log_msg(level string, msg string) {
	stamp := time.now().format('YYYY-MM-DD HH:mm:ss')
	println('[${stamp}] [${level}]: ${msg}')
}

fn on_fail(msg string, data voidptr) {
	log_msg('ERROR', msg)
}

fn main() {
	mut app := &App{}
	log_msg('INFO', 'starting custom logging example')
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - custom logging'
		bg_color:     ray_white
		frame_fn:     frame
		fail_fn:      on_fail
		user_data:    app
	)
	log_msg('INFO', 'window created')
	app.gg.run()
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(60, 200, 'Check out the console output to see the custom logger in action!',
		size:  20
		color: light_gray
	)
	ctx.end()
}

module main

// raylib [core] example - screen recording
// PORT NOTE: gg has no screen-capture/GIF encoder API. This example keeps
// the animated sine wave and moving circle and shows a recording indicator
// toggled with CTRL+R; saving frames to a GIF is not possible.

import gg
import math

const screen_width = 800
const screen_height = 450
const max_points = 256
const gif_frame_rate = 5

const ray_white = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{128, 0, 0, 255}
const red = gg.Color{255, 0, 0, 255}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	recording bool
	frame_cnt int
	circle_x  f32
	time      f32
	ctrl_down bool
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - screen recording'
		bg_color:     ray_white
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
	if e.typ == .key_down {
		if e.key_code == .left_control {
			app.ctrl_down = true
		}
		if e.key_code == .r && !e.key_repeat && app.ctrl_down {
			app.recording = !app.recording
			app.frame_cnt = 0
			println(if app.recording {
				'Start animated GIF recording'
			} else {
				'Finish animated GIF recording'
			})
		}
	}
	if e.typ == .key_up && e.key_code == .left_control {
		app.ctrl_down = false
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.time += dt
	app.circle_x += f32(screen_width) / 180
	app.circle_x = if app.circle_x > screen_width { 0 } else { app.circle_x }
	if app.recording {
		app.frame_cnt++
		if app.frame_cnt > gif_frame_rate {
			app.frame_cnt = 0
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	freq := f32(2 * math.pi / 1.5) / 60
	for i in 0 .. max_points - 1 {
		x1 := f32(i) * f32(screen_width) / 180
		x2 := f32(i + 1) * f32(screen_width) / 180
		y1 := f32(screen_height) / 2 + 150 * math.sinf(freq * f32(i))
		y2 := f32(screen_height) / 2 + 150 * math.sinf(freq * f32(i + 1))
		ctx.draw_line(x1, y1, x2, y2, maroon)
		if i % 8 == 0 {
			ctx.draw_circle_filled(x1, y1, 3, maroon)
		}
	}
	cy := f32(screen_height) / 2 + 150 * math.sinf(freq * 60 * app.time)
	ctx.draw_circle_filled(app.circle_x, cy, 30, red)
	if app.recording {
		ctx.draw_circle_filled(30, f32(screen_height) - 20, 10, maroon)
		ctx.draw_text(50, screen_height - 25, 'GIF RECORDING (simulated)',
			size:  10
			color: red
		)
	}
	ctx.end()
}

module main

// raylib [core] example - random values
// A new random value between -8 and 5 is generated every two seconds.

import gg
import rand

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{128, 0, 0, 255}
const light_gray = gg.Color{200, 200, 200, 255}

struct App {
mut:
	gg           &gg.Context = unsafe { nil }
	value        int
	frames_count u32
}

fn main() {
	mut app := &App{}
	app.value = rand.int_in_range(-8, 5) or { 0 }
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - random values'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	app.frames_count++
	if (app.frames_count / 120) % 2 == 1 {
		app.value = rand.int_in_range(-8, 5) or { 0 }
		app.frames_count = 0
	}
	ctx.begin()
	ctx.draw_text(130, 100, 'Every 2 seconds a new random value is generated:',
		size:  20
		color: maroon
	)
	ctx.draw_text(360, 180, app.value.str(), size: 80, color: light_gray)
	ctx.end()
}

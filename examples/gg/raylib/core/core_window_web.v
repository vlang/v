module main

// raylib [core] example - window web
// In raylib this example shows the Emscripten main-loop structure. gg already
// uses a callback loop on every platform, so the same frame callback works
// for desktop and web without any platform-specific code.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const sky_blue = gg.Color{102, 191, 255, 255}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - window web'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(220, 200, 'Welcome to raylib web structure!',
		size:  20
		color: sky_blue
	)
	ctx.end()
}

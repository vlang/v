module main

// raylib [core] example - input mouse wheel
// Move a box up and down with the scroll wheel.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	gg           &gg.Context = unsafe { nil }
	box_position f32         = 185
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input mouse wheel'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	app.box_position -= f32(ctx.scroll_y) * 4
	ctx.begin()
	ctx.draw_rect(x: f32(screen_width / 2 - 40), y: app.box_position, w: 80, h: 80, color: maroon)
	ctx.draw_text(10, 10, 'Use mouse wheel to move the cube up and down!',
		size:  20
		color: gray
	)
	ctx.draw_text(10, 40, 'Box position Y: ${int(app.box_position):03}',
		size:  20
		color: light_gray
	)
	ctx.end()
}

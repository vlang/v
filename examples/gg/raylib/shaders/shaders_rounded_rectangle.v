// Port of raylib [shaders] example - rounded rectangle.
// PORT NOTE: shader rounded_rectangle.fs with per-corner radius/shadow/border uniforms requires
// gg user-shader support (not yet available); gg's built-in draw_rounded_rect_filled plus a
// shadow and border rectangle approximate the effect.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - rounded rectangle'
		bg_color:      gg.white
		user_data:     app
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	x, y, w, h := f32(50), f32(70), f32(110), f32(60)
	ctx.draw_rect_empty(x - 20, y - 20, w + 40, h + 40, gg.dark_gray)
	ctx.draw_text(int(x - 20), int(y - 35), 'Rounded rectangle', size: 10, color: gg.dark_gray)

	// Shadow.
	ctx.draw_rounded_rect_filled(x + 2, y + 7, w, h, 12, gg.Color{0, 0, 139, 90})
	// Border.
	ctx.draw_rounded_rect_filled(x - 5, y - 5, w + 10, h + 10, 16, gg.light_blue)
	// Fill.
	ctx.draw_rounded_rect_filled(x, y, w, h, 12, gg.blue)

	ctx.draw_text(10, 200, 'Corner radius / shadow / border approximated with gg primitives',
		size:  14
		color: gg.dark_gray
	)
	ctx.end()
}

module main

// raylib [shapes] example - logo raylib, ported to gg.
import gg

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const gray_c = gg.Color{130, 130, 130, 255}

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_rect_filled(ctx.width / 2 - 128, ctx.height / 2 - 128, 256, 256, black_c)
	ctx.draw_rect_filled(ctx.width / 2 - 112, ctx.height / 2 - 112, 224, 224, raywhite)
	ctx.draw_text(ctx.width / 2 - 44, ctx.height / 2 + 48, 'raylib',
		size:  50
		color: black_c
	)
	ctx.draw_text(350, 370, 'this is NOT a texture!', size: 10, color: gray_c)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - logo raylib'
		user_data:    app
		frame_fn:     on_frame
		bg_color:     raywhite
	)
	app.ctx.run()
}

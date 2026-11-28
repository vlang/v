module main

// raylib [text] example - format text, ported to gg.
import gg

const (
	raywhite = gg.Color{245, 245, 245, 255}
	red_c    = gg.Color{230, 41, 55, 255}
	green_c  = gg.Color{0, 158, 47, 255}
	blue_c   = gg.Color{0, 121, 241, 255}
	black_c  = gg.Color{0, 0, 0, 255}
)

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(200, 80, 'Score: ${100020:08}', size: 20, color: red_c)
	ctx.draw_text(200, 120, 'HiScore: ${200450:08}', size: 20, color: green_c)
	ctx.draw_text(200, 160, 'Lives: ${5:02}', size: 40, color: blue_c)
	ctx.draw_text(200, 220, 'Elapsed Time: ${1000.0 / 60:.2f} ms',
		size:  20
		color: black_c
	)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - format text'
		user_data:    app
		frame_fn:     on_frame
		bg_color:     raywhite
	)
	app.ctx.run()
}

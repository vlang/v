module main

// raylib [text] example - font spritefont, ported to gg.
// PORT NOTE: raylib XNA-style sprite-sheet fonts (plain PNG, no .fnt descriptor)
// have no gg equivalent. gg's gg.load_bmfont needs an AngelCode .fnt file, so the
// messages are rendered with the configured TTF font instead, preserving layout.
import gg

const (
	raywhite = gg.Color{245, 245, 245, 255}
	white_c  = gg.Color{255, 255, 255, 255}
	msg1 = 'THIS IS A custom SPRITE FONT...'
	msg2 = '...and this is ANOTHER CUSTOM font...'
	msg3 = '...and a THIRD one! GREAT! :D'
)

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(400 - ctx.text_width(msg1) / 2, 145, msg1,
		size:  40
		color: white_c
		align: .center
	)
	ctx.draw_text(400 - ctx.text_width(msg2) / 2, 215, msg2,
		size:  32
		color: white_c
	)
	ctx.draw_text(400 - ctx.text_width(msg3) / 2, 275, msg3,
		size:  36
		color: white_c
	)
	ctx.draw_text(20, 20, 'custom sprite fonts (TTF fallback in gg)',
		size:  14
		color: gg.Color{80, 80, 80, 255}
	)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - font spritefont'
		user_data:    app
		frame_fn:     on_frame
		bg_color:     raywhite
		font_path:    'resources/KAISG.ttf'
	)
	app.ctx.run()
}

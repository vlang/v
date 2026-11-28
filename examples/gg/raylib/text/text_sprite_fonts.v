module main

// raylib [text] example - sprite fonts, ported to gg.
// PORT NOTE: raylib's bundled PNG sprite fonts have no gg equivalent without
// AngelCode .fnt files. The eight messages are shown with distinct system-font
// sizes/tints to preserve the original layout and color rhythm.
import gg

const (
	raywhite = gg.Color{245, 245, 245, 255}
	darkgray = gg.Color{80, 80, 80, 255}
	maroon   = gg.Color{190, 33, 55, 255}
	orange_c = gg.Color{255, 161, 0, 255}
	darkgreen= gg.Color{0, 117, 44, 255}
	darkblue = gg.Color{0, 82, 172, 255}
	purple   = gg.Color{211, 22, 197, 255}
	lime     = gg.Color{1, 222, 38, 255}
	gold     = gg.Color{255, 203, 0, 255}
	red_c    = gg.Color{230, 41, 55, 255}

	messages = [
		'ALAGARD FONT designed by Hewett Tsoi',
		'PIXELPLAY FONT designed by Aleksander Shevchuk',
		'MECHA FONT designed by Captain Falcon',
		'SETBACK FONT designed by Brian Kent (AEnigma)',
		'ROMULUS FONT designed by Hewett Tsoi',
		'PIXANTIQUA FONT designed by Gerhard Grossmann',
		'ALPHA_BETA FONT designed by Brian Kent (AEnigma)',
		'JUPITER_CRASH FONT designed by Brian Kent (AEnigma)',
	]!
	sizes = [20, 18, 22, 20, 20, 18, 20, 16]!
	colors = [maroon, orange_c, darkgreen, darkblue, purple, lime, gold, red_c]!
)

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(220, 20, 'free sprite fonts included with raylib',
		size:  20
		color: darkgray
	)
	ctx.draw_line(220, 50, 600, 50, darkgray)
	for i in 0 .. 8 {
		y := 60 + sizes[i] + 45 * i + (if i == 3 { 8 } else if i == 4 { 2 } else if i == 7 { -8 } else { 0 })
		ctx.draw_text(60, y, messages[i], size: sizes[i], color: colors[i])
	}
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - sprite fonts'
		user_data:    app
		frame_fn:     on_frame
		bg_color:     raywhite
	)
	app.ctx.run()
}

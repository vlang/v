// Port of raylib [shaders] example - palette switch.
// PORT NOTE: shader palette_switch.fs with an ivec3 palette uniform requires gg user-shader
// support (not yet available); the eight palette entries are selected in CPU code and drawn
// as horizontal bands.
module main

import gg

const win_width = 800
const win_height = 450
const colors_per_palette = 8

const palettes = [
	[gg.black, gg.red, gg.green, gg.blue, gg.cyan, gg.magenta, gg.yellow, gg.white],
	[gg.rgb(4, 12, 6), gg.rgb(17, 35, 24), gg.rgb(30, 58, 41), gg.rgb(48, 93, 66), gg.rgb(77, 128, 97),
		gg.rgb(137, 162, 87), gg.rgb(190, 220, 127), gg.rgb(238, 255, 204)],
	[gg.rgb(21, 25, 26), gg.rgb(138, 76, 88), gg.rgb(217, 98, 117), gg.rgb(230, 184, 193),
		gg.rgb(69, 107, 115), gg.rgb(75, 151, 166), gg.rgb(165, 189, 194), gg.rgb(255, 245, 247)],
]

const palette_names = ['3-BIT RGB', 'AMMO-8 (GameBoy-like)', 'RKBV (2-strip film)']

struct App {
mut:
	gg      &gg.Context = unsafe { nil }
	current int
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - palette switch'
		bg_color:      gg.white
		user_data:     app
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	if e.key_code == .right {
		app.current++
	} else if e.key_code == .left {
		app.current--
	}
	if app.current >= palettes.len {
		app.current = 0
	} else if app.current < 0 {
		app.current = palettes.len - 1
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	line_height := win_height / colors_per_palette
	for i in 0 .. colors_per_palette {
		ctx.draw_rect_filled(0, f32(i * line_height), f32(win_width), f32(line_height),
			palettes[app.current][i])
	}
	ctx.draw_text(10, 10, '< >', size: 30, color: gg.dark_blue)
	ctx.draw_text(60, 15, 'CURRENT PALETTE:', size: 18, color: gg.white)
	ctx.draw_text(300, 15, palette_names[app.current], size: 18, color: gg.red)
	ctx.end()
}

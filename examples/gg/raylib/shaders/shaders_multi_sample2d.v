// Port of raylib [shaders] example - multi sample2d (color mix).
// PORT NOTE: shader color_mix.fs with two sampler2D textures requires gg user-shader support
// (not yet available); the red/blue mix is drawn as two clipped rectangles with ImageEffect.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg      &gg.Context = unsafe { nil }
	divider f32         = 0.5
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - multi sample2d'
		bg_color:      gg.white
		user_data:     app
		update_fn:     update
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	step := 0.6 * dt * 60
	if app.gg.pressed_keys[gg.KeyCode.right] {
		app.divider += step / 100
	} else if app.gg.pressed_keys[gg.KeyCode.left] {
		app.divider -= step / 100
	}
	if app.divider < 0 {
		app.divider = 0
	} else if app.divider > 1 {
		app.divider = 1
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	split := f32(win_width) * app.divider
	ctx.draw_rect_filled(0, 0, split, f32(win_height), gg.red)
	ctx.draw_rect_filled(split, 0, f32(win_width) - split, f32(win_height), gg.blue)
	ctx.draw_rect_empty(0, 0, f32(win_width), f32(win_height), gg.gray)
	ctx.draw_text(80, win_height - 40, 'Use LEFT/RIGHT to move the texture mixing divider!',
		size:  18
		color: gg.white
	)
	ctx.end()
}

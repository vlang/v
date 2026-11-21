// Port of raylib [shaders] example - texture rendering (animated cube panning background).
// PORT NOTE: shader cubes_panning.fs requires gg user-shader support (not yet available);
// the animated background is painted on the CPU into a streaming Image.
module main

import gg
import math

const win_width = 800
const win_height = 450
const img_w = 400
const img_h = 225

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	image_id int         = -1
	pixels   []u8
	time     f32
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - texture rendering'
		bg_color:      gg.white
		user_data:     app
		init_fn:       init
		update_fn:     update
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.image_id = app.gg.new_streaming_image(img_w, img_h, 4, gg.StreamingImageConfig{})
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.time += dt
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	for y in 0 .. img_h {
		for x in 0 .. img_w {
			fx, fy := f32(x), f32(y)
			pan := app.time * 24
			cx := int((fx + pan) / 48) % 2
			cy := int((fy + pan) / 48) % 2
			base := if cx == cy { f32(90) } else { f32(40) }
			stripe := if int(fx + pan) % 48 < 3 || int(fy + pan) % 48 < 3 { 50 } else { 0 }
			at := (y * img_w + x) * 4
			app.pixels[at] = u8(base + stripe + 40 * math.sinf(app.time + fx * 0.01))
			app.pixels[at + 1] = u8(base + stripe)
			app.pixels[at + 2] = u8(base + stripe + 40)
			app.pixels[at + 3] = 255
		}
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.draw_image_by_id(0, 0, f32(win_width), f32(win_height), app.image_id)
	ctx.draw_text(10, 10, 'BACKGROUND is PAINTED and ANIMATED on CPU!',
		size:  18
		color: gg.dark_red
	)
	ctx.end()
}

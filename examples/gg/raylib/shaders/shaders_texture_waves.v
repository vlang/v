// Port of raylib [shaders] example - texture waves.
// PORT NOTE: shader wave.fs with freq/amp/speed uniforms requires gg user-shader support (not
// yet available); a CPU sine warp of a streaming Image approximates the two wavy textures.
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
	seconds  f32
	freq_x   f32 = 25
	freq_y   f32 = 25
	amp_x    f32 = 5
	amp_y    f32 = 5
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - texture waves'
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
	app.seconds += dt
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	for y in 0 .. img_h {
		for x in 0 .. img_w {
			fx := f32(x)
			fy := f32(y)
			wx := int(fx + math.sinf(fy / app.freq_y * 6.28 + app.seconds * 8) * app.amp_x * 4)
			wy := int(fy + math.sinf(fx / app.freq_x * 6.28 + app.seconds * 8) * app.amp_y * 4)
			wx = (wx + img_w * 2) % (img_w * 2)
			wy = (wy + img_h * 2) % (img_h * 2)
			sx := if wx >= img_w { wx - img_w } else { wx }
			sy := if wy >= img_h { wy - img_h } else { wy }
			at := (y * img_w + x) * 4
			app.pixels[at] = u8(sx % 255)
			app.pixels[at + 1] = u8(sy % 255)
			app.pixels[at + 2] = u8((sx + sy) % 255)
			app.pixels[at + 3] = 255
		}
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.draw_image_by_id(0, 0, f32(win_width) / 2, f32(win_height), app.image_id)
	ctx.draw_image_by_id(f32(win_width) / 2, 0, f32(win_width) / 2, f32(win_height), app.image_id)
	ctx.end()
}

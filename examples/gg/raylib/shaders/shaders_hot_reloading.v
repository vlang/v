// Port of raylib [shaders] example - hot reloading.
// PORT NOTE: shader hot reloading (watching reload.fs on disk and recompiling) requires gg
// user-shader support (not yet available); pressing A toggles between two CPU-generated
// procedural patterns as a stand-in for a live shader swap.
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
	auto     bool
	mouse_x  f32
	mouse_y  f32
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - hot reloading'
		bg_color:      gg.white
		user_data:     app
		init_fn:       init
		update_fn:     update
		event_fn:      event
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

fn event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .a {
		app.auto = !app.auto
	}
	app.mouse_x = e.mouse_x
	app.mouse_y = e.mouse_y
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	for y in 0 .. img_h {
		for x in 0 .. img_w {
			fx, fy := f32(x), f32(y)
			mut r, mut g, mut b := f32(0), f32(0), f32(0)
			if app.auto {
				v := math.sinf(fx * 0.03 + app.time) * math.cosf(fy * 0.03 + app.time)
				r = (v + 1) / 2 * 255
				g = (math.sinf(fy * 0.05 - app.time) + 1) / 2 * 255
				b = 120
			} else {
				d := math.sqrt((fx - app.mouse_x / 2) * (fx - app.mouse_x / 2) +
					(fy - app.mouse_y / 2) * (fy - app.mouse_y / 2))
				v := math.sinf(f32(d * 0.1 - app.time * 3))
				r = (v + 1) / 2 * 255
				g = 80
				b = 200
			}
			at := (y * img_w + x) * 4
			app.pixels[at] = u8(r)
			app.pixels[at + 1] = u8(g)
			app.pixels[at + 2] = u8(b)
			app.pixels[at + 3] = 255
		}
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.draw_image_by_id(0, 0, f32(win_width), f32(win_height), app.image_id)
	mode := if app.auto { 'AUTO' } else { 'MANUAL' }
	ctx.draw_text(10, 10, 'PRESS [A] to TOGGLE PATTERN: ${mode}',
		size:  14
		color: if app.auto { gg.red } else { gg.black }
	)
	ctx.end()
}

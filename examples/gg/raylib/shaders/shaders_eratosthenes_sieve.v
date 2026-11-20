// Port of raylib [shaders] example - eratosthenes sieve.
// PORT NOTE: shader eratosthenes.fs requires gg user-shader support (not yet available);
// the prime sieve is computed on the CPU into a streaming Image and drawn fullscreen.
module main

import gg
import math

const win_width = 800
const win_height = 450
const img_w = 400
const img_h = 225
const limit = 2048

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	image_id int         = -1
	pixels   []u8
	primes   []bool
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.primes = []bool{len: limit, init: true}
	app.primes[0] = false
	app.primes[1] = false
	for p := 2; p * p < limit; p++ {
		if app.primes[p] {
			for m := p * p; m < limit; m += p {
				app.primes[m] = false
			}
		}
	}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - eratosthenes sieve'
		bg_color:      gg.black
		user_data:     app
		init_fn:       init
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.image_id = app.gg.new_streaming_image(img_w, img_h, 4, gg.StreamingImageConfig{})
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	// Plot the sieve as a spiral of dots (CPU equivalent of the fragment shader).
	for i in 0 .. img_w * img_h {
		x := i % img_w
		y := i / img_w
		dx, dy := f32(x - img_w / 2), f32(y - img_h / 2)
		r := math.sqrt(dx * dx + dy * dy)
		angle := math.atan2(dy, dx)
		idx := int(r * 12 + angle * 4) % limit
		if idx < 0 {
			idx += limit
		}
		on := app.primes[idx] && r < f32(img_h) / 2 - 2
		at := i * 4
		if on {
			app.pixels[at] = 255
			app.pixels[at + 1] = 220
			app.pixels[at + 2] = 90
		} else {
			app.pixels[at] = 10
			app.pixels[at + 1] = 10
			app.pixels[at + 2] = 20
		}
		app.pixels[at + 3] = 255
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.draw_image_by_id(0, 0, f32(win_width), f32(win_height), app.image_id)
	ctx.draw_text(10, 10, 'Sieve of Eratosthenes (CPU image approximation)',
		size:  14
		color: gg.white
	)
	ctx.end()
}

// Port of raylib [shaders] example - raymarching rendering.
// PORT NOTE: shader raymarching.fs requires gg user-shader support (not yet available);
// approximated with a CPU raymarch of a sphere SDF written into a streaming Image.
module main

import gg
import math

const win_width = 800
const win_height = 450
const img_w = 320
const img_h = 180

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
		window_title:  'raylib [shaders] example - raymarching rendering'
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
			// Sphere SDF raymarch from a fixed eye.
			ro := gg.Vec3{0, 0, -3}
			rd := gg.Vec3{(f32(x) - img_w / 2) / f32(img_h), (f32(y) - img_h / 2) / f32(img_h), 1}.normalized()
			mut t := f32(0)
			mut hit := false
			for _ in 0 .. 24 {
				p := ro + rd.scale(t)
				p.z -= math.sinf(app.time) * 0.5
				d := f32(math.sqrt(f64(p.x * p.x + p.y * p.y + p.z * p.z))) - 1.0
				if d < 0.01 {
					hit = true
					break
				}
				t += d
				if t > 12 {
					break
				}
			}
			at := (y * img_w + x) * 4
			if hit {
				shade := u8(120 + 130 / (1 + t * 0.3))
				app.pixels[at] = shade
				app.pixels[at + 1] = u8(shade / 2)
				app.pixels[at + 2] = 60
			} else {
				app.pixels[at] = 20
				app.pixels[at + 1] = 25
				app.pixels[at + 2] = 40
			}
			app.pixels[at + 3] = 255
		}
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.draw_image_by_id(0, 0, f32(win_width), f32(win_height), app.image_id)
	ctx.draw_text(10, 10, 'CPU sphere SDF raymarch (raymarching.fs not available)',
		size:  14
		color: gg.white
	)
	ctx.end()
}

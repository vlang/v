// Port of raylib [shaders] example - postprocessing.
// PORT NOTE: the 12 postprocess fragment shaders require gg user-shader support (not yet
// available); a few effects are approximated with per-pixel CPU filters on a streaming Image.
module main

import gg
import math

const win_width = 800
const win_height = 450
const img_w = 400
const img_h = 225
const max_fx = 5

const fx_names = ['GRAYSCALE', 'POSTERIZATION', 'PIXELIZER', 'SCANLINES', 'INVERT']

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	image_id int         = -1
	pixels   []u8
	current  int
	cam      gg.Camera3D
	cube     gg.Mesh3D
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.cam = gg.Camera3D{ position: gg.Vec3{2, 3, 2}, target: gg.Vec3{0, 1, 0}, fovy: 45 }
	app.cam.orbit(30, 15, 4)
	app.cube = gg.gen_mesh_cube(gg.Vec3{2, 2, 2})
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - postprocessing'
		bg_color:      gg.white
		depth:         true
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
	app.cam.update(.orbital, app.gg, dt)
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
	if app.current >= max_fx {
		app.current = 0
	} else if app.current < 0 {
		app.current = max_fx - 1
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	for y in 0 .. img_h {
		for x in 0 .. img_w {
			// Source "scene": a colorful gradient with a bright disc.
			mut r := f32(x) / f32(img_w) * 255
			mut g := f32(y) / f32(img_h) * 255
			mut b := f32(128 + 100 * math.sinf(f32(x) * 0.03))
			dx := f32(x - img_w / 2)
			dy := f32(y - img_h / 2)
			if dx * dx + dy * dy < 3600 {
				r, g, b = 230, 210, 120
			}
			match app.current {
				0 {
					lum := r * 0.299 + g * 0.587 + b * 0.114
					r, g, b = lum, lum, lum
				}
				1 {
					r = f32(int(r / 64) * 64)
					g = f32(int(g / 64) * 64)
					b = f32(int(b / 64) * 64)
				}
				2 {
					px, py := x / 8 * 8, y / 8 * 8
					r, g, b = f32(px) / f32(img_w) * 255, f32(py) / f32(img_h) * 255, 100
				}
				3 {
					if y % 4 == 0 {
						r, g, b = r * 0.4, g * 0.4, b * 0.4
					}
				}
				4 {
					r, g, b = 255 - r, 255 - g, 255 - b
				}
				else {}
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
	ctx.draw_rect_filled(0, 0, 580, 30, gg.Color{211, 211, 211, 180})
	ctx.draw_text(10, 8, 'CURRENT EFFECT: ${fx_names[app.current]}  [< >]',
		size:  14
		color: gg.black
	)
	ctx.end()
}

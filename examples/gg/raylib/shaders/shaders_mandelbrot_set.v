// Port of raylib [shaders] example - mandelbrot set.
// PORT NOTE: shader mandelbrot_set.fs requires gg user-shader support (not yet available);
// the set is computed on the CPU into a streaming Image with mouse zoom/pan controls.
module main

import gg

const win_width = 800
const win_height = 450
const img_w = 400
const img_h = 225

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	image_id    int         = -1
	pixels      []u8
	zoom        f32 = 0.6
	ox          f32 = -0.5
	oy          f32
	max_iter    int  = 180
	controls    bool = true
	mouse_left  bool
	mouse_right bool
	mx          f32
	my          f32
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - mandelbrot set'
		bg_color:      gg.black
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

fn event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	app.mx = e.mouse_x
	app.my = e.mouse_y
	match e.typ {
		.mouse_down {
			if e.mouse_button == .left {
				app.mouse_left = true
			} else if e.mouse_button == .right {
				app.mouse_right = true
			}
		}
		.mouse_up {
			if e.mouse_button == .left {
				app.mouse_left = false
			} else if e.mouse_button == .right {
				app.mouse_right = false
			}
		}
		.key_down {
			match e.key_code {
				.r {
					app.zoom = 0.6
					app.ox = -0.5
					app.oy = 0
				}
				.f1 { app.controls = !app.controls }
				.up { app.max_iter += 20 }
				.down { app.max_iter -= 20 }
				._1 {
					app.ox = -1.76826775
					app.oy = -0.00422996283
					app.zoom = 28435.9238
				}
				._2 {
					app.ox = 0.322004497
					app.oy = -0.0357099883
					app.zoom = 56499.7266
				}
				._3 {
					app.ox = -0.748880744
					app.oy = -0.0562955774
					app.zoom = 9237.59082
				}
				._4 {
					app.ox = -1.78385007
					app.oy = -0.0156200649
					app.zoom = 14599.5283
				}
				._5 {
					app.ox = -0.0985441282
					app.oy = -0.924688697
					app.zoom = 26259.8535
				}
				._6 {
					app.ox = 0.317785531
					app.oy = -0.0322612226
					app.zoom = 29297.9258
				}
				else {}
			}
			if app.max_iter < 20 {
				app.max_iter = 20
			}
		}
		else {}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if app.mouse_left {
		app.zoom *= 1.01
	} else if app.mouse_right {
		app.zoom /= 1.01
	}
	if app.mouse_left || app.mouse_right {
		app.ox += dt * (app.mx / f32(win_width) - 0.5) * 2 / app.zoom
		app.oy += dt * (app.my / f32(win_height) - 0.5) * 2 / app.zoom
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	for y in 0 .. img_h {
		for x in 0 .. img_w {
			cx := (f32(x) / f32(img_w) - 0.5) * 3 / app.zoom + app.ox
			cy := (f32(y) / f32(img_h) - 0.5) * 3 / app.zoom + app.oy
			mut zx := f32(0)
			mut zy := f32(0)
			mut i := 0
			for i < app.max_iter && zx * zx + zy * zy < 4 {
				xt := zx * zx - zy * zy + cx
				zy = 2 * zx * zy + cy
				zx = xt
				i++
			}
			at := (y * img_w + x) * 4
			if i == app.max_iter {
				app.pixels[at] = 0
				app.pixels[at + 1] = 0
				app.pixels[at + 2] = 0
			} else {
				v := u8(f32(i) * 255.0 / f32(app.max_iter))
				app.pixels[at] = u8(255 - v)
				app.pixels[at + 1] = v
				app.pixels[at + 2] = u8(90 + v / 2)
			}
			app.pixels[at + 3] = 255
		}
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.draw_image_by_id(0, 0, f32(win_width), f32(win_height), app.image_id)
	if app.controls {
		ctx.draw_text(10, 15, 'Mouse zoom/move, [1-6] points, [R] reset, UP/DOWN iterations',
			size:  10
			color: gg.white
		)
	}
	ctx.end()
}

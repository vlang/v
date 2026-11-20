// Port of raylib [shaders] example - julia set.
// PORT NOTE: shader julia_set.fs requires gg user-shader support (not yet available);
// the Julia set is computed on the CPU into a streaming Image with mouse zoom/pan controls.
module main

import gg

const win_width = 800
const win_height = 450
const img_w = 400
const img_h = 225
const max_iter = 96

struct App {
mut:
	gg            &gg.Context = unsafe { nil }
	image_id      int         = -1
	pixels        []u8
	cx            f32 = -0.348827
	cy            f32 = 0.607167
	zoom          f32 = 0.75
	ox            f32
	oy            f32
	show_controls bool = true
	mouse_left    bool
	mouse_right   bool
	mouse_x       f32
	mouse_y       f32
	speed         int
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - julia set'
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
	app.mouse_x = e.mouse_x
	app.mouse_y = e.mouse_y
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
				._1 {
					app.cx = -0.348827
					app.cy = 0.607167
				}
				._2 {
					app.cx = -0.786268
					app.cy = 0.169728
				}
				._3 {
					app.cx = -0.8
					app.cy = 0.156
				}
				._4 {
					app.cx = 0.285
					app.cy = 0
				}
				._5 {
					app.cx = -0.835
					app.cy = -0.2321
				}
				._6 {
					app.cx = -0.70176
					app.cy = -0.3842
				}
				.r {
					app.zoom = 0.75
					app.ox = 0
					app.oy = 0
				}
				.f1 { app.show_controls = !app.show_controls }
				.space { app.speed = 0 }
				.right { app.speed++ }
				.left { app.speed-- }
				else {}
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
		app.ox += dt * (app.mouse_x / f32(win_width) - 0.5) * 2 / app.zoom
		app.oy += dt * (app.mouse_y / f32(win_height) - 0.5) * 2 / app.zoom
	}
	dc := dt * f32(app.speed) * 0.0005
	app.cx += dc
	app.cy += dc
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	for y in 0 .. img_h {
		for x in 0 .. img_w {
			zx := (f32(x) / f32(img_w) - 0.5) * 3 / app.zoom + app.ox
			zy := (f32(y) / f32(img_h) - 0.5) * 3 / app.zoom + app.oy
			mut i := 0
			for i < max_iter && zx * zx + zy * zy < 4 {
				xt := zx * zx - zy * zy + app.cx
				zy = 2 * zx * zy + app.cy
				zx = xt
				i++
			}
			at := (y * img_w + x) * 4
			if i == max_iter {
				app.pixels[at] = 0
				app.pixels[at + 1] = 0
				app.pixels[at + 2] = 0
			} else {
				v := u8(i * 255 / max_iter)
				app.pixels[at] = v
				app.pixels[at + 1] = u8(v / 2)
				app.pixels[at + 2] = u8(255 - v)
			}
			app.pixels[at + 3] = 255
		}
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.draw_image_by_id(0, 0, f32(win_width), f32(win_height), app.image_id)
	if app.show_controls {
		ctx.draw_text(10, 15, 'Mouse buttons zoom/move, [1-6] presets, [F1] controls',
			size:  10
			color: gg.white
		)
	}
	ctx.end()
}

// Port of raylib [shaders] example - color correction.
// PORT NOTE: shader color_correction.fs requires gg user-shader support (not yet available);
// contrast/saturation/brightness are applied per-pixel on the CPU to a procedural image.
module main

import gg

const win_width = 800
const win_height = 450
const img_w = 290
const img_h = 290

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	image_id   int         = -1
	pixels     []u8
	contrast   f32
	saturation f32
	brightness f32
	selected   int
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - color correction'
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

fn clamp8(v f32) u8 {
	if v < 0 {
		return 0
	}
	if v > 255 {
		return 255
	}
	return u8(v)
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.image_id = app.gg.new_streaming_image(img_w, img_h, 4, gg.StreamingImageConfig{})
}

fn update(dt f32, data voidptr) {
	_ = dt
	mut app := unsafe { &App(data) }
	step := f32(0.5) * dt * 60
	if app.gg.pressed_keys[gg.KeyCode.left] {
		app.contrast -= step
	} else if app.gg.pressed_keys[gg.KeyCode.right] {
		app.contrast += step
	}
	if app.gg.pressed_keys[gg.KeyCode.down] {
		app.brightness -= step
	} else if app.gg.pressed_keys[gg.KeyCode.up] {
		app.brightness += step
	}
	if app.contrast < -100 {
		app.contrast = -100
	}
	if app.contrast > 100 {
		app.contrast = 100
	}
	if app.brightness < -100 {
		app.brightness = -100
	}
	if app.brightness > 100 {
		app.brightness = 100
	}
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	match e.key_code {
		._1 { app.selected = 0 }
		._2 { app.selected = 1 }
		._3 { app.selected = 2 }
		._4 { app.selected = 3 }
		.r {
			app.contrast = 0
			app.saturation = 0
			app.brightness = 0
		}
		else {}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	for y in 0 .. img_h {
		for x in 0 .. img_w {
			// Procedural "picture" so no external assets are needed.
			base_r := f32(x) / f32(img_w) * 255
			base_g := f32(y) / f32(img_h) * 255
			base_b := f32((x + y) % 255)
			r := (base_r - 128) * (1 + app.contrast / 100) + 128 + app.brightness * 2 + app.saturation
			g := (base_g - 128) * (1 + app.contrast / 100) + 128 + app.brightness * 2 + app.saturation
			b := (base_b - 128) * (1 + app.contrast / 100) + 128 + app.brightness * 2 + app.saturation
			at := (y * img_w + x) * 4
			app.pixels[at] = clamp8(r)
			app.pixels[at + 1] = clamp8(g)
			app.pixels[at + 2] = clamp8(b)
			app.pixels[at + 3] = 255
		}
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.draw_image_by_id(f32(win_width / 2 - img_w / 2), f32(win_height / 2 - img_h / 2 - 30), f32(img_w),
		f32(img_h), app.image_id)
	ctx.draw_rect_filled(580, 0, f32(win_width - 580), f32(win_height), gg.Color{232, 232, 232, 255})
	ctx.draw_text(585, 40, 'Color Correction (CPU)', size: 18, color: gg.gray)
	ctx.draw_text(600, 75, 'Picture ${app.selected + 1}/4', size: 12, color: gg.gray)
	ctx.draw_text(600, 110, 'Contrast: ${app.contrast:.0f}', size: 12, color: gg.dark_gray)
	ctx.draw_text(600, 130, 'Brightness: ${app.brightness:.0f}', size: 12, color: gg.dark_gray)
	ctx.draw_text(600, 150, 'L/R contrast, U/D brightness, 1-4 picture, R reset',
		size:  10
		color: gg.gray
	)
	ctx.end()
}

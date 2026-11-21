// Port of raylib [shaders] example - shapes textures.
// PORT NOTE: shader grayscale.fs applied to a texture requires gg user-shader support (not yet
// available); the "custom shader" region is approximated with grayed-out colors and a
// grayscale procedural Image drawn with gg's ImageEffect support.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	image_id int         = -1
	pixels   []u8
	gray     gg.Image
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: 64 * 64 * 4, init: 255}
	for i in 0 .. 64 * 64 {
		v := u8((i % 64) * 4)
		app.pixels[i * 4] = v
		app.pixels[i * 4 + 1] = v
		app.pixels[i * 4 + 2] = v
		app.pixels[i * 4 + 3] = 255
	}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - shapes textures'
		bg_color:      gg.white
		user_data:     app
		init_fn:       init
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.image_id = app.gg.new_streaming_image(64, 64, 4, gg.StreamingImageConfig{})
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	app.gray = app.gg.create_image('resources/fudesumi.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(20, 40, 'USING DEFAULT SHADER', size: 10, color: gg.red)
	ctx.draw_circle_filled(80, 120, 35, gg.dark_blue)
	ctx.draw_circle_filled(80, 220, 60, gg.light_green)
	ctx.draw_circle_empty(80, 340, 80, gg.dark_blue)

	ctx.draw_text(190, 40, 'USING CUSTOM SHADER (gray)', size: 10, color: gg.red)
	ctx.draw_rect_filled(190, 90, 120, 60, gg.gray)
	ctx.draw_rect_filled(160, 170, 180, 130, gg.dark_gray)
	ctx.draw_rect_empty(210, 320, 80, 60, gg.light_gray)

	ctx.draw_text(370, 40, 'USING DEFAULT SHADER', size: 10, color: gg.red)
	ctx.draw_triangle_filled(430, 80, 370, 150, 490, 150, gg.purple)
	ctx.draw_triangle_empty(430, 160, 410, 230, 450, 230, gg.dark_blue)
	ctx.draw_polygon_filled(430, 320, 80, 6, 0, gg.rgb(165, 42, 42))

	if app.image_id >= 0 {
		ctx.draw_image_by_id(500, 60, 240, 240, app.image_id)
	}
	ctx.draw_text(380, win_height - 20, '(c) Fudesumi sprite by Eiden Marsal',
		size:  10
		color: gg.gray
	)
	ctx.end()
}

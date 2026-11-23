module main

// Port of raylib textures_image_loading.c
import gg

const load_raywhite = gg.Color{245, 245, 245, 255}
const load_gray = gg.Color{128, 128, 128, 255}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	texture gg.Image
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/raylib_logo.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	app.ctx.draw_image(400 - f32(app.texture.width) / 2, 225 - f32(app.texture.height) / 2,
		f32(app.texture.width), f32(app.texture.height), &app.texture)
	app.ctx.draw_text(300, 370, 'this IS a texture loaded from an image!',
		size:  10
		color: load_gray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - image loading'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     load_raywhite
	)
	app.ctx.run()
}

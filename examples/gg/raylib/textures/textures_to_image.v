module main

// Port of raylib textures_to_image.c
// PORT NOTE: gg has no GPU texture readback (LoadImageFromTexture), so the
// texture -> image -> texture round trip is omitted; the loaded image is
// uploaded and drawn directly, which is the observable result.
import gg

const to_image_raywhite = gg.Color{245, 245, 245, 255}
const to_image_gray = gg.Color{128, 128, 128, 255}

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
		color: to_image_gray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - to image'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     to_image_raywhite
	)
	app.ctx.run()
}

module main

// Port of raylib textures_image_text.c
// PORT NOTE: gg cannot rasterise a TTF font into a custom sprite font atlas
// (LoadFontEx/ImageDrawTextEx). The caption is drawn as regular 2D text over
// the texture at render time, and SPACE shows a placeholder box labelled as
// the font atlas instead of the real atlas image.
import gg

const itext_raywhite = gg.Color{245, 245, 245, 255}
const itext_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	texture   gg.Image
	show_font bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/parrots.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.show_font = app.ctx.pressed_keys[gg.KeyCode.space]
	app.ctx.begin()
	if !app.show_font {
		x := 400 - f32(app.texture.width) / 2
		y := 225 - f32(app.texture.height) / 2 - 20
		app.ctx.draw_image(x, y, f32(app.texture.width), f32(app.texture.height), &app.texture)
		app.ctx.draw_text(x + 20, y + 300, '[Parrots font drawing]', size: 24, color: gg.white)
	} else {
		app.ctx.draw_rect_empty(40, 40, 720, 370, itext_darkgray)
		app.ctx.draw_text(280, 200, 'FONT ATLAS (unavailable in gg)',
			size:  24
			color: itext_darkgray
		)
	}
	app.ctx.draw_text(290, 420, 'PRESS SPACE to SHOW FONT ATLAS USED',
		size:  10
		color: itext_darkgray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - image text'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     itext_raywhite
	)
	app.ctx.run()
}

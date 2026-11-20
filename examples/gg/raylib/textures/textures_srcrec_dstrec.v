module main

// Port of raylib textures_srcrec_dstrec.c
import gg

const srcrec_raywhite = gg.Color{245, 245, 245, 255}
const srcrec_gray = gg.Color{128, 128, 128, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	texture  gg.Image
	rotation f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/scarfy.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.rotation++
	fw := f32(app.texture.width) / 6
	fh := f32(app.texture.height)
	app.ctx.begin()
	app.ctx.draw_image_with_config(
		img:       &app.texture
		img_rect:  gg.Rect{400, 225, fw * 2, fh * 2}
		part_rect: gg.Rect{0, 0, fw, fh}
		rotation:  app.rotation
	)
	app.ctx.draw_line(400, 0, 400, 450, srcrec_gray)
	app.ctx.draw_line(0, 225, 800, 225, srcrec_gray)
	app.ctx.draw_text(800 - 220, 430, '(c) Scarfy spritesheet by Eiden Marsal',
		size:  10
		color: srcrec_gray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - srcrec dstrec'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     srcrec_raywhite
	)
	app.ctx.run()
}

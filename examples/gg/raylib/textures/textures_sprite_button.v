module main

// Port of raylib textures_sprite_button.c
// PORT NOTE: raylib plays a WAV sound on activation; gg's audio wrapper is not
// used here, so the press action is visual only.
import gg

const btn_raywhite = gg.Color{245, 245, 245, 255}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	texture   gg.Image
	btn_state int
	frame_h   f32
	btn_x     f32
	btn_y     f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/button.png') or { gg.Image{} }
	app.frame_h = f32(app.texture.height) / 3
	app.btn_x = 400 - f32(app.texture.width) / 2
	app.btn_y = 225 - app.frame_h / 2
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	mx := f32(app.ctx.mouse_pos_x)
	my := f32(app.ctx.mouse_pos_y)
	hover := mx >= app.btn_x && mx <= app.btn_x + f32(app.texture.width)
		&& my >= app.btn_y && my <= app.btn_y + app.frame_h
	if hover {
		app.btn_state = if app.ctx.mouse_buttons.has(.left) { 2 } else { 1 }
	} else {
		app.btn_state = 0
	}
	app.ctx.begin()
	app.ctx.draw_image_with_config(
		img:       &app.texture
		img_rect:  gg.Rect{app.btn_x, app.btn_y, f32(app.texture.width), app.frame_h}
		part_rect: gg.Rect{0, f32(app.btn_state) * app.frame_h, f32(app.texture.width), app.frame_h}
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - sprite button'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     btn_raywhite
	)
	app.ctx.run()
}

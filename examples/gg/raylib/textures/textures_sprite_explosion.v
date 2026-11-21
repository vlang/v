module main

// Port of raylib textures_sprite_explosion.c
// PORT NOTE: the boom.wav sound is not played (gg audio setup omitted).
import gg

const exp_raywhite = gg.Color{245, 245, 245, 255}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	texture gg.Image
	active  bool
	frame   int
	line    int
	counter int
	pos_x   f32
	pos_y   f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/explosion.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if !app.active && app.ctx.mouse_buttons.has(.left) {
		app.pos_x = f32(app.ctx.mouse_pos_x) - f32(app.texture.width) / 5 / 2
		app.pos_y = f32(app.ctx.mouse_pos_y) - f32(app.texture.height) / 5 / 2
		app.active = true
	}
	if app.active {
		app.counter++
		if app.counter > 2 {
			app.counter = 0
			app.frame++
			if app.frame >= 5 {
				app.frame = 0
				app.line++
				if app.line >= 5 {
					app.line = 0
					app.active = false
				}
			}
		}
	}
	app.ctx.begin()
	if app.active {
		fw := f32(app.texture.width) / 5
		fh := f32(app.texture.height) / 5
		app.ctx.draw_image_with_config(
			img:       &app.texture
			img_rect:  gg.Rect{app.pos_x, app.pos_y, fw, fh}
			part_rect: gg.Rect{f32(app.frame) * fw, f32(app.line) * fh, fw, fh}
		)
	}
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - sprite explosion'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     exp_raywhite
	)
	app.ctx.run()
}

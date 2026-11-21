module main

// Port of raylib textures_sprite_stacking.c
import gg

const stack_raywhite = gg.Color{245, 245, 245, 255}
const stack_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx           &gg.Context = unsafe { nil }
	texture       gg.Image
	stack_scale   f32 = 3.0
	stack_spacing f32 = 2.0
	stack_count   int = 122
	rotation      f32
	rot_speed     f32 = 30.0
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/booth.png') or { gg.Image{} }
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.pressed_keys[gg.KeyCode.left] || app.ctx.pressed_keys[gg.KeyCode.a] {
		app.rot_speed -= 0.25
	}
	if app.ctx.pressed_keys[gg.KeyCode.right] || app.ctx.pressed_keys[gg.KeyCode.d] {
		app.rot_speed += 0.25
	}
	app.stack_spacing += f32(app.ctx.scroll_y) * 0.1
	if app.stack_spacing < 0 {
		app.stack_spacing = 0
	}
	if app.stack_spacing > 5 {
		app.stack_spacing = 5
	}
	app.rotation += app.rot_speed * dt
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	fw := f32(app.texture.width)
	fh := f32(app.texture.height) / f32(app.stack_count)
	sw := fw * app.stack_scale
	sh := fh * app.stack_scale
	mut i := app.stack_count - 1
	for i >= 0 {
		y := 225 + f32(i) * app.stack_spacing - app.stack_spacing * f32(app.stack_count) / 2
		app.ctx.draw_image_with_config(
			img:       &app.texture
			img_rect:  gg.Rect{400 - sw / 2, y, sw, sh}
			part_rect: gg.Rect{0, f32(i) * fh, fw, fh}
			rotation:  app.rotation
		)
		i--
	}
	app.ctx.draw_text(10, 10, "A/D to spin\nmouse wheel to change separation (aka 'angle')",
		size:  20
		color: stack_darkgray
	)
	app.ctx.draw_text(10, 50, 'current spacing: ${app.stack_spacing:.1f}',
		size:  20
		color: stack_darkgray
	)
	app.ctx.draw_text(10, 70, 'current speed: ${app.rot_speed:.2f}',
		size:  20
		color: stack_darkgray
	)
	app.ctx.draw_text(10, 420, 'redbooth model (c) kluchek under cc 4.0',
		size:  20
		color: stack_darkgray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - sprite stacking'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     stack_raywhite
	)
	app.ctx.run()
}

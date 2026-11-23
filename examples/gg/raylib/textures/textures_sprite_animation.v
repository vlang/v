module main

// Port of raylib textures_sprite_animation.c
import gg

const anim_raywhite = gg.Color{245, 245, 245, 255}
const anim_darkgray = gg.Color{80, 80, 80, 255}
const anim_maroon = gg.Color{128, 0, 0, 255}
const anim_lime = gg.Color{0, 255, 0, 255}

struct App {
mut:
	ctx          &gg.Context = unsafe { nil }
	texture      gg.Image
	current      int
	frames_count int
	frames_speed int = 8
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/scarfy.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.frames_count++
	if app.frames_count >= 60 / app.frames_speed {
		app.frames_count = 0
		app.current++
		if app.current > 5 {
			app.current = 0
		}
	}
	app.ctx.begin()
	app.ctx.draw_image(15, 40, f32(app.texture.width), f32(app.texture.height), &app.texture)
	app.ctx.draw_rect_empty(15, 40, f32(app.texture.width), f32(app.texture.height), anim_lime)
	fw := f32(app.texture.width) / 6
	app.ctx.draw_rect_empty(15 + f32(app.current) * fw, 40, fw, f32(app.texture.height), gg.red)

	app.ctx.draw_text(165, 210, 'FRAME SPEED: ', size: 10, color: anim_darkgray)
	app.ctx.draw_text(575, 210, '${app.frames_speed} FPS', size: 10, color: anim_darkgray)
	app.ctx.draw_text(290, 240, 'PRESS RIGHT/LEFT KEYS to CHANGE SPEED!',
		size:  10
		color: anim_darkgray
	)
	for i in 0 .. 15 {
		x := f32(250 + 21 * i)
		if i < app.frames_speed {
			app.ctx.draw_rect_filled(x, 205, 20, 20, gg.red)
		}
		app.ctx.draw_rect_empty(x, 205, 20, 20, anim_maroon)
	}
	app.ctx.draw_image_with_config(
		img:       &app.texture
		img_rect:  gg.Rect{350, 280, fw, f32(app.texture.height)}
		part_rect: gg.Rect{f32(app.current) * fw, 0, fw, f32(app.texture.height)}
	)
	app.ctx.draw_text(800 - 220, 430, '(c) Scarfy spritesheet by Eiden Marsal',
		size:  10
		color: anim_darkgray
	)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		if e.key_code == .right && app.frames_speed < 15 {
			app.frames_speed++
		} else if e.key_code == .left && app.frames_speed > 1 {
			app.frames_speed--
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - sprite animation'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     anim_raywhite
	)
	app.ctx.run()
}

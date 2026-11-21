module main

// Port of raylib textures_blend_modes.c
// PORT NOTE: raylib's BLEND_ADD_COLORS mode has no gg pipeline equivalent;
// the fourth mode maps to gg's screen effect, which is the closest fit.
import gg

const blend_raywhite = gg.Color{245, 245, 245, 255}
const blend_gray = gg.Color{128, 128, 128, 255}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	background gg.Image
	foreground gg.Image
	blend_mode int
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.background = app.ctx.create_image('resources/cyberpunk_street_background.png') or { gg.Image{} }
	app.foreground = app.ctx.create_image('resources/cyberpunk_street_foreground.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	app.ctx.draw_image(400 - f32(app.background.width) / 2, 225 - f32(app.background.height) / 2,
		f32(app.background.width), f32(app.background.height), &app.background)

	effect := match app.blend_mode {
		0 { gg.ImageEffect.alpha }
		1 { gg.ImageEffect.add }
		2 { gg.ImageEffect.multiply }
		else { gg.ImageEffect.screen }
	}
	app.ctx.draw_image_with_config(
		img:      &app.foreground
		img_rect: gg.Rect{400 - f32(app.foreground.width) / 2, 225 - f32(app.foreground.height) / 2, f32(app.foreground.width), f32(app.foreground.height)}
		effect:   effect
	)

	names := ['BLEND_ALPHA', 'BLEND_ADDITIVE', 'BLEND_MULTIPLIED', 'BLEND_ADD_COLORS/SCREEN']
	app.ctx.draw_text(310, 350, 'Press SPACE to change blend modes.', size: 10, color: blend_gray)
	app.ctx.draw_text(340, 370, 'Current: ${names[app.blend_mode]}', size: 10, color: blend_gray)
	app.ctx.draw_text(800 - 330, 430, '(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)',
		size:  10
		color: blend_gray
	)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.blend_mode = (app.blend_mode + 1) % 4
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - blend modes'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     blend_raywhite
	)
	app.ctx.run()
}

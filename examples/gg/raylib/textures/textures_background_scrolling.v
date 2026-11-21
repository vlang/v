module main

// Port of raylib textures_background_scrolling.c
import gg

const scroll_bg_color = gg.Color{5, 44, 70, 255}
const scroll_raywhite = gg.Color{245, 245, 245, 255}

struct App {
mut:
	ctx           &gg.Context = unsafe { nil }
	bg            gg.Image
	mid           gg.Image
	fg            gg.Image
	scrolling_bg  f32
	scrolling_mid f32
	scrolling_fg  f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.bg = app.ctx.create_image('resources/cyberpunk_street_background.png') or { gg.Image{} }
	app.mid = app.ctx.create_image('resources/cyberpunk_street_midground.png') or { gg.Image{} }
	app.fg = app.ctx.create_image('resources/cyberpunk_street_foreground.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.scrolling_bg -= 0.1
	app.scrolling_mid -= 0.5
	app.scrolling_fg -= 1.0
	if app.scrolling_bg <= -f32(app.bg.width) * 2 {
		app.scrolling_bg = 0
	}
	if app.scrolling_mid <= -f32(app.mid.width) * 2 {
		app.scrolling_mid = 0
	}
	if app.scrolling_fg <= -f32(app.fg.width) * 2 {
		app.scrolling_fg = 0
	}

	app.ctx.begin()
	app.ctx.draw_image(app.scrolling_bg, 20, f32(app.bg.width) * 2, f32(app.bg.height) * 2, &app.bg)
	app.ctx.draw_image(f32(app.bg.width) * 2 + app.scrolling_bg, 20, f32(app.bg.width) * 2,
		f32(app.bg.height) * 2, &app.bg)
	app.ctx.draw_image(app.scrolling_mid, 20, f32(app.mid.width) * 2, f32(app.mid.height) * 2, &app.mid)
	app.ctx.draw_image(f32(app.mid.width) * 2 + app.scrolling_mid, 20, f32(app.mid.width) * 2,
		f32(app.mid.height) * 2, &app.mid)
	app.ctx.draw_image(app.scrolling_fg, 70, f32(app.fg.width) * 2, f32(app.fg.height) * 2, &app.fg)
	app.ctx.draw_image(f32(app.fg.width) * 2 + app.scrolling_fg, 70, f32(app.fg.width) * 2,
		f32(app.fg.height) * 2, &app.fg)

	app.ctx.draw_text(10, 10, 'BACKGROUND SCROLLING & PARALLAX', size: 20, color: gg.red)
	app.ctx.draw_text(800 - 330, 430, '(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)',
		size:  10
		color: scroll_raywhite
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - background scrolling'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     scroll_bg_color
	)
	app.ctx.run()
}

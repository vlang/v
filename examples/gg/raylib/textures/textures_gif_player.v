module main

// Port of raylib textures_gif_player.c
// PORT NOTE: gg has no GIF decoder (LoadImageAnim). A horizontal spritesheet
// (scarfy.png) is used as a stand-in, with the same frame-delay controls and
// offset readout as the original GIF example.
import gg

const gif_raywhite = gg.Color{245, 245, 245, 255}
const gif_gray = gg.Color{128, 128, 128, 255}
const gif_darkgray = gg.Color{80, 80, 80, 255}
const gif_maroon = gg.Color{128, 0, 0, 255}

const max_delay = 20
const min_delay = 1

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	sheet      gg.Image
	frames     int = 6
	current    int
	delay      int = 8
	frame_cntr int
	offset     int
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.sheet = app.ctx.create_image('resources/scarfy.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.frame_cntr++
	if app.frame_cntr >= app.delay {
		app.current++
		if app.current >= app.frames {
			app.current = 0
		}
		app.offset = app.sheet.width * app.sheet.height * 4 * app.current
		app.frame_cntr = 0
	}
	app.ctx.begin()
	app.ctx.draw_text(50, 30, 'TOTAL GIF FRAMES:  ${app.frames}', size: 20, color: gif_light())
	app.ctx.draw_text(50, 60, 'CURRENT FRAME: ${app.current}', size: 20, color: gif_gray)
	app.ctx.draw_text(50, 90, 'CURRENT FRAME IMAGE.DATA OFFSET: ${app.offset}',
		size:  20
		color: gif_gray
	)
	app.ctx.draw_text(100, 305, 'FRAMES DELAY: ', size: 10, color: gif_darkgray)
	app.ctx.draw_text(620, 305, '${app.delay} frames', size: 10, color: gif_darkgray)
	app.ctx.draw_text(290, 350, 'PRESS RIGHT/LEFT KEYS to CHANGE SPEED!',
		size:  10
		color: gif_darkgray
	)
	for i in 0 .. max_delay {
		x := f32(190 + 21 * i)
		if i < app.delay {
			app.ctx.draw_rect_filled(x, 300, 20, 20, gg.red)
		}
		app.ctx.draw_rect_empty(x, 300, 20, 20, gif_maroon)
	}
	fw := f32(app.sheet.width) / f32(app.frames)
	app.ctx.draw_image_with_config(
		img:       &app.sheet
		img_rect:  gg.Rect{400 - fw / 2, 140, fw, f32(app.sheet.height)}
		part_rect: gg.Rect{f32(app.current) * fw, 0, fw, f32(app.sheet.height)}
	)
	app.ctx.draw_text(600, 430, '(c) Scarfy sprite by Eiden Marsal', size: 10, color: gif_gray)
	app.ctx.end()
}

fn gif_light() gg.Color {
	return gg.Color{211, 211, 211, 255}
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		if e.key_code == .right && app.delay < max_delay {
			app.delay++
		} else if e.key_code == .left && app.delay > min_delay {
			app.delay--
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - gif player'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     gif_raywhite
	)
	app.ctx.run()
}

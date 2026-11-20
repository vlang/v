module main

// raylib [core] example - window letterbox
// A fixed 640x480 game view is scaled to fit the resizable window, with
// black bars preserving the aspect ratio. gg's set_viewport supplies the
// letterbox region directly, so no render texture is required.

import gg
import rand

const screen_width = 800
const screen_height = 450
const game_width = 640
const game_height = 480

const ray_white = gg.Color{245, 245, 245, 255}
const black = gg.Color{0, 0, 0, 255}
const green = gg.Color{0, 158, 47, 255}
const yellow = gg.Color{255, 203, 0, 255}
const white = gg.Color{255, 255, 255, 255}

struct App {
mut:
	gg     &gg.Context = unsafe { nil }
	colors []gg.Color  = []
}

fn main() {
	mut app := &App{}
	for _ in 0 .. 10 {
		app.colors << gg.Color{u8(rand.int_in_range(100, 250) or { 150 }), u8(rand.int_in_range(50, 150) or { 100 }), u8(rand.int_in_range(10, 100) or { 50 }), 255}
	}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - window letterbox'
		bg_color:     black
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space && !e.key_repeat {
		app.colors.clear()
		for _ in 0 .. 10 {
			app.colors << gg.Color{u8(rand.int_in_range(100, 250) or { 150 }), u8(rand.int_in_range(50, 150) or { 100 }), u8(rand.int_in_range(10, 100) or { 50 }), 255}
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	scale := if f32(screen_width) / game_width < f32(screen_height) / game_height {
		f32(screen_width) / game_width
	} else {
		f32(screen_height) / game_height
	}
	vw := int(f32(game_width) * scale)
	vh := int(f32(game_height) * scale)
	vx := (ctx.width - vw) / 2
	vy := (ctx.height - vh) / 2
	mouse_x := f32(ctx.mouse_pos_x)
	mouse_y := f32(ctx.mouse_pos_y)
	virtual_x := (mouse_x - f32(ctx.width - vw) * 0.5) / scale
	virtual_y := (mouse_y - f32(ctx.height - vh) * 0.5) / scale

	ctx.begin()
	ctx.set_viewport(vx, vy, vw, vh)
	ctx.draw_rect(x: 0, y: 0, w: f32(vw), h: f32(vh), color: ray_white)
	for i, c in app.colors {
		ctx.draw_rect(x: 0, y: f32(game_height / 10 * i) * scale, w: f32(vw), h: f32(game_height / 10) * scale, color: c)
	}
	ctx.draw_text(10, int(25 * scale),
		'If executed inside a window,\nyou can resize the window,\nand see the screen scaling!',
		size:  int(20 * scale)
		color: white
	)
	ctx.draw_text(int(350 * scale), int(25 * scale),
		'Default Mouse: [${int(mouse_x)} , ${int(mouse_y)}]',
		size:  int(20 * scale)
		color: green
	)
	ctx.draw_text(int(350 * scale), int(55 * scale),
		'Virtual Mouse: [${int(virtual_x)} , ${int(virtual_y)}]',
		size:  int(20 * scale)
		color: yellow
	)
	ctx.set_viewport(0, 0, ctx.width, ctx.height)
	ctx.end()
}

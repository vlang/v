module main

// raylib [core] example - scissor test
// A 300x300 scissor rectangle follows the mouse; press S to toggle it.
// gg exposes ctx.scissor_rect() from its UI helpers.

import gg

const screen_width = 800
const screen_height = 450
const scissor_size = 300

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const black = gg.Color{0, 0, 0, 255}
const red = gg.Color{255, 0, 0, 255}

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	scissor_on bool        = true
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - scissor test'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .s && !e.key_repeat {
		app.scissor_on = !app.scissor_on
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	x := ctx.mouse_pos_x - scissor_size / 2
	y := ctx.mouse_pos_y - scissor_size / 2
	ctx.begin()
	if app.scissor_on {
		ctx.scissor_rect(x, y, scissor_size, scissor_size)
	}
	ctx.draw_rect(x: 0, y: 0, w: f32(ctx.width), h: f32(ctx.height), color: red)
	ctx.draw_text(190, 200, 'Move the mouse around to reveal this text!',
		size:  20
		color: light_gray
	)
	if app.scissor_on {
		ctx.scissor_rect(0, 0, ctx.width, ctx.height)
	}
	ctx.draw_rect_empty(f32(x), f32(y), scissor_size, scissor_size, black)
	ctx.draw_text(10, 10, 'Press S to toggle scissor test', size: 20, color: black)
	ctx.end()
}

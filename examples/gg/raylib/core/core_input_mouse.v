module main

// raylib [core] example - input mouse
// The ball follows the mouse; clicking changes its colour. H toggles the
// cursor via sokol.sapp (gg has no higher-level cursor API).

import gg
import sokol.sapp

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const dark_blue = gg.Color{0, 82, 172, 255}
const red = gg.Color{255, 0, 0, 255}
const lime = gg.Color{124, 252, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	gg            &gg.Context = unsafe { nil }
	ball_color    gg.Color    = dark_blue
	cursor_hidden bool
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input mouse'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .h && !e.key_repeat {
		app.cursor_hidden = !app.cursor_hidden
		sapp.show_mouse(!app.cursor_hidden)
	}
	if e.typ == .mouse_down {
		app.ball_color = match e.mouse_button {
			.left { maroon }
			.middle { lime }
			.right { dark_blue }
			else { app.ball_color }
		}
		// PORT NOTE: gg/sokol exposes only left, right and middle mouse
		// buttons; raylib's SIDE/EXTRA/FORWARD/BACK buttons are unavailable.
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_circle_filled(f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y), 40, app.ball_color)
	ctx.draw_text(10, 10, 'move ball with mouse and click mouse button to change color',
		size:  20
		color: dark_gray
	)
	ctx.draw_text(10, 30, "Press 'H' to toggle cursor visibility", size: 20, color: dark_gray)
	if app.cursor_hidden {
		ctx.draw_text(20, 60, 'CURSOR HIDDEN', size: 20, color: red)
	} else {
		ctx.draw_text(20, 60, 'CURSOR VISIBLE', size: 20, color: lime)
	}
	ctx.end()
}

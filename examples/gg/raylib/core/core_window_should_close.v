module main

// raylib [core] example - window should close
// When a close is requested, ask for confirmation before quitting.
// PORT NOTE: sokol may still close on the platform's close button; the
// confirmation logic is demonstrated in the quit_requested event.

import gg
import sokol.sapp

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const black = gg.Color{0, 0, 0, 255}
const white = gg.Color{255, 255, 255, 255}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	requested bool
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - window should close'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .quit_requested {
		app.requested = true
	}
	if e.typ == .key_down && !e.key_repeat {
		if e.key_code == .escape {
			app.requested = true
		}
		if app.requested {
			if e.key_code == .y {
				sapp.quit()
			} else if e.key_code == .n {
				app.requested = false
			}
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	if app.requested {
		ctx.draw_rect(x: 0, y: 100, w: screen_width, h: 200, color: black)
		ctx.draw_text(40, 180, 'Are you sure you want to exit program? [Y/N]',
			size:  30
			color: white
		)
	} else {
		ctx.draw_text(120, 200, 'Try to close the window to get confirmation message!',
			size:  20
			color: light_gray
		)
	}
	ctx.end()
}

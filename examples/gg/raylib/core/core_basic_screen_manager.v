module main

// raylib [core] example - basic screen manager
// A tiny state machine: LOGO -> TITLE -> GAMEPLAY -> ENDING -> TITLE.
// ENTER or a tap advances the screen; the logo waits for 120 frames.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const green = gg.Color{0, 158, 47, 255}
const dark_green = gg.Color{0, 121, 64, 255}
const purple = gg.Color{127, 0, 255, 255}
const maroon = gg.Color{128, 0, 0, 255}
const blue = gg.Color{0, 121, 241, 255}
const dark_blue = gg.Color{0, 82, 172, 255}

enum GameScreen {
	logo
	title
	gameplay
	ending
}

struct App {
mut:
	gg           &gg.Context = unsafe { nil }
	screen       GameScreen  = .logo
	frames_count int
	gestures     gg.GestureTracker
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - basic screen manager'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }

	// A touch/mouse tap acts like the raylib GESTURE_TAP.
	if gesture := app.gestures.update(e) {
		if gesture.kind == .tap && app.screen != .logo {
			app.screen = match app.screen {
				.title { .gameplay }
				.gameplay { .ending }
				.ending { .title }
				else { app.screen }
			}
		}
	}
	if e.typ == .key_down && e.key_code == .enter && !e.key_repeat {
		app.screen = match app.screen {
			.title { .gameplay }
			.gameplay { .ending }
			.ending { .title }
			else { app.screen }
		}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	if app.screen == .logo {
		app.frames_count++
		if app.frames_count > 120 {
			app.screen = .title
		}
	}

	ctx.begin()
	match app.screen {
		.logo {
			ctx.draw_text(20, 20, 'LOGO SCREEN', size: 40, color: light_gray)
			ctx.draw_text(290, 220, 'WAIT for 2 SECONDS...', size: 20, color: gray)
		}
		.title {
			ctx.draw_rect(x: 0, y: 0, w: screen_width, h: screen_height, color: green)
			ctx.draw_text(20, 20, 'TITLE SCREEN', size: 40, color: dark_green)
			ctx.draw_text(120, 220, 'PRESS ENTER or TAP to JUMP to GAMEPLAY SCREEN',
				size:  20
				color: dark_green
			)
		}
		.gameplay {
			ctx.draw_rect(x: 0, y: 0, w: screen_width, h: screen_height, color: purple)
			ctx.draw_text(20, 20, 'GAMEPLAY SCREEN', size: 40, color: maroon)
			ctx.draw_text(130, 220, 'PRESS ENTER or TAP to JUMP to ENDING SCREEN',
				size:  20
				color: maroon
			)
		}
		.ending {
			ctx.draw_rect(x: 0, y: 0, w: screen_width, h: screen_height, color: blue)
			ctx.draw_text(20, 20, 'ENDING SCREEN', size: 40, color: dark_blue)
			ctx.draw_text(120, 220, 'PRESS ENTER or TAP to RETURN to TITLE SCREEN',
				size:  20
				color: dark_blue
			)
		}
	}
	ctx.end()
}

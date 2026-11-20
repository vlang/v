module main

// raylib [core] example - clipboard text
// Uses gg.set_clipboard_text / gg.get_clipboard_text. The raygui text box
// and buttons are replaced with a small keyboard-driven text field and
// clickable rectangles drawn with gg.
// PORT NOTE: gg has no bundled GUI toolkit, so editing is minimal.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{128, 0, 0, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const sky_blue = gg.Color{102, 191, 255, 255}
const dark_gray = gg.Color{80, 80, 80, 255}

struct Button {
mut:
	x      f32
	y      f32
	width  f32
	height f32
	label  string
}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	input     string      = 'Hello from raylib!'
	clipboard string
	editing   bool
	ctrl_down bool
	buttons   []Button = []
}

fn main() {
	mut app := &App{}
	app.buttons = [
		Button{50, 180, 158, 40, 'CUT'},
		Button{215, 180, 158, 40, 'COPY'},
		Button{380, 180, 158, 40, 'PASTE'},
		Button{545, 180, 158, 40, 'CLEAR'},
		Button{710, 120, 40, 40, 'RND'},
	]
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - clipboard text'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn cut(mut app App) {
	gg.set_clipboard_text(app.input)
	app.clipboard = gg.get_clipboard_text()
	app.input = ''
}

fn copy(mut app App) {
	gg.set_clipboard_text(app.input)
	app.clipboard = gg.get_clipboard_text()
}

fn paste(mut app App) {
	text := gg.get_clipboard_text()
	if text.len > 0 {
		app.input = text
	}
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	match e.typ {
		.mouse_down {
			mx, my := f32(e.mouse_x), f32(e.mouse_y)
			app.editing = mx >= 50 && mx <= 702 && my >= 120 && my <= 160
			for b in app.buttons {
				if mx >= b.x && mx <= b.x + b.width && my >= b.y && my <= b.y + b.height {
					match b.label {
						'CUT' { cut(mut app) }
						'COPY' { copy(mut app) }
						'PASTE' { paste(mut app) }
						'CLEAR' { app.input = '' }
						'RND' { app.input = 'The quick brown fox jumps over the lazy dog' }
						else {}
					}
				}
			}
		}
		.key_down {
			if e.key_code == .left_control || e.key_code == .right_control {
				app.ctrl_down = true
			}
			if !e.key_repeat {
				if app.ctrl_down {
					match e.key_code {
						.x { cut(mut app) }
						.c { copy(mut app) }
						.v { paste(mut app) }
						else {}
					}
				}
				if e.key_code == .backspace && app.input.len > 0 {
					app.input = app.input[0..app.input.len - 1]
				}
			}
		}
		.key_up {
			if e.key_code == .left_control || e.key_code == .right_control {
				app.ctrl_down = false
			}
		}
		.char {
			if app.editing && e.char_code >= 32 && e.char_code < 127 {
				app.input += rune(e.char_code).str()
			}
		}
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(50, 20, 'Use the BUTTONS or KEY SHORTCUTS:', size: 20, color: maroon)
	ctx.draw_text(50, 60, '[CTRL+X] - CUT | [CTRL+C] COPY | [CTRL+V] | PASTE',
		size:  20
		color: maroon
	)
	ctx.draw_rect(x: 50, y: 120, w: 652, h: 40, color: if app.editing { sky_blue } else { light_gray })
	ctx.draw_rect_empty(50, 120, 652, 40, gray)
	ctx.draw_text(56, 126, app.input + (if app.editing { '_' } else { '' }),
		size:  20
		color: dark_gray
	)
	for b in app.buttons {
		ctx.draw_rect(x: b.x, y: b.y, w: b.width, h: b.height, color: sky_blue)
		ctx.draw_rect_empty(b.x, b.y, b.width, b.height, gray)
		ctx.draw_text(int(b.x) + 8, int(b.y) + 8, b.label, size: 20, color: dark_gray)
	}
	ctx.draw_text(50, 260, 'Clipboard current text data:', size: 20, color: maroon)
	ctx.draw_text(50, 300, app.clipboard, size: 20, color: dark_gray)
	ctx.draw_text(50, 360, 'Try copying text from other applications and pasting here!',
		size:  20
		color: gray
	)
	ctx.end()
}

import term_input as input

struct App {
mut:
	ti &input.Context = 0
}

fn event(e &input.Event, x voidptr) {
	if e.typ == .key_down && e.code == .escape { exit(0) }
}

fn frame(x voidptr) {
	mut app := &App(x)

	app.ti.clear()
	app.ti.draw_text(4, 4, 'Hello there!')
	app.ti.set_bg_color(r: 180, g: 180, b: 250)
	app.ti.draw_rect(6, 12, 48, 24)
	app.ti.set_bg_color(r: 250, g: 180, b: 180)
	app.ti.draw_line(6, 12, 48, 24)
	app.ti.reset_bg_color()
	app.ti.flush()
}


mut app := &App{}
app.ti = input.init(
	user_data: app,
	event_fn: event,
	frame_fn: frame

	capture_events: true
	frame_rate: 60
)

print('\x1b[?25l') // Hide the mouse cursor
// println('V term.input event viewer (press `esc` to exit)\n\n')
app.ti.run()

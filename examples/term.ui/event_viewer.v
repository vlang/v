import term.ui as tui

struct App {
mut:
	tui &tui.Context = 0
}

fn event(e &tui.Event, x voidptr) {
	mut app := &App(x)
	app.tui.clear()
	app.tui.set_cursor_position(0, 0)
	app.tui.write('V term.input event viewer (press `esc` to exit)\n\n')
	app.tui.write('$e')
	app.tui.write('\n\nRaw event bytes: "${e.utf8.bytes().hex()}" = ${e.utf8.bytes()}')
	app.tui.flush()

	if e.typ == .key_down && e.code == .escape { exit(0) }
}

mut app := &App{}
app.tui = tui.init(
	user_data: app,
	event_fn: event

	window_title: 'V term.ui event viewer'
	hide_cursor: true
	capture_events: true
	frame_rate: 60
)

println('V term.input event viewer (press `esc` to exit)\n\n')
app.tui.run()

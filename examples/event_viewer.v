import term.input as ti

struct App {
mut:
	ti &ti.Context = 0
}

fn event(e &ti.Event, x voidptr) {
	print('\x1b[0;0H\x1b[2J\x1b[3J') // Clear everything
	println('V term.input event viewer (press `esc` to exit)\n\n')

	println(e)
	println('Raw event bytes: "${e.utf8.bytes().hex()}" = ${e.utf8.bytes()}')

	if e.modifiers == ti.ctrl | ti.alt {
		println('CTRL + ALT')
	}

	if e.typ == .key_down && e.code == .escape { exit(0) }
}

mut app := &App{}
app.ti = ti.init(
	user_data: app,
	event_fn: event

	hide_cursor: true
	capture_events: true
	frame_rate: 60
)

println('V term.input event viewer (press `esc` to exit)\n\n')
app.ti.run()

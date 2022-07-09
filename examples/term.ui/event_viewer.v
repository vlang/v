import term.ui as tui

struct App {
mut:
	tui &tui.Context = unsafe { 0 }
}

fn event(e &tui.Event, x voidptr) {
	mut app := &App(x)
	app.tui.clear()
	app.tui.set_cursor_position(0, 0)
	app.tui.write('V term.input event viewer (press `esc` to exit)\n\n')
	app.tui.write('$e')
	app.tui.write('\n\nRaw event bytes: "$e.utf8.bytes().hex()" = $e.utf8.bytes()')
	if !e.modifiers.is_empty() {
		app.tui.write('\nModifiers: $e.modifiers = ')
		if e.modifiers.has(.ctrl) {
			app.tui.write('ctrl. ')
		}
		if e.modifiers.has(.shift) {
			app.tui.write('shift ')
		}
		if e.modifiers.has(.alt) {
			app.tui.write('alt. ')
		}
	}
	app.tui.flush()

	if e.typ == .key_down && e.code == .escape {
		exit(0)
	}
}

fn main() {
	mut app := &App{}
	app.tui = tui.init(
		user_data: app
		event_fn: event
		window_title: 'V term.ui event viewer'
		hide_cursor: true
		capture_events: true
		frame_rate: 60
		use_alternate_buffer: false
	)
	println('V term.ui event viewer (press `esc` to exit)\n\n')
	app.tui.run()?
}

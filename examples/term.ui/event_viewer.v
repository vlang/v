import term.ui as tui

struct App {
mut:
	tui &tui.Context = unsafe { nil }
}

fn (mut app App) show_header() {
	app.tui.clear()
	app.tui.set_cursor_position(0, 0)
	app.tui.write('V term.input event viewer (press `esc` to exit)\n\n')
	app.tui.flush()
}

fn event(e &tui.Event, mut app App) {
	app.show_header()
	app.tui.write('${e}')
	app.tui.write('\n\nRaw event bytes: "${e.utf8.bytes().hex()}" = ${e.utf8.bytes()}')
	if !e.modifiers.is_empty() {
		app.tui.write('\nModifiers: ${e.modifiers} = ')
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
		println('\nGood bye.')
		exit(0)
	}
}

type EventFn = fn (&tui.Event, voidptr)

fn main() {
	mut app := &App{}
	app.tui = tui.init(
		user_data:            app
		event_fn:             EventFn(event)
		window_title:         'V term.ui event viewer'
		hide_cursor:          true
		capture_events:       true
		frame_rate:           60
		use_alternate_buffer: false
	)
	app.show_header()
	app.tui.run()!
}

import term_input as input

struct App {
mut:
	ti &input.Context = 0
}

fn init(app &App) {
	println('INIT')
}

fn frame(app &App) {
	println('FRAME')
}

fn event(e &input.Event, app &App) {
	println('EVENT ${e.typ:-12} - $e')
}

mut app := &App{}
app.ti = input.init(
	user_data: app,
	init_fn: init,
	frame_fn: frame,
	event_fn: event
)

app.ti.run()

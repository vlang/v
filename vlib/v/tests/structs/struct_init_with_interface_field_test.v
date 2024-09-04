interface Refresher {
	refresh()
}

struct Dummy {}

fn (d Dummy) refresh() {}

struct Source {
mut:
	refresher Refresher = Dummy{}
}

struct App {
mut:
	src Source
}

fn test_struct_init_with_interface_field() {
	mut app := &App{}
	app.src = Source{}

	println(app)
	assert '${app}'.contains('refresher: Refresher(Dummy{})')
}

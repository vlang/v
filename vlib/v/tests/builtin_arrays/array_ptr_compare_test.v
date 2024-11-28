@[heap]
struct Particle {
}

@[heap]
struct App {
mut:
	list_opti [][]&Particle
}

fn (mut app App) init_opti_list() {
	for mut liste in app.list_opti {
		if liste != [] {
			liste.clear()
		}
	}
}

fn test_main() {
	mut app := &App{
		list_opti: [][]&Particle{len: 10, init: []&Particle{}}
	}
	app.init_opti_list()
}

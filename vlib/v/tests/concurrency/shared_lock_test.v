struct St {
mut:
	a int
}

fn f(shared x St, shared y St, shared z St) {
	for _ in 0 .. 101 {
		lock x, y {
			tmp := y.a
			y.a = x.a
			x.a = tmp
		}
	}
	lock z {
		z.a--
	}
}

fn test_shared_lock() {
	shared x := St{
		a: 5
	}
	shared y := &St{
		a: 7
	}
	shared z := St{
		a: 1
	}
	worker := spawn f(shared x, shared y, shared z)
	for _ in 0 .. 100 {
		lock x, y {
			tmp := x.a
			x.a = y.a
			y.a = tmp
		}
	}
	worker.wait()
	rlock z {
		assert z.a == 0
	}
	rlock x, y {
		assert x.a == 7 && y.a == 5
	}
}

struct App {
	id string = 'test'
mut:
	app_data shared AppData
}

fn (mut a App) init_server_direct() {
	lock a.app_data {
		// a.app_data = AppData{}
	}
}

struct AppData {
	id string = 'foo'
}

fn test_shared_field_init() {
	mut app1 := App{}
	app1.init_server_direct()
	id := rlock app1.app_data {
		app1.app_data.id
	}
	// assert id == 'foo'
}

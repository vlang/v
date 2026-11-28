// vtest retry: 3

struct App {
mut:
	idx atomic int
}

fn test_atomic() {
	mut app := &App{}
	worker_count := 10
	mut workers := []thread{cap: worker_count}
	for _ in 0 .. worker_count {
		workers << spawn app.run()
	}
	workers.wait()
	println('idx=${app.idx}')
	assert app.idx == worker_count
}

fn (mut app App) run() {
	app.idx++
}

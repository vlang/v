import time

// vtest flaky: true
// vtest retry: 3

struct App {
mut:
	idx atomic int
}

fn test_atomic() {
	mut app := &App{}
	for i in 0 .. 10 {
		spawn app.run()
	}
	time.sleep(200 * time.millisecond)
	println('idx=${app.idx}')
	assert app.idx == 10
}

fn (mut app App) run() {
	app.idx++
}

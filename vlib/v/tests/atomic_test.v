import time

struct App {
mut:
	idx atomic int
}

fn test_atomic() {
	mut app := &App{}
	for i in 0 .. 10 {
		go app.run()
	}
	time.sleep(200 * time.millisecond)
	println('idx=$app.idx')
	assert app.idx == 10
}

fn (mut app App) run() {
	app.idx++
}

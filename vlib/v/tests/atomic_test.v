import term
import os
import runtime
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
	time.sleep(2 * time.second)
	println('idx=$app.idx')
	assert app.idx == 10
}

fn (mut app App) run() {
	app.idx++
}

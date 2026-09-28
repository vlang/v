struct Window {
mut:
	width int
}

fn update(mut w Window) { w.width = 42 }

fn schedule(mut w Window) {
	callback := fn [mut w] () {
		task := spawn update(mut w)
		task.wait()
	}
	callback()
}

fn test_spawn_mut_capture() {
	mut w := &Window{}
	schedule(mut w)
	assert w.width == 42
}

fn (mut w Window) update_method() { w.width = 43 }

fn schedule_method(mut w Window) {
	outer := fn [mut w] () {
		inner := fn [mut w] () {
			task := spawn w.update_method()
			task.wait()
		}
		inner()
	}
	outer()
}

fn test_spawn_nested_mut_capture_receiver() {
	mut w := &Window{}
	schedule_method(mut w)
	assert w.width == 43
}

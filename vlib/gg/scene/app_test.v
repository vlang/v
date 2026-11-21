module scene

fn counting_init(mut a App) {
	a.elapsed += 1
}

fn counting_frame(mut a App, dt f32) {
	a.elapsed += dt
}

fn test_app_lifecycle() {
	mut app := new_app()
	assert app.add(System{ name: 'counter', init: counting_init, frame: counting_frame }) == 0
	assert !app.running
	app.start()
	assert app.running
	assert app.elapsed == 1
	app.update(0.5)
	assert app.elapsed == 2
	app.stop()
	assert !app.running
	app.update(0.5)
	assert app.elapsed == 2
}

fn test_app_stop_without_start() {
	mut app := new_app()
	app.stop()
	assert !app.running
}

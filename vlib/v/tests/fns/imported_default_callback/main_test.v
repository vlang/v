import api

fn test_imported_callback_default() {
	mut window := api.window(api.WindowCfg{ on_init: fn (mut w api.Window) { w.count++ } })
	event := api.Event{ value: 1 }
	window.on_event(&event, mut window)
	assert window.count == 10
}

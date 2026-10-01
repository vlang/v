import api
import foreign
import selected

struct Event {
	label string
}

fn test_imported_callback_default() {
	mut window := api.window(api.WindowCfg{ on_init: fn (mut w api.Window) { w.count++ } })
	event := api.Event{ value: 1 }
	window.on_event(&event, mut window)
	assert window.count == 10
}

fn test_imported_callback_return_and_selective_parameter_types() {
	assert Event{ label: 'caller' }.label == 'caller'
	local_defaults := api.ResultDefaults{}
	assert local_defaults.make_event().value == 42
	local_defaults.make_pair()
	events := chan api.Event{cap: 1}
	local_defaults.on_channel(events)
	imported_defaults := selected.ImportedDefaults{}
	assert imported_defaults.on_event(&foreign.Event{ value: 91 }) == 91
	assert imported_defaults.make_event().value == 73
}

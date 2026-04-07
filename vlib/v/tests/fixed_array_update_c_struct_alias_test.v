module main

import sokol.sapp

struct Event {
	touches [8]sapp.TouchPoint
}

fn test_fixed_array_update_with_c_struct_alias() {
	e := Event{}
	ev := Event{
		...e
	}
	assert ev.touches.len == 8
	assert ev.touches[0].identifier == 0
	assert ev.touches[7].identifier == 0
}

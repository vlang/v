module gg

fn test_gamepad_buttons() {
	mut g := new_gamepad()
	assert !g.button_down(0)
	g.set_button(0, true)
	assert g.button_down(0)
	assert g.button_just_pressed(0)
	g.end_frame()
	assert !g.button_just_pressed(0)
	assert g.button_down(0)
	g.set_button(0, false)
	assert !g.button_down(0)
	g.set_button(99, true)
	assert !g.button_down(99)
}

fn test_gamepad_axes() {
	mut g := new_gamepad()
	g.set_axis(0, 0.1)
	assert g.axis(0) == 0
	g.set_axis(0, -0.8)
	assert g.axis(0) == -0.8
	assert g.axis(9) == 0
}

fn test_gamepad_connection() {
	mut g := new_gamepad()
	g.set_connected(true, 'test pad')
	assert g.connected
	assert g.id == 'test pad'
	g.set_button(1, true)
	g.set_connected(false, '')
	assert !g.connected
	assert !g.button_down(1)
}

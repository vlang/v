module gg

fn test_input_key_edges() {
	mut s := new_input_state()
	assert !s.is_down(.a)
	s.on_key_down(.a)
	assert s.is_down(.a)
	assert s.just_pressed(.a)
	s.on_key_down(.a)
	assert s.just_pressed(.a)
	s.end_frame()
	assert !s.just_pressed(.a)
	assert s.is_down(.a)
	s.on_key_up(.a)
	assert s.just_released(.a)
	assert !s.is_down(.a)
	s.end_frame()
	assert !s.just_released(.a)
}

fn test_input_mouse() {
	mut s := new_input_state()
	s.on_mouse_button(.left, true, 12, 34)
	assert s.mouse_down(.left)
	assert s.mouse_just_pressed(.left)
	p := s.mouse_pos()
	assert p.x == 12 && p.y == 34
	s.on_mouse_move(50, 60)
	assert s.mouse_pos().x == 50
	s.end_frame()
	assert !s.mouse_just_pressed(.left)
	s.on_mouse_button(.left, false, 50, 60)
	assert s.mouse_just_released(.left)
}

fn test_input_actions() {
	mut s := new_input_state()
	s.bind('jump', [.space, .w])
	assert !s.action_down('jump')
	assert !s.action_just_pressed('missing')
	s.on_key_down(.w)
	assert s.action_down('jump')
	assert s.action_just_pressed('jump')
	s.end_frame()
	assert !s.action_just_pressed('jump')
}

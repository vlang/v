const mouse_action_to_function_map = {
	1: handle_mouse_down_signal
}

fn handle_mouse_down_signal() string {
	return 'hello'
}

fn test_option_map_fn_type_value() {
	t := mouse_action_to_function_map[1] or { return }
	println(t())
	assert t() == 'hello'
}

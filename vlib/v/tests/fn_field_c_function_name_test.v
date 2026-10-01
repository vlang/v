struct CNameCallbackHolder {
	cb fn (int) int = unsafe { nil }
}

fn c_name_callback_double(value int) int {
	return value * 2
}

fn c_name_callback_parameter(wait CNameCallbackHolder) int {
	return wait.cb(21)
}

fn test_fn_field_receiver_can_shadow_a_c_function() {
	wait := CNameCallbackHolder{ cb: c_name_callback_double }
	read := CNameCallbackHolder{ cb: c_name_callback_double }
	close := &CNameCallbackHolder{ cb: c_name_callback_double }
	assert wait.cb(21) == 42
	assert read.cb(22) == 44
	assert close.cb(23) == 46
	assert c_name_callback_parameter(wait) == 42
}

fn test_fn_field_iteration_receiver_can_shadow_a_c_function() {
	for wait in [CNameCallbackHolder{ cb: c_name_callback_double }] {
		assert wait.cb(24) == 48
	}
}

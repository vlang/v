import callbacks

// This unrelated function must not supply the type of callbacks' local `h`.
fn h(value string) string {
	return value
}

fn handler(fail bool) !callbacks.Response {
	if fail {
		return error('callback failed')
	}
	return callbacks.Response{ value: 42 }
}

fn test_aliased_callback_parameter_shadows_main_function() {
	assert h('main') == 'main'
	response := callbacks.forward(handler, false) or { panic(err) }
	assert response.value == 42
	callbacks.forward(handler, true) or {
		assert err.msg() == 'callback failed'
		return
	}
	assert false, 'callback error was lost'
}

fn test_aliased_callback_map_guard_shadows_main_function() {
	handlers := {
		'custom': callbacks.Handler(handler)
	}
	response := callbacks.dispatch(handlers, false) or { panic(err) }
	assert response.value == 42
	callbacks.dispatch(handlers, true) or {
		assert err.msg() == 'callback failed'
		return
	}
	assert false, 'callback error was lost'
}

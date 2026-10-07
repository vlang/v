fn decode_shadow[T](value &T) T {
	return *value
}

fn apply_shadow[E, K](decode_shadow fn (E) K, value E) K {
	return decode_shadow(value)
}

fn apply_local_shadow[E, K](callback fn (E) K, value E) K {
	decode_shadow := callback
	return decode_shadow(value)
}

fn apply_closure_shadow[E, K](decode_shadow fn (E) K, value E) K {
	callback := fn [decode_shadow] [E, K](item E) K {
		return decode_shadow(item)
	}
	return callback(value)
}

fn test_generic_function_callable_parameter_shadows_function() {
	assert apply_shadow[int, string](fn (value int) string {
		return 's${value}'
	}, 1) == 's1'
	assert apply_shadow[string, int](fn (value string) int {
		return value.len
	}, 'hello') == 5
}

fn test_generic_function_local_callable_shadows_function() {
	assert apply_local_shadow[int, string](fn (value int) string {
		return 'local:${value}'
	}, 2) == 'local:2'
}

fn test_generic_function_captured_callable_shadows_function() {
	assert apply_closure_shadow[int, string](fn (value int) string {
		return 'closure:${value}'
	}, 3) == 'closure:3'
}

fn test_generic_function_unshadowed_call_remains_supported() {
	value := 4
	assert decode_shadow(&value) == 4
}

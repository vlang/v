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

fn apply_block_shadow[T](value T, callback fn (T) T, use_callback bool) T {
	mut result := value
	if use_callback {
		decode_shadow := callback
		result = decode_shadow(result)
	}
	return decode_shadow(&result)
}

fn apply_nested_shadow[T](value T, callback fn (T) T) T {
	return apply_block_shadow(value, callback, true)
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

fn test_generic_function_block_shadow_ends_before_nested_generic_call() {
	assert apply_nested_shadow(4, fn (value int) int {
		return value + 1
	}) == 5
	assert apply_nested_shadow('hello', fn (value string) string {
		return value.to_upper()
	}) == 'HELLO'
	assert apply_block_shadow(4, fn (value int) int {
		return value + 1
	}, false) == 4
}

fn test_generic_function_fixed_array_local_callable_shadows_function() {
	assert apply_local_shadow[[2]int, string](fn (values [2]int) string {
		return '${values[0]}:${values[1]}'
	}, [4, 5]!) == '4:5'
}

fn test_generic_function_fixed_array_block_shadow_ends_before_nested_generic_call() {
	assert apply_nested_shadow([4, 5]!, fn (values [2]int) [2]int {
		return [values[0] + 1, values[1] + 2]!
	}) == [5, 7]!
}

fn call_key_is_generic<T>(v T) {
	a := map[T]u8{}
	_ := a.keys().filter(it == v)
}

fn call_value_is_generic<T>(v T) {
	a := map[u8]T{}
	_ := a.values().filter(it == v)
}

fn call_all_is_generic_keys_method<T, U>(v T) {
	a := map[T]U{}
	_ := a.keys().filter(it == v)
}

fn call_all_is_generic_values_method<T, U>(v U) {
	a := map[T]U{}
	_ := a.values().filter(it == v)
}

fn test_call_has_generic() {
	call_key_is_generic<int>(1)
	call_value_is_generic<string>('')
	call_all_is_generic_keys_method<int, string>(1)
	call_all_is_generic_values_method<int, string>('')
	assert true
}

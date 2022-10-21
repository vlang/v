fn call_key_is_generic<T>(v T) {
	a := map[T]u8{}
	_ := a.keys().filter(it == v)
}

fn call_value_is_generic<T>(v T) {
	a := map[u8]T{}
	_ := a.values().filter(it == v)
}

fn test_call_has_generic() {
	call_key_is_generic<int>(1)
	call_value_is_generic<string>('')
	assert true
}

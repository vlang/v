fn test_local_join_path_function_value_is_not_lowered() {
	join_path := fn (a string, b string) string {
		return a + ':' + b
	}
	assert join_path('a', 'b') == 'a:b'
}

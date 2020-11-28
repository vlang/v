fn get<T>(typ T) T {
   return typ
}

fn get_string(typ string) string {
   return 'boom'
}

fn test_generic_with_same_type() {
	assert get_string('') == 'boom'
	assert get<string>('hello') == 'hello'
}

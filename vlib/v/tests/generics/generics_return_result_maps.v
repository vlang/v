fn decode_map[K, V](a map[K]V) !map[K]V {
	return a
}

fn decode[T]() !T {
	return decode_map(T{})!
}

fn test_main() {
	x_str_str := decode[map[string]string]()!
	x_str_int := decode[map[string]int]()!

	assert typeof(x_str_str).name == 'map[string]string'
	assert typeof(x_str_int).name == 'map[string]int'
}

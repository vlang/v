type PathKey = string

fn (path PathKey) up() string {
	return string(path).to_upper()
}

fn alias_unsafe_value[T](path PathKey) string {
	value := unsafe { path }
	return value.up()
}

fn alias_unsafe_array[T]() string {
	mut paths := unsafe { []PathKey{} }
	paths << PathKey('b')
	return paths[0].up()
}

fn alias_map_key[T](paths &map[PathKey]int) string {
	mut result := ''
	for key, _ in *paths {
		result += key.up()
	}
	return result
}

fn alias_map_value[T](paths &map[int]PathKey) string {
	mut result := ''
	for _, value in *paths {
		result += value.up()
	}
	return result
}

fn test_generic_alias_methods_after_unsafe_expressions() {
	assert alias_unsafe_value[int](PathKey('a')) == 'A'
	assert alias_unsafe_value[string](PathKey('hello')) == 'HELLO'
	assert alias_unsafe_array[int]() == 'B'
	assert alias_unsafe_array[string]() == 'B'
}

fn test_generic_alias_methods_on_map_iteration_bindings() {
	keys := &map[PathKey]int{
		PathKey('c'): 1
	}
	values := &map[int]PathKey{
		1: PathKey('d')
	}
	assert alias_map_key[int](keys) == 'C'
	assert alias_map_key[string](keys) == 'C'
	assert alias_map_value[int](values) == 'D'
	assert alias_map_key[int](&map[PathKey]int{}) == ''
}

fn alias_optional_value[T](path PathKey) ?PathKey {
	return path
}

fn alias_result_value[T](path PathKey) !PathKey {
	return path
}

fn unsafe_plain_string[T](text string) string {
	value := unsafe { text }
	return value.to_upper()
}

fn alias_array_range_length[T](paths []PathKey) int {
	return paths[0..1].len
}

fn test_generic_alias_recovery_keeps_unwrapping_and_range_types() {
	optional := alias_optional_value[int](PathKey('e')) or { panic('unexpected none') }
	result := alias_result_value[int](PathKey('f')) or { panic(err) }
	assert optional.up() == 'E'
	assert result.up() == 'F'
	assert unsafe_plain_string[int]('plain') == 'PLAIN'
	assert alias_array_range_length[int]([PathKey('a'), PathKey('b')]) == 1
}

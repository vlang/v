fn test_array_map_callback_uses_its_own_it_scope() {
	indices := [1, 2]
	deltas := [-1, 0, 1]
	neighbours := indices.map(fn [deltas] (i int) []int {
		return deltas.map(i + it)
	})
	assert neighbours == [[0, 1, 2], [1, 2, 3]]
	assert indices.map(fn (it int) int { return it * 2 }) == [2, 4]
	assert indices.map((fn (it int) int { return it + 3 })) == [4, 5]
}

struct MapperHolder {
	callback fn (int) int
}

fn double_value(value int) int {
	return value * 2
}

fn test_array_map_still_returns_function_fields_from_dsl() {
	holders := [MapperHolder{ callback: double_value }]
	callbacks := holders.map(it.callback)
	assert callbacks[0](21) == 42
}

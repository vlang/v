struct Point {
	x f64
	y f64
}

fn generic_map[T](items map[string]T) []T {
	return items.values()
}

fn generic_map_with_constraint[T](items map[string]T) []T {
	return items.values()
}

fn generic_map_keys[T](items map[string]T) []string {
	return items.keys()
}

fn map_any[T](items []T, cb fn (item T) bool) bool {
	for item in items {
		if cb(item) {
			return true
		}
	}
	return false
}

fn test_values_method() {
	// testing map[int]string
	items_1 := {
		1: 'item_1'
		2: 'item_2'
		3: 'item_3'
	}
	assert items_1.values().len == 3
	for i, item in items_1.values() {
		assert item == 'item_${i + 1}'
	}

	// testing map[string]int
	items_2 := {
		'item_1': 1
		'item_2': 2
		'item_3': 3
		'item_4': 4
	}
	assert items_2.values().len == 4
	for i, item in items_2.values() {
		assert item == i + 1
	}

	// testing generics
	items_3 := {
		'a': 10
		'b': 20
		'c': 30
	}
	generic_values := generic_map(items_3)
	assert generic_values.len == 3
	assert generic_values.contains(10)
	assert generic_values.contains(20)
	assert generic_values.contains(30)

	// testing empty map
	empty_map := map[string]int{}
	empty_values := empty_map.values()
	assert empty_values.len == 0

	// testing map with complex types (struct)
	points := {
		'origin': Point{0.0, 0.0}
		'unit_x': Point{1.0, 0.0}
		'unit_y': Point{0.0, 1.0}
	}
	point_values := points.values()
	assert point_values.len == 3
	assert map_any(point_values, fn (point Point) bool {
		return point.x == 0.0 && point.y == 0.0
	})
	assert map_any(point_values, fn (point Point) bool {
		return point.x == 1.0 && point.y == 0.0
	})
	assert map_any(point_values, fn (point Point) bool {
		return point.x == 0.0 && point.y == 1.0
	})
}

fn test_values_method_with_generic_constraints() {
	// test with string constraint
	string_map := {
		'first':  'hello'
		'second': 'world'
	}
	string_result := generic_map_with_constraint(string_map)
	assert string_result.len == 2
	assert string_result.contains('hello')
	assert string_result.contains('world')

	// test with int constraint
	int_map := {
		'one': 1
		'two': 2
	}
	int_result := generic_map_with_constraint(int_map)
	assert int_result.len == 2
	assert int_result.contains(1)
	assert int_result.contains(2)
}

fn test_keys_method() {
	// testing map[int]string keys
	items_1 := {
		1: 'item_1'
		2: 'item_2'
		3: 'item_3'
	}
	for key in items_1.keys() {
		assert 'item_${key}' == items_1[key]
	}

	// testing map[string]int keys
	items_2 := {
		'item_1': 1
		'item_2': 2
		'item_3': 3
		'item_4': 4
	}
	keys_2 := items_2.keys()
	assert keys_2.len == 4
	assert keys_2.contains('item_1')
	assert keys_2.contains('item_2')
	assert keys_2.contains('item_3')
	assert keys_2.contains('item_4')

	// testing empty map keys
	empty_map := map[string]int{}
	empty_keys := empty_map.keys()
	assert empty_keys.len == 0

	// testing map with single element keys
	single_item := {
		'only': 42
	}
	single_keys := single_item.keys()
	assert single_keys.len == 1
	assert single_keys[0] == 'only'

	// testing map with complex types as values but simple keys
	points := {
		'origin': Point{0.0, 0.0}
		'unit_x': Point{1.0, 0.0}
		'unit_y': Point{0.0, 1.0}
	}
	point_keys := points.keys()
	assert point_keys.len == 3
	assert point_keys.contains('origin')
	assert point_keys.contains('unit_x')
	assert point_keys.contains('unit_y')
}

fn test_keys_method_with_generic_constraints() {
	// test with string values
	string_map := {
		'first':  'hello'
		'second': 'world'
		'third':  'test'
	}
	string_keys := generic_map_keys(string_map)
	assert string_keys.len == 3
	assert string_keys.contains('first')
	assert string_keys.contains('second')
	assert string_keys.contains('third')

	// test with struct values
	point_map := {
		'origin': Point{0.0, 0.0}
		'center': Point{5.0, 5.0}
	}
	point_keys := generic_map_keys(point_map)
	assert point_keys.len == 2
	assert point_keys.contains('origin')
	assert point_keys.contains('center')

	// test with empty map
	empty_map := map[string]int{}
	empty_keys := generic_map_keys(empty_map)
	assert empty_keys.len == 0
}

fn test_direct_map_access() {
	// testing map[int]string
	items_1 := {
		1: 'one'
		2: 'two'
		3: 'three'
	}
	assert items_1[1] == 'one'
	assert items_1[2] == 'two'
	assert items_1[3] == 'three'

	// testing map[string]int
	items_2 := {
		'one':   1
		'two':   2
		'three': 3
	}
	assert items_2['one'] == 1
	assert items_2['two'] == 2
	assert items_2['three'] == 3
}

fn test_map_len() {
	// testing on the fly maps
	items_1 := {
		'one': 1
		'two': 2
	}
	assert items_1.len == 2

	// testing empty map length
	mut items_2 := map[string]int{}
	assert items_2.len == 0

	// testing dynamic addition map length
	items_2['one'] = 1
	items_2['two'] = 2
	assert items_2.len == 2
}

fn test_rune_keys() {
	mut m := {
		`!`: 2
		`%`: 3
	}
	assert typeof(m).name == 'map[rune]int'
	assert m[`!`] == 2
	m[`@`] = 7
	assert m.len == 3
	println(m)
	assert '${m}' == '{`!`: 2, `%`: 3, `@`: 7}'
}

fn main() {
	test_values_method()
	test_values_method_with_generic_constraints()
	test_keys_method()
	test_keys_method_with_generic_constraints()
	test_direct_map_access()
	test_map_len()
	test_rune_keys()
}

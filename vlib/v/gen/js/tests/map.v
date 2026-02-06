type T0 = int | string
type T1 = T0 | rune

struct Point {
	x f64
	y f64
}

enum Colors {
	red = 1
	green
	blue
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

fn test_map_with_different_key_types() {
	// map[int]string
	mut items_1 := {
		1: 'one'
		2: 'two'
		3: 'three'
	}
	assert typeof(items_1).name == 'map[int]string'
	assert items_1[2] == 'two'
	items_1[4] = 'four'
	assert items_1[4].len == 4
	keys_1 := items_1.keys()
	assert keys_1.contains(1)
	assert keys_1.contains(2)
	assert keys_1.contains(3)
	assert keys_1.contains(4)
	assert '${items_1}' == "{1: 'one', 2: 'two', 3: 'three', 4: 'four'}"

	// map[string]int
	mut items_2 := {
		'one':   1
		'two':   2
		'three': 3
	}
	assert typeof(items_2).name == 'map[string]int'
	assert items_2['two'] == 2
	items_2['four'] = 4
	assert items_2['four'] == 4
	keys_2 := items_2.keys()
	assert keys_2.contains('one')
	assert keys_2.contains('two')
	assert keys_2.contains('three')
	assert keys_2.contains('four')
	assert '${items_2}' == "{'one': 1, 'two': 2, 'three': 3, 'four': 4}"

	// map[f64]string
	mut items_3 := {
		1.1: 'one dot one'
		2.2: 'two dot two'
		3.3: 'three dot three'
	}
	assert typeof(items_3).name == 'map[f64]string'
	assert items_3[2.2] == 'two dot two'
	items_3[4.4] = 'four dot four'
	assert items_3[4.4].len == 13
	keys_3 := items_3.keys()
	assert keys_3.contains(1.1)
	assert keys_3.contains(2.2)
	assert keys_3.contains(3.3)
	assert keys_3.contains(4.4)
	assert '${items_3}' == "{1.1: 'one dot one', 2.2: 'two dot two', 3.3: 'three dot three', 4.4: 'four dot four'}"

	// map[u8]string
	mut items_4 := {
		u8(1): 'one'
		2:     'two'
	}
	assert typeof(items_4).name == 'map[u8]string'
	assert items_4[2] == 'two'
	items_4[3] = 'three'
	assert items_4[3].len == 5
	keys_4 := items_4.keys()
	assert keys_4.contains(1)
	assert keys_4.contains(2)
	assert keys_4.contains(3)
	assert '${items_4}' == "{49: 'one', 50: 'two', 51: 'three'}"

	// map[rune]int
	mut items_5 := {
		`!`: 2
		`%`: 3
	}
	assert typeof(items_5).name == 'map[rune]int'
	assert items_5[`!`] == 2
	items_5[`@`] = 7
	assert items_5.len == 3
	keys_5 := items_5.keys()
	assert keys_5.contains(`!`)
	assert keys_5.contains(`%`)
	assert keys_5.contains(`@`)
	assert '${items_5}' == '{`!`: 2, `%`: 3, `@`: 7}'

	// map[sum-type]string
	mut items_6 := {
		T1(T0(1)): 'one'
		T0('2'):   'two'
	}
	items_6[`!`] = 'exclamation'
	assert items_6[`!`].len == 11
	keys_6 := items_6.keys()
	assert keys_6.contains(T0(1))
	assert keys_6.contains(T0('2'))
	assert keys_6.contains(`!`)

	// map[enum-type]string
	mut items_7 := {
		Colors.red: 'red'
		.green:     'green'
	}
	items_7[.blue] = 'blue'
	assert items_7[.blue].len == 4
	keys_7 := items_7.keys()
	keys_7.contains(.red)
	keys_7.contains(.green)
	keys_7.contains(.blue)
}

fn main() {
	test_values_method()
	test_values_method_with_generic_constraints()
	test_keys_method()
	test_keys_method_with_generic_constraints()
	test_direct_map_access()
	test_map_len()
	test_map_with_different_key_types()
}

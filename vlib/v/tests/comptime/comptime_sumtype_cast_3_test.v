pub type Any = bool | int | map[string]Any | string

struct StructType[T] {
mut:
	val T
}

fn map_from[T](t T) map[string]Any {
	mut m := map[string]Any{}
	$if T is $struct {
		$for field in T.fields {
			key := field.name
			m[key] = t.$(field.name)
		}
	}
	return m
}

fn test_struct_to_map_string() {
	array_of_struct := [StructType[string]{
		val: 'true'
	}, StructType[string]{
		val: 'false'
	}]

	mut array_of_map := []Any{}

	for variable in array_of_struct {
		array_of_map << map_from(variable)
	}

	println(array_of_map)
	assert array_of_map[0] == Any({
		'val': Any('true')
	})
	assert array_of_map[1] == Any({
		'val': Any('false')
	})
}

fn test_struct_to_map_bool() {
	array_of_struct := [StructType[bool]{
		val: true
	}, StructType[bool]{
		val: false
	}]

	mut array_of_map := []Any{}

	for variable in array_of_struct {
		array_of_map << map_from(variable)
	}

	println(array_of_map)
	assert array_of_map[0] == Any({
		'val': Any(true)
	})
	assert array_of_map[1] == Any({
		'val': Any(false)
	})
}

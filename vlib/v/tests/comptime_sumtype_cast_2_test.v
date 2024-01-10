pub type Any = bool | int | map[string]Any | string

struct StructType[T] {
mut:
	val T
}

fn map_from[T](t T) Any {
	mut value := Any{}
	$if T is $struct {
		$for field in T.fields {
			println(t.$(field.name))
			value = t.$(field.name)
			value = Any(t.$(field.name))
		}
	}
	return value
}

fn test_generic_struct_with_sumtype() {
	struct_type := StructType[string]{
		val: 'true'
	}
	struct_type2 := StructType[int]{
		val: 1
	}
	array_of_struct := [struct_type, struct_type]

	for variable in array_of_struct {
		assert map_from(variable) == Any('true')
	}

	array_of_struct2 := [struct_type2, struct_type2]

	for variable in array_of_struct2 {
		assert map_from(variable) == Any(1)
	}
}

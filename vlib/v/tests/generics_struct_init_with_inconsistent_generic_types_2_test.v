struct Tuple[A, B] {
	a A
	b B
}

// map to array of key-value tuples
fn map_to_array[K, V](m map[K]V) []Tuple[K, V] {
	mut r := []Tuple[K, V]{cap: m.len}
	for k, v in m {
		r << Tuple[K, V]{k, v}
	}
	return r
}

fn test_generics_struct_init_with_inconsistent_generic_types() {
	x := {
		'one': 1
		'two': 2
	}
	println(x)
	arr := map_to_array(x)
	println(arr)
	assert arr.len == 2
	assert arr[0].a == 'one'
	assert arr[0].b == 1
	assert arr[1].a == 'two'
	assert arr[1].b == 2
}

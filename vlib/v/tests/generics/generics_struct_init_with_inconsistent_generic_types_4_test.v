struct Tuple1[A] {
	a A
}

struct Tuple2[A, B] {
	a A
	b B
}

struct Tuple3[A, B, X] { // note: "C" is reserved for C language...
	a A
	b B
	c X
}

// map to array of key tuples
fn map_to_array1_k[K, V](m map[K]V) []Tuple1[K] {
	mut r := []Tuple1[K]{cap: m.len}
	for k, _ in m {
		r << Tuple1[K]{k}
	}
	return r
}

// map to array of value tuples
fn map_to_array1_v[K, V](m map[K]V) []Tuple1[V] {
	mut r := []Tuple1[V]{cap: m.len}
	for _, v in m {
		r << Tuple1[V]{v}
	}
	return r
}

// map to array of key-value tuples
fn map_to_array2_k_v[K, V](m map[K]V) []Tuple2[K, V] {
	mut r := []Tuple2[K, V]{cap: m.len}
	for k, v in m {
		r << Tuple2[K, V]{k, v}
	}
	return r
}

// map to array of value-key tuples
fn map_to_array2_v_k[K, V](m map[K]V) []Tuple2[V, K] {
	mut r := []Tuple2[V, K]{cap: m.len}
	for k, v in m {
		r << Tuple2[V, K]{v, k}
	}
	return r
}

// map to array of key-int tuples
fn map_to_array2_k_int[K, V](m map[K]V) []Tuple2[K, int] {
	mut r := []Tuple2[K, int]{cap: m.len}
	mut i := 0
	for k, _ in m {
		r << Tuple2[K, int]{k, i}
		i += 1
	}
	return r
}

// map to array of value-int-key tuples
fn map_to_array3_v_int_k[K, V](m map[K]V) []Tuple3[V, int, K] {
	mut r := []Tuple3[V, int, K]{cap: m.len}
	mut i := 0
	for k, v in m {
		r << Tuple3[V, int, K]{v, i, k}
		i += 1
	}
	return r
}

fn test_generics_struct_init_with_inconsistent_generic_types() {
	x := {
		'one': 1
		'two': 2
	}
	y := {
		3: 'three'
		4: 'four'
	}

	println(x)
	rx1 := map_to_array1_k(x)
	println(rx1)
	assert rx1[0].a == 'one'
	assert rx1[1].a == 'two'

	rx2 := map_to_array1_v(x)
	println(rx2)
	assert rx2[0].a == 1
	assert rx2[1].a == 2

	rx3 := map_to_array2_k_v(x)
	println(rx3)
	assert rx3[0].a == 'one'
	assert rx3[0].b == 1
	assert rx3[1].a == 'two'
	assert rx3[1].b == 2

	rx4 := map_to_array2_v_k(x)
	println(rx4)
	assert rx4[0].a == 1
	assert rx4[0].b == 'one'
	assert rx4[1].a == 2
	assert rx4[1].b == 'two'

	rx5 := map_to_array2_k_int(x)
	println(rx5)
	assert rx5[0].a == 'one'
	assert rx5[0].b == 0
	assert rx5[1].a == 'two'
	assert rx5[1].b == 1

	rx6 := map_to_array3_v_int_k(x)
	println(rx6)
	assert rx6[0].a == 1
	assert rx6[0].b == 0
	assert rx6[0].c == 'one'
	assert rx6[1].a == 2
	assert rx6[1].b == 1
	assert rx6[1].c == 'two'

	println(y)
	ry1 := map_to_array1_k(y)
	println(ry1)
	assert ry1[0].a == 3
	assert ry1[1].a == 4

	ry2 := map_to_array1_v(y)
	println(ry2)
	assert ry2[0].a == 'three'
	assert ry2[1].a == 'four'

	ry3 := map_to_array2_k_v(y)
	println(ry3)
	assert ry3[0].a == 3
	assert ry3[0].b == 'three'
	assert ry3[1].a == 4
	assert ry3[1].b == 'four'

	ry4 := map_to_array2_v_k(y)
	println(ry4)
	assert ry4[0].a == 'three'
	assert ry4[0].b == 3
	assert ry4[1].a == 'four'
	assert ry4[1].b == 4

	ry5 := map_to_array2_k_int(y)
	println(ry5)
	assert ry5[0].a == 3
	assert ry5[0].b == 0
	assert ry5[1].a == 4
	assert ry5[1].b == 1

	ry6 := map_to_array3_v_int_k(y)
	println(ry6)
	assert ry6[0].a == 'three'
	assert ry6[0].b == 0
	assert ry6[0].c == 3
	assert ry6[1].a == 'four'
	assert ry6[1].b == 1
	assert ry6[1].c == 4
}

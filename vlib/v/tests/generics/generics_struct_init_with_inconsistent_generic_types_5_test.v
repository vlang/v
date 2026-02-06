struct Tuple2[A, B] {
	a A
	b B
}

// map to array of Tuple2[int, Tuple2[key, value]] tuples
fn map_to_array_int_kv[K, V](m map[K]V) []Tuple2[int, Tuple2[K, V]] {
	mut r := []Tuple2[int, Tuple2[K, V]]{cap: m.len}
	mut i := 0
	for k, v in m {
		r << Tuple2[int, Tuple2[K, V]]{i, Tuple2[K, V]{k, v}}
		i += 1
	}
	return r
}

// map to array of Tuple2[int, Tuple2[Tuple2[key, value], Tuple2[value, key]]] tuples
fn map_to_array_int_kv_vk[K, V](m map[K]V) []Tuple2[int, Tuple2[Tuple2[K, V], Tuple2[V, K]]] {
	mut r := []Tuple2[int, Tuple2[Tuple2[K, V], Tuple2[V, K]]]{cap: m.len}
	mut i := 0
	for k, v in m {
		r << Tuple2[int, Tuple2[Tuple2[K, V], Tuple2[V, K]]]{i, Tuple2[Tuple2[K, V], Tuple2[V, K]]{Tuple2[K, V]{k, v}, Tuple2[V, K]{v, k}}}
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
	rx1 := map_to_array_int_kv(x)
	println(rx1)
	assert rx1[0].a == 0
	assert rx1[0].b.a == 'one'
	assert rx1[0].b.b == 1
	assert rx1[1].a == 1
	assert rx1[1].b.a == 'two'
	assert rx1[1].b.b == 2

	rx2 := map_to_array_int_kv_vk(x)
	println(rx2)
	assert rx2[0].a == 0
	assert rx2[0].b.a.a == 'one'
	assert rx2[0].b.a.b == 1
	assert rx2[0].b.b.a == 1
	assert rx2[0].b.b.b == 'one'
	assert rx2[1].a == 1
	assert rx2[1].b.a.a == 'two'
	assert rx2[1].b.a.b == 2
	assert rx2[1].b.b.a == 2
	assert rx2[1].b.b.b == 'two'

	println(y)
	ry1 := map_to_array_int_kv(y)
	println(ry1)
	assert ry1[0].a == 0
	assert ry1[0].b.a == 3
	assert ry1[0].b.b == 'three'
	assert ry1[1].a == 1
	assert ry1[1].b.a == 4
	assert ry1[1].b.b == 'four'

	ry2 := map_to_array_int_kv_vk(y)
	println(ry2)
	assert ry2[0].a == 0
	assert ry2[0].b.a.a == 3
	assert ry2[0].b.a.b == 'three'
	assert ry2[0].b.b.a == 'three'
	assert ry2[0].b.b.b == 3
	assert ry2[1].a == 1
	assert ry2[1].b.a.a == 4
	assert ry2[1].b.a.b == 'four'
	assert ry2[1].b.b.a == 'four'
	assert ry2[1].b.b.b == 4

	zx1 := []Tuple2[int, Tuple2[string, int]]{}
	println(typeof(zx1).name)
	println(typeof(rx1).name)
	zx2 := []Tuple2[int, Tuple2[Tuple2[string, int], Tuple2[int, string]]]{}
	println(typeof(zx2).name)
	println(typeof(rx2).name)
	assert typeof(zx1).name == '[]Tuple2[int, Tuple2[string, int]]'
	assert typeof(zx2).name == '[]Tuple2[int, Tuple2[Tuple2[string, int], Tuple2[int, string]]]'
	assert typeof(zx1).name == typeof(rx1).name
	assert typeof(zx2).name == typeof(rx2).name
}

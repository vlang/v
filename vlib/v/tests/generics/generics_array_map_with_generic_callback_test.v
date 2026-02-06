fn test_generics_array_map_with_generic_callback() {
	mut a := []Sth{}
	a = arr_generic[Sth](buggy_cb)
	println(a)
	assert a.len == 0
}

fn arr_generic[T](cb fn (int) T) []T {
	return []int{}.map(cb(it))
}

fn buggy_cb(a int) Sth {
	return Sth{
		a: a
	}
}

struct Sth {
	a int
	b string
}

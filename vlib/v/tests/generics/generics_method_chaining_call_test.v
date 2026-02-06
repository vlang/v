struct Seq[T] {
	ar []T
}

type MapFn[T, K] = fn (T) K

fn (s Seq[T]) map[K](map_fn MapFn[T, K]) Seq[K] {
	mut map_ar := []K{cap: s.ar.len}

	for _, i in s.ar {
		map_ar << map_fn[T, K](i)
	}

	return Seq[K]{map_ar}
}

fn test_generics_method_chaining_call() {
	s := Seq[string]{['one', 'two']}
		.map[int](fn (element string) int {
			match element {
				'one' { return 1 }
				'two' { return 2 }
				else { return -1 }
			}
		})
		.map[int](fn (element int) int {
			return element + 2
		})

	println(s)
	assert s.ar == [3, 4]
}

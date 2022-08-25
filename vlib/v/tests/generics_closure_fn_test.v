fn setter<T>(mut m map[T]int) fn (T, int) {
	return fn [mut m] <T>(x T, k int) {
		m[x] = k
	}
}

fn test_generics_closure_fn() {
	mut m := {
		f32(0.1): 1
	}

	f := setter(mut m)
	f(0.2, 2)

	println(m)
	assert m == {
		f32(0.1): 1
		0.2:      2
	}
}

fn abc[T](x T) fn (p T) T {
	return fn [x] [T](p T) T {
		return p * x
	}
}

fn test_generic_closures_with_different_generic_types() {
	f := abc[int](12345)
	a := f(2)
	dump(a)
	assert a == 24690

	g := abc[u8](5)
	b := g(2)
	dump(b)
	assert b == 10
}
